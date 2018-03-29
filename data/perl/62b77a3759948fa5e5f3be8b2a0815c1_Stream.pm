package Rum::Loop::Stream;
use strict;
use warnings;
use Rum::Loop::TCP ();
use Rum::Loop::Queue;
use Rum::Loop::Utils 'assert';
use Rum::Loop::SendRecv;
use Rum::Loop::Flags qw(:Stream :IO :Errors :Platform $CLOSING $CLOSED $KQUEUE);
use IO::Handle;
use Socket;
use POSIX ();
use Data::Dumper;
use POSIX ':errno_h';
use base qw/Exporter/;
our @EXPORT = qw (
    stream_init
    stream_open
    stream_close
    stream_destroy
    read_start
    read_stop
    try_write
    write
    write2
    shutdown
    accept
    is_readable
    is_writable
);

##FIXME this may mess signals module?!
$SIG{PIPE} = 'IGNORE';

my $UNKNOWN_HANDLE = 0;

sub stream_init {
    my ($loop,$stream,$type) = @_;
    
    $loop->handle_init($_[1],$type);
    $_[1]->{read_cb}  = undef;
    $_[1]->{read2_cb} = undef;
    $_[1]->{alloc_cb} = undef;
    $_[1]->{close_cb} = undef;
    $_[1]->{connection_cb} = undef;
    $_[1]->{connect_req}   = undef;
    $_[1]->{shutdown_req}  = undef;
    $_[1]->{queued_fds} = [];
    $_[1]->{accepted_fd}   = -1;
    $_[1]->{delayed_error} = 0;
    $_[1]->{write_queue}   = QUEUE_INIT($_[1]);
    $_[1]->{write_completed_queue} = QUEUE_INIT($_[1]);
    $_[1]->{write_queue_size} = 0;
    $_[1]->{io_watcher} = $loop->io_init(0, \&stream_io);
    $_[1]->{io_watcher}->{stream} = $_[1];
    return 1;
}

# Implements a best effort approach to mitigating accept() EMFILE errors.
# We have a spare file descriptor stashed away that we close to get below
# the EMFILE limit. Next, we accept all pending connections and close them
# immediately to signal the clients that we're overloaded - and we are, but
# we still keep on trucking.

# There is one caveat: it's not reliable in a multi-threaded environment.
# The file descriptor limit is per process. Our party trick fails if another
# thread opens a file or creates a socket in the time window between us
# calling close() and accept().
sub _emfile_trick {
    my ($loop, $accept_fd, $accept_fh) = @_;
    my $fd;
    
    $! = 0;
    if ($loop->{emfile_fd} == -1){
        $! = EMFILE;
        return $!;
    }
    
    _close($loop->{emfile_fd});
    
    for (;;) {
        $fd = _accept($accept_fd,$accept_fh);
        if ($fd) {
            _close($fd);
            next;
        }
        
        if ($! == EINTR) {
            next;
        }
        
        return $!;
    }
}

sub server_io {
    my ($loop, $w, $events) = @_;
    my $err;
    my $stream = $w->{stream};
    assert( $events & $POLLACCEPT );
    assert(!($stream->{flags} & $CLOSING));
    
    $loop->io_start($stream->{io_watcher},  $POLLACCEPT );
    
    # connection_cb can close the server socket while we're
    # in the loop so check it on each iteration.
    while ( $stream->{io_watcher}->{fd} != -1 ) {
        assert(!$stream->{accepted_fh});
        $err = Rum::Loop::Core::_accept( $stream->{io_watcher}->{fh} );
        if (!$err) {
            return if ($! == EAGAIN || $! == EWOULDBLOCK || $! == EBADF); #not an error
            next if ($! == ECONNABORTED); #Ignore. Nothing we can do about that
            
            if ($! == EMFILE || $! == ENFILE) {
                
                #TODO
                $err = _emfile_trick($loop, $stream->{fh});
                
                if ($err == EAGAIN || $err == EWOULDBLOCK) {
                    last;
                }
            }
            
            $stream->{connection_cb}->($stream, $err ? $err : $!);
            next;
        }
        
        $stream->{accepted_fd} = fileno $err || -1;
        $stream->{accepted_fh} = $err;
        $stream->{connection_cb}->($stream, 0);
        
        if ( $stream->{accepted_fh} ) {
            # The user hasn't yet accepted called uv_accept() */
            $loop->io_stop($w, $POLLACCEPT);
            return 1;
        }
        
        #if ($stream->{type} eq 'TCP' && ($stream->{flags} & $SINGLE_ACCEPT)) {
        #    #Give other processes a chance to accept connections. */
        #    select undef,undef,undef,.1;
        #}
    }
    return 1;
}

sub write {
    my ($loop,$req, $handle, $bufs, $nbufs, $cb) = @_;
    return write2($loop,$req, $handle, $bufs, $nbufs, undef, $cb);
}

sub write2 {
    my ($loop,
        $req,
        $stream,
        $bufs,
        $nbufs,
        $send_handle,
        $cb) = @_;
    
    if (ref $bufs eq 'HASH'){
        $bufs = [$bufs];
    }
    
    assert($nbufs > 0);
    assert(($stream->{type} eq 'TCP' ||
            $stream->{type} eq 'NAMED_PIPE' ||
            $stream->{type} eq 'TTY'),
            "uv_write (unix) does not yet support other types of streams");
    
    if (stream_fd($stream) < 0){
        $! = EBADF;
        return;
    }
    
    if ($send_handle) {
        if ( $stream->{type} ne 'NAMED_PIPE' || !$stream->{ipc} ){
            $! = EINVAL;
            return;
        }
        
        #XXX We abuse uv_write2() to send over UDP handles to child processes.
        #Don't call uv__stream_fd() on those handles, it's a macro that on OS X
        #evaluates to a function that operates on a uv_stream_t with a couple of
        #OS X specific fields. On other Unices it does (handle)->io_watcher.fd,
        #which works but only by accident.
        if (handle_fd($send_handle) < 0){
            $! = EBADF;
            return;
        }
    }
    
    #It's legal for write_queue_size > 0 even when the write_queue is empty;
    #it means there are error-state requests in the write_completed_queue that
    #will touch up write_queue_size later, see also uv__write_req_finish().
    #We chould check that write_queue is empty instead but that implies making
    #a write() syscall when we know that the handle is in error mode.
    
    my $empty_queue = ($stream->{write_queue_size} == 0);
    
    #Initialize the req
    $loop->req_init($req, 'WRITE');
    $req->{cb} = $cb;
    $req->{handle} = $stream;
    $req->{error} = 0;
    $req->{off} = 0;
    $req->{send_handle} = $send_handle;
    $req->{queue} = QUEUE_INIT($req);
    $req->{bufs} = $req->{bufsml} = $bufs;
    if (!$req->{bufs}){
        $! = ENOMEM;
        return;
    }
    
    $req->{nbufs} = $nbufs;
    $req->{write_index} = 0;
    $stream->{write_queue_size} += uv_count_bufs($bufs, $nbufs);
    
    #Append the request to write_queue.
    QUEUE_INSERT_TAIL($stream->{write_queue}, $req->{queue});
    
    #If the queue was empty when this function began, we should attempt to
    #do the write immediately. Otherwise start the write_watcher and wait
    #for the fd to become writable.
    if ($stream->{connect_req}) {
        # Still connecting, do nothing.
    } elsif ($empty_queue) {
        uv__write($loop,$stream);
    } else {
        #blocking streams should never have anything in the queue.
        #if this assert fires then somehow the blocking stream isn't being
        #sufficiently flushed in uv__write.
        assert(!($stream->{flags} & $STREAM_BLOCKING));
        $loop->io_start($stream->{io_watcher}, $POLLOUT);
    }
    
    return 1;
}

sub _writev {
    my $fh = shift;
    my $data = shift;
    my $index = shift;
    my $bufnum = shift;
    #my $str = join '', @{$data};
    my $str = '';
    for ($index .. @{$data} - 1){
        $str .=  $data->[$_];
        last if length $str >= 160 * 1024;
    }
    
    ##to avoid constructing string over and over again
    ##on failure we loop for errors here too
    my $n = 0;
    do {
        $n = syswrite($fh, $str, length $str);
    } while (!defined $n && $! == EINTR);
    
    return $n;
}

sub uv__write {
    my $loop = shift;
    my $stream = shift;
    my $q;
    my $req;
    my $n;
    my $msghdr;
    
    start : {
        assert(stream_fd($stream) >= 0);
        if (QUEUE_EMPTY($stream->{write_queue})) { return; }
        $q = QUEUE_HEAD($stream->{write_queue});
        $req = $q->{data};
        assert($req->{handle} == $stream);
        my $iov = $req->{bufs};
        my $iovcnt = $req->{nbufs} - $req->{write_index};
        if ($req->{send_handle}) {
            $! = 0;
            my $fd_to_send = handle_fd($req->{send_handle});
            my $fh_to_send = stream_fh($req->{send_handle});
            my $pid = $stream->{ipc_pid};
            do {
                $n = sendmsg(stream_fh($stream), $iov->[0], $fd_to_send, $pid);
            } while (!defined $n && $! == EINTR);
            
        } else {
            do {
                if ($iovcnt == 1) {
                    $n = syswrite(stream_fh($stream), $iov->[$req->{write_index}],
                              length $iov->[$req->{write_index}]);
                } else {
                    $n = _writev(stream_fh($stream), $iov, $req->{write_index}, $iovcnt);
                }
            } while (!defined $n && $! == EINTR);
        }
        
        if (!defined $n) {
            if ($! != EAGAIN && $! != EWOULDBLOCK) {
                #Error
                $req->{error} = $!;
                uv__write_req_finish($loop,$req);
                $loop->io_stop($stream->{io_watcher}, $POLLOUT);
                if (!$loop->io_active($stream->{io_watcher}, $POLLIN)) {
                    $loop->handle_stop($stream);
                }
                return;
            } elsif ($stream->{flags} & $STREAM_BLOCKING) {
                #If this is a blocking stream, try again.
                goto start;
            }
        } else {
            #Successful write
            while ($n >= 0) {
                my $len = length $req->{bufs}->[$req->{write_index}];
                assert($req->{write_index} < $req->{nbufs});
                if ($n < $len) {
                    my $new = substr $req->{bufs}->[$req->{write_index}], $n;
                    undef $req->{bufs}->[$req->{write_index}];
                    $req->{bufs}->[$req->{write_index}] = $new;
                    undef $new;
                    $stream->{write_queue_size} -= $n;
                    $n = 0;
                    #There is more to write.
                    if ($stream->{flags} & $STREAM_BLOCKING) {
                        #If we're blocking then we should not be enabling the write
                        #watcher - instead we need to try again.
                        goto start;
                    } else {
                        #Break loop and ensure the watcher is pending. */
                        last;
                    }
                } else {
                    #undef $req->{bufs}->[$req->{write_index}];
                    #Finished writing the buf at index req->write_index.
                    $req->{off} = 0;
                    $req->{write_index}++;
                    assert($n >= $len);
                    $n -= $len;
                    assert($stream->{write_queue_size} >= $len);
                    $stream->{write_queue_size} -= $len;
                    if ($req->{write_index} == $req->{nbufs}) {
                        #Then we're done!
                        assert($n == 0);
                        uv__write_req_finish($loop,$req);
                        #TODO: start trying to write the next request.
                        return;
                    }
                }
            }
        }
    }
    
    #Either we've counted n down to zero or we've got EAGAIN. */
    assert(!$n);
    
    # Only non-blocking streams should use the write_watcher. */
    assert(!($stream->{flags} & $STREAM_BLOCKING));
    
    #We're not done. */
    $loop->io_start($stream->{io_watcher}, $POLLOUT);
}

sub try_write_cb {
    die "should never be called";
}

sub try_write {
    my ($loop, $stream, $bufs, $nbufs) = @_;
    my $req_size = 0;
    my $req = {};
    
    #Connecting or already writing some data */
    if ($stream->{connect_req} || $stream->{write_queue_size} != 0) { return 1 }
    my $has_pollout = $loop->io_active($stream->{io_watcher}, $POLLOUT);
    my $r = $loop->write($req, $stream, $bufs, $nbufs, \&try_write_cb);
    
    if (!defined $r){ return; }
    
    #Remove not written bytes from write queue size
    my $written = uv_count_bufs($bufs, $nbufs);
    
    if ($req->{bufs}) {
        $req_size = uv__write_req_size($req);
    } else {
        $req_size = 0;
    }
    
    $written -= $req_size;
    $stream->{write_queue_size} -= $req_size;
    
    #Unqueue request, regardless of immediateness
    QUEUE_REMOVE($req->{queue});
    $loop->req_unregister($req);
    undef $req->{bufs};
    # Do not poll for writable, if we wasn't before calling this
    if (!$has_pollout) {
        $loop->io_stop($stream->{io_watcher}, $POLLOUT);
    }
    return $written;
}

sub uv__write_req_finish {
    my $loop = shift;
    my $req = shift;
    my $stream = $req->{handle};

    # Pop the req off tcp->write_queue.
    QUEUE_REMOVE($req->{queue});
    
    # Only free when there was no error. On error, we touch up write_queue_size
    # right before making the callback. The reason we don't do that right away
    # is that a write_queue_size > 0 is our only way to signal to the user that
    # he should stop writing - which he should if we got an error. Something to
    # revisit in future revisions of the libuv API.
    
    if ($req->{error} == 0) {
        undef $req->{bufs};
    }
    
    # Add it to the write_completed_queue where it will have its
    # callback called in the near future.
    $req->{queue}->{data} = $req;
    QUEUE_INSERT_TAIL($stream->{write_completed_queue}, $req->{queue});
    $loop->io_feed($stream->{io_watcher});
}

sub uv__write_callbacks {
    my $loop = shift;
    my $stream = shift;
    while ( !QUEUE_EMPTY($stream->{write_completed_queue}) ) {
        #Pop a req off write_completed_queue.
        my $q = QUEUE_HEAD($stream->{write_completed_queue});
        my $req = $q->{data};
        QUEUE_REMOVE($q);
        $loop->req_unregister($req);
        if ($req->{bufs}) {
            $stream->{write_queue_size} -= uv__write_req_size($req);
            undef $req->{bufs};
        }
        if ($req->{cb}){
            $req->{cb}->($req, $req->{error});
        }
    }
    
    assert(QUEUE_EMPTY($stream->{write_completed_queue}));
    if (QUEUE_EMPTY($stream->{write_queue})){
        uv__drain($loop, $stream);
    }
}

sub uv__drain {
    my $loop = shift;
    my $stream = shift;
    my ($req,$err);
    assert(QUEUE_EMPTY($stream->{write_queue}));
    $loop->io_stop($stream->{io_watcher}, $POLLOUT);
    if (($stream->{flags} & $STREAM_SHUTTING) &&
      !($stream->{flags} & $CLOSING) &&
      !($stream->{flags} & $STREAM_SHUT)) {
        assert($stream->{shutdown_req});
        $req = $stream->{shutdown_req};
        undef $stream->{shutdown_req};
        $stream->{flags} &= ~$STREAM_SHUTTING;
        $loop->req_unregister($req);
        $err = 0;
        if (!shutdown(stream_fh($stream), 1)){
            $err = $!;
        }
        
        if ($err == 0){
            $stream->{flags} |= $STREAM_SHUT;
        }
        
        if ($req->{cb}){
            $req->{cb}->($req,$err);
        }
    }
}

sub uv_count_bufs {
    my $bufs = shift;
    my $nbufs = shift;
    my $index = shift || 0;
    my $bytes = 0;
    
    for (my $i = $index; $i < $nbufs; $i++) {
        $bytes += length $bufs->[$i];
    }
    
    return $bytes;
}

sub uv__write_req_size {
    my $req = shift;
    assert(defined $req->{bufs});
    my $size = uv_count_bufs($req->{bufs},
                       $req->{nbufs} - $req->{write_index}, $req->{write_index});
    assert($req->{handle}->{write_queue_size} >= $size);
    return $size;
}

sub stream_io {
    my ($loop, $w, $events) = @_;
    my $stream = $w->{stream};
    assert($stream->{type} eq 'TCP' ||
            $stream->{type} eq 'NAMED_PIPE' ||
            $stream->{type} eq 'TTY');
    
    assert(!($stream->{flags} & $CLOSING));
    if ($stream->{connect_req}) {
        _stream_connect($loop,$stream);
        return;
    }
    
    assert(stream_fd($stream) >= 0);
    #Ignore POLLHUP here. Even it it's set, there may still be data to read.
    if ($events & ($POLLIN | $POLLERR)) {
        _read($loop,$stream);
    }
    
    if (stream_fd($stream) == -1) {
        return;  # read_cb closed stream.
    }
    
    # Short-circuit if POLLHUP is set, the user is still interested in read
    # events and uv__read() reported a partial read but not EOF. If the EOF
    # flag is set, uv__read() called read_cb with err=UV_EOF and we don't
    # have to do anything. If the partial read flag is not set, we can't
    # report the EOF yet because there is still data to read.
    if (($events & $POLLHUP) &&
        ($stream->{flags} & $STREAM_READING) &&
        ($stream->{flags} & $STREAM_READ_PARTIAL) &&
        !($stream->{flags} & $STREAM_READ_EOF)) {
        
        my $buf = {
            base => undef,
            len => 0
        };
        __stream_eof($loop, $stream, $buf);
    }
    
    if (stream_fd($stream) == -1) {
        return;  #read_cb closed stream.
    }
    
    if ($events & ($POLLOUT | $POLLERR | $POLLHUP)) {
        uv__write($loop,$stream);
        uv__write_callbacks($loop, $stream);
    }
}

sub _close {
    return Rum::Loop::Core::__close($_[0]);
}

sub accept {
    my ($loop, $server, $client) = @_;
    $! = 0;
    my $err = 0;
    if ($server->{accepted_fd} == -1) {
        $! = $EAGAIN;
        return;
    }
    
    my $type = $client->{type};
    if ($type eq 'NAMED_PIPE' || $type eq 'TCP') {
        if (!stream_open($client,
            $server->{accepted_fh},
            $STREAM_READABLE | $STREAM_WRITABLE)){
            _close($server->{accepted_fh});
            $err = $!;
            goto done;
        }
    } elsif ($type eq 'UDP') {
        die "not implemented yet";
    } else {
        die "Unknown type";
    }
    
    done: {
        #Process queued fds
        if ( @{$server->{queued_fds}} ) {
            my $fh = shift @{$server->{queued_fds}};
            $server->{accepted_fh} = $fh;
            $server->{accepted_fd} = fileno $fh;
        } else {
            $server->{accepted_fd} = -1;
            $server->{accepted_fh} = 0;
            if ($err == 0) {
                $loop->io_start($server->{io_watcher}, $POLLACCEPT);
            }
        }
    };
    
    return $err ? 0 : 1;
}

sub stream_open {
    my ($stream, $fh, $flags) = @_;
    my $fd = fileno $fh;
    if (!defined $fd) { return; }
    assert($fd >= 0);
    $stream->{flags} |= $flags;
    if ($stream->{type} eq 'TCP') {
        if (($stream->{flags} & $TCP_NODELAY) && uv__tcp_nodelay($fh, 1)){
            return;
        }
        #TODO Use delay the user passed in.
        if (($stream->{flags} & $TCP_KEEPALIVE) && uv__tcp_keepalive($fh, 1, 60)){
            #return -errno;
            return;
        }
    }
    
    $stream->{io_watcher}->{fh} = $fh;
    $stream->{io_watcher}->{fd} = $fd;
    return 1;
}

sub __stream_queue_fd {
    my ($stream, $fd) = @_;
    push @{$stream->{queued_fds}}, $fd;
    return 1;
}

#We get called here from directly following a call to connect(2).
#In order to determine if we've errored out or succeeded must call
#getsockopt.
sub _stream_connect {
    my $loop = shift;
    my $stream = shift;
    my $error = 0;
    $! = 0;
    my $req = $stream->{connect_req};
    assert($stream->{type} eq 'TCP' || $stream->{type} eq 'NAMED_PIPE');
    assert($req);
    if ($stream->{delayed_error}) {
        # To smooth over the differences between unixes errors that
        # were reported synchronously on the first connect can be delayed
        # until the next tick--which is now.
        $error = $stream->{delayed_error};
        $stream->{delayed_error} = 0;
    } else {
        # Normal situation: we need to get the socket error from the kernel.
        assert(stream_fd($stream) >= 0);
        if (my $e = getsockopt(stream_fh($stream),SOL_SOCKET,SO_ERROR)){
            $error = unpack ("I", $e);
        } else {
            die $!;
        }
    }
    
    if ($error == EINPROGRESS || $error == EISCONN ) {
        return;
    }
    
    $stream->{connect_req} = undef;
    $loop->req_unregister($req);
    $loop->io_stop($stream->{io_watcher}, $POLLOUT);
    
    if ($req->{cb}) {
        $! = $error;
        $req->{cb}->($req, $error);
    }
}

sub __stream_recv_cmsg {
    my ($stream, $buf) = @_;
    my @fds = @{$buf->{fds}};
    foreach my $fh (@fds) {
        #Already has accepted fd, queue now
        if ($stream->{accepted_fd} != -1) {
            __stream_queue_fd($stream, $fh);
        } else {
            $fh->blocking(0);
            $fh->autoflush(1);
            $stream->{accepted_fd} = fileno $fh;
            $stream->{accepted_fh} = $fh;
            $stream->{flags} |= $STREAM_READABLE | $STREAM_WRITABLE;
        }
    }
    
    return 1;
}

my $BUFLEN = 15 * 1024;
sub _read {
    my $loop = shift;
    my $stream = shift;
    $stream->{flags} &= ~$STREAM_READ_PARTIAL;
    my $inHdr;
    my $is_ipc = $stream->{type} eq 'NAMED_PIPE' && $stream->{ipc};
    #Prevent loop starvation when the data comes in as fast as (or faster than)
    #we can read it. XXX Need to rearm fd if we switch to edge-triggered I/O.
    my $count = 32;
    $! = 0;
    my $fh;
    
    while (($stream->{read_cb} || $stream->{read2_cb})
        && ($stream->{flags} & $STREAM_READING)
        && ($count-- > 0)) {
        my $nread;
        my $buf = {
            base => '',
            len => 0,
            fds => []
        };
        
        assert(stream_fd($stream) >= 0);
        if (!$is_ipc) {
            do {
                $nread = sysread(stream_fh($stream), $buf->{base}, $BUFLEN);
            } while (!defined $nread && $! == EINTR);
            $buf->{len} = $nread ? $nread : 0;
        } else {
            Rum::Loop::Core::nonblock(stream_fh($stream), 1);
            do {
                $nread = recvmsg(stream_fh($stream), $buf);
            } while (!defined $nread && $! == EINTR);
        }
        
        if (!defined $nread) {
            #Error
            if ($! == EAGAIN || $! == EWOULDBLOCK ) {
                #Wait for the next one.
                if ($stream->{flags} & $STREAM_READING) {
                    $loop->io_start($stream->{io_watcher}, $POLLIN);
                }
                $stream->{read_cb}->($stream, 0, $buf);
            } else {
                if ($isWin && $! == ECONNABORTED){
                    $! = ECONNRESET;
                }
                #Error. User should call uv_close().
                $stream->{read_cb}->($stream, -1, $buf);
                assert(!$loop->io_active($stream->{io_watcher}, $POLLIN),
                        "stream read_cb(status=-1) did not call Rum::Loop::close()");
            }
            return;
        } elsif ($nread == 0) { #EOF
            __stream_eof($loop, $stream, $buf);
            return 1;
        } else {
            #Successful read
            if ($is_ipc) {
                my $ret = __stream_recv_cmsg($stream, $buf);
                if (!$ret) {
                    $stream->{read_cb}->($stream, $!, $buf);
                    return;
                }
            }
            
            $stream->{read_cb}->($stream, $nread, $buf);
        }
    }
    
    return 1;
}

sub __stream_read_cb {
    my ($stream,$status,$buf,$type) = @_;
    $stream->{read_cb}->($stream, $status, $buf);
    return;
    if ($stream->{read_cb}) {
        $stream->{read_cb}->($stream, $status, $buf);
    } else {
        $stream->{read2_cb}->($stream, $status, $buf, $type);
    }
}

sub __stream_eof {
    my ($loop,$stream, $buf) = @_;
    $stream->{flags} |= $STREAM_READ_EOF;
    $loop->io_stop($stream->{io_watcher}, $POLLIN);
    if (!$loop->io_active($stream->{io_watcher}, $POLLOUT)) {
        $loop->handle_stop($stream);
    }
    
    __stream_read_cb($stream, $EOF, $buf, $UNKNOWN_HANDLE);
}

sub stream_close {
    my $loop = shift;
    my $handle = shift;
    
    $loop->io_close($handle->{io_watcher});
    read_stop($loop,$handle);
    $loop->handle_stop($handle);
    
    if ($handle->{io_watcher}->{fd} != -1) {
        #Don't close stdio file descriptors.  Nothing good comes from it.
        my $fd = $handle->{io_watcher}->{fd};
        if ($fd != fileno STDERR && $fd != fileno STDIN
                    && $fd != fileno STDOUT ) {
            _close($handle->{io_watcher}->{fh});
        }
        $handle->{io_watcher}->{fd} = -1;
        $handle->{io_watcher}->{fh} = 0;
    }
    
    if ($handle->{accepted_fd} != -1) {
        _close($handle->{accepted_fh});
        $handle->{accepted_fh} = 0;
        $handle->{accepted_fd} = -1;
    }
    
    #Close all queued fds
    foreach my $fh ( @{$handle->{queued_fds}} ) {
        _close($fh);
    }
    
    $handle->{queued_fds} = [];
    assert(!$loop->io_active($handle->{io_watcher},
            $POLLIN | $POLLOUT));
}

sub read_stop {
    my $loop = shift;
    my $stream = shift;
    #Sanity check. We're going to stop the handle unless it's primed for
    #writing but that means there should be some kind of write action in
    #progress.
    assert(!$loop->io_active($stream->{io_watcher}, $POLLOUT) ||
         !QUEUE_EMPTY($stream->{write_completed_queue}) ||
         !QUEUE_EMPTY($stream->{write_queue}) ||
         $stream->{shutdown_req} ||
         $stream->{connect_req});
    
    $stream->{flags} &= ~$STREAM_READING;
    $loop->io_stop($stream->{io_watcher}, $POLLIN);
    if (!$loop->io_active($stream->{io_watcher}, $POLLOUT)) {
        $loop->handle_stop($stream);
    }
    
    undef $stream->{read_cb};
    undef $stream->{read2_cb};
    return 1;
}

sub stream_destroy {
    my $loop = shift;
    my $stream = shift;
    my $req;
    my $q;
    
    assert(!$loop->io_active($stream->{io_watcher},
            $POLLIN | $POLLOUT));
    
    assert($stream->{flags} & $CLOSED);
    
    if ($stream->{connect_req}) {
        $loop->req_unregister($stream->{loop}, $stream->{connect_req});
        $stream->{connect_req}->{cb}->($stream->{connect_req}, $ECANCELED);
        $stream->{connect_req} = undef;
    }
    
    while (!QUEUE_EMPTY($stream->{write_queue})) {
        $q = QUEUE_HEAD($stream->{write_queue});
        $req = $q->{data};
        QUEUE_REMOVE($q);
        $loop->req_unregister($stream->{loop}, $req);
        undef $req->{bufs};
        if ($req->{cb}) {
            $req->{cb}->($req, $ECANCELED);
        }
    }
    
    while ( !QUEUE_EMPTY($stream->{write_completed_queue}) ) {
        $q = QUEUE_HEAD($stream->{write_completed_queue});
        $req = $q->{data};
        QUEUE_REMOVE($q);
        $loop->req_unregister($req);
        if ($req->{bufs}) {
            $stream->{write_queue_size} -= uv__write_req_size($req);
            undef $req->{bufs};
        }
        
        if ($req->{cb}) {
            $req->{cb}->($req, $req->{error});
        }
    }
    
    if ($stream->{shutdown_req}) {
        # The ECANCELED error code is a lie, the shutdown(2) syscall is a
        # fait accompli at this point. Maybe we should revisit this in v0.11.
        # A possible reason for leaving it unchanged is that it informs the
        # callee that the handle has been destroyed.
        $loop->req_unregister($stream->{shutdown_req});
        $stream->{shutdown_req}->{cb}->($stream->{shutdown_req}, $ECANCELED);
        undef $stream->{shutdown_req};
    }
    
    ##Totally destroy stream queue circular references
    delete $stream->{write_queue}->{next};
    delete $stream->{write_queue}->{prev};
    delete $stream->{write_queue}->{data};
    delete $stream->{write_completed_queue}->{next};
    delete $stream->{write_completed_queue}->{prev};
    delete $stream->{write_completed_queue}->{data};
    delete $stream->{io_watcher};
}

sub _read_start_common {
    my ($loop, $stream ,$read_cb,$read2_cb) = @_;
    $! = 0;
    my $type = $stream->{type};
    assert($type =~ /(TCP|NAMED_PIPE|TTY)/, $type);
    
    if ($stream->{flags} & $CLOSING) {
        $! = EINVAL;
        return;
    }
    
    ##The UV_STREAM_READING flag is irrelevant of the state of the tcp - it just
    ##expresses the desired state of the user.
    $stream->{flags} |= $STREAM_READING;
    
    ##TODO: try to do the read inline?
    ##TODO: keep track of tcp state. If we've gotten a EOF then we should
    ##not start the IO watcher.
    assert(stream_fd($stream) >= 0, stream_fd($stream));
    $stream->{read_cb}  = $read_cb;
    $stream->{read2_cb} = $read2_cb;
    undef $stream->{buffer};
    #some tests fails when we do read inline, need to fix test before
    #implement inline reads as suggested by libuv team
    #if (!_read($loop,$stream) && $stream->{io_watcher}->{fd} != -1){
        $loop->io_start($stream->{io_watcher}, $POLLIN);
        $loop->handle_start($stream);
    #}
    
    return 1;
}

sub shutdown {
    my ($loop, $req, $stream, $cb) = @_;
    assert(($stream->{type} eq 'TCP' || $stream->{type} eq 'NAMED_PIPE'),
         "uv_shutdown (unix) only supports uv_handle_t right now");

    if (!($stream->{flags} & $STREAM_WRITABLE) ||
        $stream->{flags} & $STREAM_SHUT ||
        $stream->{flags} & $STREAM_SHUTTING ||
        $stream->{flags} & $CLOSED ||
        $stream->{flags} & $CLOSING) {
        $! = ENOTCONN;
        return;
    }
    
    assert(stream_fd($stream) >= 0);
    #Initialize request
    $loop->req_init($req, 'SHUTDOWN');
    $req->{handle} = $stream;
    $req->{cb} = $cb;
    $stream->{shutdown_req} = $req;
    $stream->{flags} |= $STREAM_SHUTTING;
    $loop->io_start($stream->{io_watcher}, $POLLOUT);
    return 1;
}

sub read_start {
    return _read_start_common(@_);
}

sub stream_fd {
    my $handle = shift;
    return $handle->{io_watcher}->{fd};
}

sub handle_fd {
    my $handle = shift;
    return $handle->{io_watcher}->{fd};
}

sub stream_fh {
    my $handle = shift;
    return $handle->{io_watcher}->{fh};
}


sub is_readable {
    my ($loop, $handle) = @_;
    return !!($handle->{flags} & $STREAM_READABLE);
}


sub is_writable {
  my ($loop, $handle) = @_;
    return !!($handle->{flags} & $STREAM_WRITABLE);
}

1;

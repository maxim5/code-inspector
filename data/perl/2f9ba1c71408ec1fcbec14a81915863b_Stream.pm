package Rum::Wrap::Stream;
use strict;
use warnings;
use lib '../../';
use Rum::Wrap::Handle 'getHandleData';
use Scalar::Util 'weaken';
use POSIX qw(INT_MAX);
use Rum::Loop::Utils 'assert';
use Data::Dumper;
use Rum::String;

my $loop = Rum::Loop::default_loop();

use base qw/Exporter/;
our @EXPORT = qw (
    readStart
    readStop
    shutdown
    writeBuffer
    writeAsciiString
    writeUtf8String
    writeUcs2String
    writev
    stream
    UpdateWriteQueueSize
);

sub StreamWrap {
    my ($object, $stream, $provider) = @_;
    Rum::Wrap::Handle::HandleWrap($object,$stream,$provider);
}

sub stream {
    my $this = shift;
    return $this->{handle__};
}

sub readStop {
    my $wrap = shift;
    $loop->read_stop($wrap->stream()) or return $!;
    return 0;
}

sub readStart {
    my $wrap = shift;
    $loop->read_start($wrap->stream(), \&OnRead) or return $!;
    return 0;
}

sub OnRead {
    my ($handle, $nread, $buf) = @_;
    
    my $wrap = getHandleData($handle);
    my $type = 'UNKNOWN_HANDLE';
    
    if (is_named_pipe_ipc($wrap) &&
            $loop->pipe_pending_count($handle) > 0 ){
        $type = $loop->pipe_pending_type($handle);
    }
    
    OnReadCommon($handle, $nread, $buf, $type);
}

sub OnReadCommon {
    my ($handle, $nread, $buf, $pending) = @_;
    my $wrap = getHandleData($handle);
    DoRead($wrap,$handle, $nread, $buf, $pending);
}

sub AcceptHandle {
    my ($wrap, $pipe, $pending) = @_;
    my $handle = $wrap->{handle__};
    $! = 0;
    if (!$loop->accept($pipe,$handle)){
        die $!;
    }
    return $wrap;
}

sub DoRead {
    my ($wrap,$handle,$nread,$buf,$pending) = @_;
    my @argv = (
        $nread,
        undef,
        undef
    );
    
    if ($nread < 0) {
        if ($buf->{base}){
            undef $buf->{base};
        }
        
        Rum::MakeCallback2($wrap,'onread',$wrap,@argv);
        return;
    }
    
    if ($nread == 0) {
        undef $buf->{base};
        return;
    }
    
    $argv[1] = $buf;
    my $pending_object;
    if ($pending =~ /(TCP|NAMED_PIPE|UDP)/) {
        my $TCPwrap = Rum::Wrap::TCP->new();
        $pending_object = AcceptHandle($TCPwrap, $handle, $pending);
    } else {
        assert($pending eq "UNKNOWN_HANDLE");
    }
    
    if ($pending_object) {
        $argv[2] = $pending_object;
    }
    
    Rum::MakeCallback2($wrap,'onread',$wrap,@argv);
}

sub is_named_pipe {
    return shift->{handle__}->{type} eq 'NAMED_PIPE';
}

sub is_named_pipe_ipc {
    my $self = shift;
    return is_named_pipe($self) &&
           ($self->stream()->{ipc} && $self->stream()->{ipc} != 0);
}

sub is_tcp {
    shift->stream()->{type} eq 'TCP';
}

sub UpdateWriteQueueSize {
    my $this = shift;
    my $write_queue_size = $this->{handle__} ?
                            $this->{handle__}->{write_queue_size} :
                            $this->{writeQueueSize};
    
    $this->{writeQueueSize} = $write_queue_size;
}

sub writeAsciiString {
    return WriteStringImpl('ascii',@_);
}

sub writeUtf8String {
    return WriteStringImpl('utf8',@_);
}

sub writeBuffer {
    return WriteStringImpl('buffer',@_);
}

sub writev {
    return WriteStringImpl('writev',@_);
}

sub WriteStringImpl {
    my $encoding = shift;
    my $wrap = shift;
    my $req_wrap_obj = shift;
    my $string = shift;
    my $send_handle_obj = shift;
    
    my $writev = ref $string eq 'ARRAY' ? 1 : 0;
    my $req_wrap;
    my $data = '';
    my $data_size = 0;
    my $buf;
    my $buf_count = $writev ? scalar @{$string} : 1;
    my $err = 0;
    $! = 0;
    
    my $try_write = 0;
    
    if ($writev) {
        for (0 .. @{$string} - 1){
            my $enc = $string->[$_]->{enc};
            $data_size += Rum::String::write($string->[$_]->{chunk}, $string->[$_], $enc);
        }
    } else {
        $data_size = Rum::String::write($string, $string, $encoding);
        #Try writing immediately if write size isn't too big
        $try_write = !$writev && $data_size <= (16*1024) &&
                    (!is_named_pipe_ipc($wrap) || !$send_handle_obj);
    }
    
    if (!$writev && $try_write) {
        #die Dumper $data_size;
        $buf = [$string];
        my $count = 1;
        $err = TryWrite($wrap, $buf, $count);
        
        #Failure
        if ($err != 0){
            goto done;
        }
        
        #Success
        if ($count == 0) {
            goto done;
        }
        
        #Partial write
        assert($count == 1);
    }
    
    $req_wrap = $req_wrap_obj;
    
    if ($try_write) {
        #Copy partial data
        $data_size = length $buf->[0];
        $data = $buf->[0];
    }
    else {
        $data = $string;
        $data_size = length $string;
    }
    
    $buf = $loop->buf_init($data);
    
    if (!is_named_pipe_ipc($wrap)) {
        $err = DoWrite(
                    $wrap,
                    $req_wrap,
                    $buf,
                    $buf_count,
                    undef,
                    \&AfterWrite);
    } else {
        my $send_handle = undef;
        if ($send_handle_obj) {
            my $wrap = $send_handle_obj;
            $send_handle = $wrap->{handle__};
            assert($req_wrap);
            weaken($req_wrap->{handle} = $send_handle_obj);
        }
        
        $err = DoWrite(
            $wrap,
            $req_wrap,
            $buf,
            $buf_count,
            $send_handle,
            \&AfterWrite
        );
    }
    
    Dispatch($req_wrap);
    $req_wrap->{async} = 1;
    
    if ($err) {
        undef $req_wrap;
    }
    
    done: {
        if ($err && $!){
            $req_wrap_obj->{error} = $!;
        }
        $req_wrap_obj->{bytes} = $data_size;
        return $err;
    }
}

sub Dispatch {
    my $req_wrap = shift;
    weaken($req_wrap->{data} = $req_wrap);
}

sub DoWrite {
    my ($wrap,$w,$bufs,$count,$send_handle,$cb) = @_;
    my $r = 0;
    if (!$send_handle) {
        $r = $loop->write($w, $wrap->stream(), $bufs, $count, $cb);
    } else {
        $r = $loop->write2($w, $wrap->stream(), $bufs, $count, $send_handle, $cb);
    }
    
    UpdateWriteQueueSize($wrap);
    return $r ? 0 : $!;
}

sub TryWrite {
    my ($wrap, $bufs, $count) = @_;
    my $err = 0;
    my $written = 0;   
    $! = 0;
    $err = $loop->try_write($wrap->stream(), $bufs, $count);
    unless (defined $err){
        return $!;
    }
    
    #Slice off the buffers: skip all written buffers and slice the one that
    #was partially written.
    $written = $err;
    for (; $written != 0 && $count > 0; $count--) {
        #Slice
        if (length $bufs->[0] > $written) {
            $bufs->[0] = substr $bufs->[0],$written;
            $written = 0;
        #Discard
        } else {
            $written -= length $bufs->[0];
        }
    }
    
    $_[1] = $bufs;
    $_[2] = $count;
    return 0;
}

sub AfterWrite {
    my ($req, $status) = @_;
    my $wrap = getHandleData($req->{handle});
    my $req_wrap_obj = $req;
    
    #The wrap and request objects should still be there.
    assert($req_wrap_obj);
    assert($wrap, "wrap object should still there");
    
    #Unref handle property
    $loop->unref($req_wrap_obj->{handle});
    delete $req_wrap_obj->{handle};
    UpdateWriteQueueSize($wrap);
    
    my @argv = (
        $status,
        $wrap,
        $req_wrap_obj,
        undef
    );
    
    if ($status){
        $argv[3] = $status;
    }
    
    Rum::MakeCallback2($req_wrap_obj,'oncomplete', @argv);
    undef $req_wrap_obj;
}

sub shutdown {
    my $wrap = shift;
    assert(CORE::ref $wrap);
    my $req_wrap_obj = shift;
    
    $loop->shutdown($req_wrap_obj, $wrap->{handle__}, \&AfterShutdown)
            or return $!;
    
    return 0;
}

sub AfterShutdown {
    my ($req, $status) = @_;
    my $req_wrap = $req;
    my $wrap = getHandleData($req->{handle});
    Rum::MakeCallback2($req_wrap,'oncomplete',$status,$wrap,$req_wrap);
    undef $req_wrap;
}

1;

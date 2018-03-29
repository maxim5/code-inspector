package AnyEvent::XMPP::Stream;
use strict;
no warnings;
use AnyEvent::Socket;
use AnyEvent::Handle;
use AnyEvent::XMPP::Parser;
use AnyEvent::XMPP::Error::Exception;
use AnyEvent::XMPP::Util qw/dump_twig_xml bare_jid/;
use AnyEvent::XMPP::Namespaces qw/xmpp_ns xmpp_ns_maybe/;
use AnyEvent::XMPP::Node qw/simxml/;
use Encode;

our $DEBUG = 0;

use base qw/Object::Event/;

=head1 NAME

AnyEvent::XMPP::Stream - Class for exchanging XMPP "XML" protocol messages.

=head1 SYNOPSIS

   use AnyEvent::XMPP::Stream;

   my $stream = AnyEvent::XMPP::Stream->new;

   $stream->reg_cb (
      connect_error => sub { ... },
      connected     => sub {
         my ($stream, $peerhost, $peerport) = @_;

         $stream->send_header;

         # ...
      },
      disconnected  => sub { ... },
      error => sub {
         my ($self, $error) = @_;

         warn "error: " . $error->string . "\n";

         $self->stop_event; # important, see documentation!
      },
      recv => sub {
         my ($stream, $node) = @_;

         warn "recevied stanza type: " . $node->meta->type . "\n";
      }
   );

   $stream->connect ('jabber.org', '5222');


   # Or for the more brave people:

   use base qw/AnyEvent::XMPP::Stream/;

   sub new {
      my $this  = shift;
      my $class = ref($this) || $this;
      my $self  = $class->SUPER::new (@_); # also important, see doc!

      return $self
   }

=head1 DESCRIPTION

This module provides basic TCP/TLS connectivity and knows how to parse XMPP
stanzas and partial "XML" tags. And provides the ability to send XMPP stanzas
and other partial "XML" stuff for XMPP handshaking.

It's used by L<AnyEvent::XMPP::Stream::Client> and L<AnyEvent::XMPP::Stream::Component>.

For TCP connecting you can use the C<connect> method, but it's also possible to
have your own TCP connect function and just set the file handle via the
C<set_handle> method. That is useful in case you might want to build an XMPP
server.

You may subclass from this connection or use the event interface (see below, in
the EVENTS section). If you subclass from this class you B<must> call the
constructor of this class, because it's needed by L<Object::Event> to
work properly.

=head1 METHODS

=over 4

=item $stream = AnyEvent::XMPP::Stream->new (%args)

This is the constructor, you must call it from all subclasses you write.

B<NOTE:> The C<$stream> object, as well, as all subclasses, guarantee you that
you can use the C<heap> member of these objects, which stores a hash for you to
store any data you want to associate with the stream. For example timers or
anything else.  All subclasses should also take care that this member is not
used for other purposes.

C<%args> hash may contain these options:

=over 4

=item default_stream_namespace => $namespace

Sets the XML namespace of the stream. The default for C<$namespace> is
'client'.  Other classes like L<AnyEvent::XMPP::Stream::Component> set it to
'component' for instance.

=item default_stream_lang => $lang

This should be the language of the human readable contents that
will be transmitted over the stream. The default will be 'en'.

Please look in RFC 3066 how C<$lang> should look like.

=item stream_end_timeout => $seconds

This is the timeout that is used for closing the connection from our
end gracefully. In case we don't receive a stream closing tag for our
sent stream closing tag, this timeout will take care to disconnect the
connection anyway. See also C<send_end> method below.

=back

=cut

{
use POSIX ();
my $stream_f;
sub stream_log {
	my $self = shift;
	my ($type,$data) = @_;
	$data = $$data if ref $data;
	return unless length $data;
	utf8::encode $data if utf8::is_utf8($data);
	defined $stream_f or do {
		return warn "no jid for log" unless $self->{jid};
		my $log = "/data/streamlogs/".bare_jid($self->{jid}).".log";
		open $stream_f, '>>:raw', $log
			or return warn ("Can't open streamlog $log: $!"),$stream_f = 0;
		select( (select($stream_f), $| = 1 )[0] );
	};
	return if $stream_f == 0;
	$data .= "\n" unless substr($data,-1,1) eq "\n";
	printf {$stream_f}
		"[%s] %s (%s)\n%s",
		POSIX::strftime('%b %d %H:%M:%S',localtime()),
		($type eq 'in' ? '<< in' : $type eq 'out' ? '>> out' : '?? '.$type),
		"$self",
		$data,
}
}

sub new {
   my $this  = shift;
   my $class = ref($this) || $this;
   my $self  = $class->SUPER::new (
      default_stream_namespace => 'client',
      default_stream_lang      => 'en',
      stream_end_timeout       => 30,
      hash                     => { },
      @_,
      enable_methods           => 1,
   );
   $self->stream_log(init => ~~localtime());

   $self->{default_stream_namespace} = xmpp_ns_maybe ($self->{default_stream_namespace});

   $self->{namespace_prefixes} = {
      $self->{default_stream_namespace} => '',
      xmpp_ns ('stream') => 'stream',
      xmpp_ns ('xml')    => 'xml',
      %{$self->{namespace_prefixes} || {}}
   };

   $self->{parser} = AnyEvent::XMPP::Parser->new;

   $self->{parser}->reg_cb (
      feed_text => sub {
         my ($parser, $txt) = @_;
         $self->debug_recv ($txt);
      },
      stream_start => sub {
         my ($parser, $node) = @_;

         $self->{stream_header} = $node;

         if (defined (my $lang = $node->attr_ns (xml => 'lang'))) {
            $self->{stream_in_lang} = $lang;
         } else {
            $self->{stream_in_lang} = $self->{default_stream_lang};
         }

         $self->recv_stanza_xml ($node);
         $self->stream_start ($node);
      },
      stream_end => sub {
         my ($parser, $node) = @_;
         $self->recv_stanza_xml ($node);
         $self->stream_end ($node);
      },
      recv => sub {
         my ($parser, $node) = @_;
         $self->recv_stanza_xml ($node);

         my $meta = $node->meta;
         my $type = $meta->{type};

         if (defined (my $lang = $node->attr_ns (xml => 'lang'))) {
            $meta->{lang} = $lang;
         } else {
            $meta->{lang} = $self->{stream_in_lang};
         }

         if ($type ne 'error') {
            $self->recv ($node);

         } else {
            $self->error (
               $self->{error} = AnyEvent::XMPP::Error::Stream->new (node => $node)
            );
         }
      },
      parse_error => sub {
         my ($parser, $ex, $data) = @_;

         $self->error (
            AnyEvent::XMPP::Error::Parser->new (
               exception => $ex, data => $data
            )
         );

         $self->disconnect ("xml error: $ex: $data");
      }
   );

   $self->set_exception_cb (sub {
      my ($ex, $ev) = @_;

      $self->error (
         AnyEvent::XMPP::Error::Exception->new (
            exception => $ex,
            context   => 'stream event callback: ' . $ev
         )
      );
   });


   return $self
}

sub cleanup_flags {
   my ($self) = @_;

   delete $self->{connected};
   delete $self->{ssl_enabled};
   delete $self->{peer_host};
   delete $self->{peer_port};
   delete $self->{write_done_queue};
   delete $self->{error};
   delete $self->{disconnect_timer};
}

=item $stream->reinit ()

This method reinitializes the parser and the writer of the stream.  Use this
method if you want to reinitialize the XMPP stream while handshaking (eg. after
TLS has been enabled or SASL authentication was finished).

=cut

sub reinit {
   my ($self) = @_;

   $self->{parser}->init;
}

=item $stream->connect ($host, $service, $timeout)

Try to connect (non blocking) to the C<$host> and C<$service>.  C<$service>
might be a value like 'xmpp-client=5222', which will cause L<AnyEvent::Socket>
to make a DNS SRV lookup, or a plain port number like '5222', which will skip
the DNS SRV lookup.

C<$timeout> is the TCP connect timeout in seconds. The default value 0 also
means that no timeout is installed.

B<NOTE>: You can reuse the C<$stream> object in case you've lost the connection.

When the connection has been established the C<set_handle> method is called
with the new file handle. So see the documentation of C<set_handle> for further
information.

=cut

sub connect {
   my ($self, $host, $service, $timeout) = @_;

   if ($self->{handle} or $self->{connect_guard}) {
      $self->disconnect ("reconnecting");
   }

   $self->cleanup_flags;

   $self->{connect_guard} =
      tcp_connect $host, $service, sub {
         my ($fh, $peer_host, $peer_port) = @_;

         unless ($fh) {
            $self->disconnect ("Couldn't create socket to $host:$service: $!");
            return;
         }

         $self->set_handle ($fh, $peer_host, $peer_port);

      }, ($timeout ? sub { $timeout } : ());
}

=item $stream->set_handle ($fh, $peer_host, $peer_port)

This method will set the file handle (socket) that is used for communication.
C<$peer_host> and C<$peer_port> should be the hostname/ip and port number of
the other TCP endpoint the socket C<$fh> is connected with. The are used mainly
for error messages.

This method will emit the C<connected> event.

=cut

sub set_handle {
   my ($self, $fh, $peer_host, $peer_port) = @_;

   $self->cleanup_flags;

   binmode $fh, ":raw";
   $self->{handle} =
      AnyEvent::Handle->new (
         fh       => $fh,
 #        peername => $self->{host},
 #        tls_ctx  => { verify => 1 , verify_peername => 'xmpp' },

         on_eof => sub {
            $self->disconnect (
               "EOF on connection to $self->{peer_host}:$self->{peer_port}"
               . ($self->{error} ? ": " . $self->{error}->string . "."
                                 : "."));
         },
         on_error => sub {
            my ($hdl, $fatal, $msg) = @_;

            $self->disconnect (
               "Error on connection to "
               . "$self->{peer_host}:$self->{peer_port}: $! ($msg)");
         },
         on_read => sub {
            my ($hdl) = @_;
            $self->stream_log( in => \$hdl->{rbuf} );
            $self->{parser}->feed (\$hdl->{rbuf});
         },
      );

   $self->{connected} = 1;
   $self->{peer_host} = $peer_host;
   $self->{peer_port} = $peer_port;

   $self->reinit;
   $self->connected ($peer_host, $peer_port);
}

=item $stream->write_data ($data)

Please only use this method if you know what you are doing, for usual stanza
sending you should use the C<send> method (see below) and L<AnyEvent::XMPP::Node>
objects.

This method will write out unicode character data to the TCP/TLS connection.
The data will be UTF-8 encoded before they are written out, as RFC 3920
demands.

=cut

sub write_data {
   my ($self, $data) = @_;
   $self->{handle} or do {
      warn "Handle not defined";
      return;
   };
   $self->stream_log(out => $data);

   $self->{handle}->push_write (encode_utf8 ($data));
   $self->debug_send ($data);
   $self->{handle}->on_drain (sub {
      my @cbs = @{delete $self->{write_done_queue} || []};
      $_->() for @cbs;
      $self->send_buffer_empty
   }) if $self->is_connected;
}

=item $stream->send ($node)

This method will serialize and send the L<AnyEvent::XMPP::Node> object
C<$node>, which may have L<AnyEvent::XMPP::Meta> information attached to it
(see C<meta> method of C<$node>).

Sending may be intercepted by stopping the generated C<send> event.

B<NOTE:> The C<$node> will be serialized as if it's own namespace is the
default namespace. This is done so that it is ensured that no prefixes are
generated for your stanza's namespace, as it is required by XMPP.  Of course
that means that stanzas of different namespaces can't be distinguished from the
default namespace of the stream anymore.

If you want to enforce a namespace declaration use the C<add_decl_prefix> method
of L<AnyEvent::XMPP::Node> to enforce it. (Or use the C<dns> parameter if you
use C<simxml>).

=item $stream->starttls ($state, $ctx)

This method will enable TLS on the connection by using L<AnyEvent::Handle>'s
C<starttls> method with the argument of C<$state>. The default value for
C<$state> is 'connect'.  Please consult the L<AnyEvent::Handle> documentation
about the special values for C<$state> and C<$ctx>.

Usually you will just need this, if you are on the client side of an XMPP stream:
 
   $stream->starttls ('connect')

Please note that if you want to do SSL certificate checking/validating
you will currently need to access the L<AnyEvent::Handle> directly:

   my $net_ssleay_obj = $stream->{handle}->{tls};

   # and the context can be retrieved like this, if you didn't provide it
   # in the C<$ctx> argument:

   my $ctx            = AnyEvent::Handle::TLS_CTX;

Please see L<AnyEvent::Handle> for more details and L<Net::SSLeay> about
certificate validation (which you really should do, and which, arguably, should
be done by L<AnyEvent::XMPP> conveniently - patches welcome :-)

B<NOTE>: This method will call the C<reinit> method for you. As you need a new
parsing/writing context anyways.

=cut

sub starttls {
   my ($self, $state, $ctx) = @_;

   $state ||= 'connect';

   $self->{handle}->starttls ($state, $ctx);
   $self->{ssl_enabled} = 1;
   $self->reinit;
}

=item $stream->send_header ($version, %attrs)

This method sends the XMPP stream header. C<$version> is the version for this
stream, the default is '1.0'.  And if C<$version> is the empty string no
version attribute will be generated.

You may pass other attributes for the stream start tag in C<%attrs>.

=cut

sub send_header {
   my ($self, $version, %attrs) = @_;
   return unless $self->{connected};

   $version = '1.0' unless defined $version;

   my $node = simxml (
      defns => 'stream',
      node => {
         name => 'stream',
         attrs => [
            [xmpp_ns ('xml'), 'lang'] => $self->{default_stream_lang},
            ($version ne '' ? (version => $version) : ()),
            %attrs
         ]
      }
   );
   # Note: We don't send a header currently. If we ever do that
   # try to check whether it works with all servers. And make it
   # optional (opt-out), Zimbra seems to have problems with this.
   $node->set_only_start;
   $node->add_decl_prefix (xmpp_ns ('stream') => 'stream');
   $node->add_decl_prefix ($self->{default_stream_namespace} => '');
   $self->send ($node);
}

=item $stream->send_end ()

This method will send a closing stream stanza if we are connected.
Please use this method whenever you want to close a connection gracefully.
It will not immediately disconnect the stream, but wait for the stream
end of the other side and then disconnect the tcp stream.

There is a timeout installed, in case the other side doesn't send any stream
end. See C<stream_end_timeout> argument to C<new>.

=cut

sub send_end {
   my ($self) = @_;
   return unless $self->{connected};

   $self->write_data ("</stream:stream>"); # shameless, but XML
   $self->{disconnect_timer} =
      AnyEvent->timer (after => $self->{stream_end_timeout}, cb => sub {
         $self->disconnect ("receival of stream end timeouted.");
      });
}

=item $stream->disconnect ($msg)

Call this method if you want to kill the connection forcefully.
C<$msg> is a human readable message for logging purposes.

This method will emit a C<disconnected> event in case we were connected or a
C<connect_error> event in case we weren't connected yet. And no event at all if
not even the C<connect> method had been called or a handle set via
C<set_handle>.

=cut

sub disconnect {
   my ($self, $msg) = @_;

   if ($self->{connected}) {
      delete $self->{handle};
      delete $self->{connect_guard};
      $self->disconnected ($self->{peer_host}, $self->{peer_port}, $msg);

   }
   elsif ($self->{connect_guard}) {
      delete $self->{handle};
      delete $self->{connect_guard};
      $self->connect_error ($msg);
   }
   elsif ($self->{handle}) {
      warn "Have handle, but not connected?";
      delete $self->{handle};
      delete $self->{connect_guard};
      $self->connect_error ($msg);
   }

   $self->cleanup_flags;
}

=item $con->stream_header ()

This method will return the start element the other side sent us as
L<AnyEvent::XMPP::Node> object.

=cut

sub stream_header { $_[0]->{stream_header} }

=item $stream->is_connected ()

Returns true if the connection is connected and ready to send and receive
any kind of stanzas or protocol messages.

=cut

sub is_connected {
   my ($self) = @_;
   $self->{handle} && $self->{connected}
}

=item $stream->cleanup ()

Use this method if you want to destroy the C<$stream> object.
This method will free internal cyclic data structures.

=cut

sub cleanup {
   my ($self) = @_;

   $self->cleanup_flags;

   if ($self->{handle}) {
      delete $self->{handle};
   }
   if ($self->{connect_guard}) {
      delete $self->{connect_guard};
   }

   if ($self->{parser}) {
      $self->{parser}->remove_all_callbacks;
      $self->{parser}->cleanup;
      delete $self->{parser};
   }
}

=back

=head1 EVENTS

These events are provided via the event interface of L<Object::Event>.
You can register event callbacks via the C<reg_cb> method, please consult the 
L<Object::Event> documentation for more details or the examples in
this documentation or the L<AnyEvent::XMPP> distribution.

=over 4

=item error => $error

This is the error event. C<$error> is a descendant of the class
L<AnyEvent::XMPP::Error> and has for instance the C<string> method to produce a
human readable string.

Please note that if you don't stop the error event somewhere a default handler
will kick in and print out a nice warning telling you what error happened and
that you forgot to stop the event.

Here is an example:

   $stream->reg_cb (error => sub {
      my ($stream, $error) = @_;
      warn "got error: " . $error->string . "\n":
      $stream->stop_event; # see documentation of Object::Event.
      ()
   });

=cut

sub error : event_cb(ext_after) {
   my ($self, $error) = @_;

   warn "unhandled error in AnyEvent::XMPP::Stream: " . $error->string . "."
        ." Please read the documentation of the 'error' event, to inhibit this"
        ." warning!\n";
}

=item connected => $peer_host, $peer_port

This event is emitted when the TCP connection is connected (and/or a handle
has been set via C<set_handle>). C<$peer_host> and C<$peer_port>
are the hostname and port number of the other TCP endpoint.

=cut

sub connected : event_cb {
   my ($self, $ph, $pp) = @_;

   if ($DEBUG) {
      warn "connected to $ph:$pp\n";
   }
}

=item connect_error => $error_string

This event is emitted when a connection try has been initiated via the
C<connect> method and couldn't connect (or timeouted).
C<$error_string> is a human readable string which explains why the connection
couldn't be made.

=cut

sub connect_error : event_cb {
   my ($self, $str) = @_;

   if ($DEBUG) {
      warn "couldn't connect: $str\n";
   }
}

=item disconnected => $peer_host, $peer_port, $reason

This event is emitted when the connection disconnected for some C<$reason>.

=cut

sub disconnected : event_cb {
   my ($self, $ph, $pp, $reas) = @_;

   if ($DEBUG && $reas !~ /expected stream end/) {
      warn "disconnected from $ph:$pp: $reas\n";
   }
}

=item stream_start => $node

This event is emitted when a stream header (start tag) has been parsed,
it's information (attributes mainly) are available via C<$node> which is
an L<AnyEvent::XMPP::Node> object.

=cut

sub stream_start : event_cb { }

=item stream_end => $node

This event is emitted whenever the stream end tag has been received.  C<$node>
is the L<AnyEvent::XMPP::Node> object containing information about the stream
element.

=cut

sub stream_end : event_cb {
   my ($self) = @_;

   if ($self->{disconnect_timer}) {
      delete $self->{disconnect_timer};
      $self->disconnect ("recevied expected stream end.");
   }
}

=item recv_stanza_xml => $node

This is a debugging event, which is invoked for any "XML" elements that have 
been received (also for the tag received in C<stream_start>, see above).
Please also note that two C<$node> elements are generated for the start tag and 
the end tag of the stream. The first one will contain the text of the start
and the last one of the end, so that you get a consistent debugging output
when you use the C<as_string> method of C<$node>.

C<$node> is an L<AnyEvent::XMPP::Node> object.

=cut

sub recv_stanza_xml : event_cb {
   my ($self, $node) = @_;

   if ($DEBUG) {
      warn ">>> $self->{jid}:$self->{peer_host}:$self->{peer_port} >>>\n"
           . dump_twig_xml ($node->raw_string)
   }
}

=item sent_stanza_xml => $data

This debugging event is generated when a L<AnyEvent::XMPP::Node> has been
serialized and sent out. You will catch 99% of the outside traffic with this,
maybe except exotic things like whitespace pings.
For a 100% coverage always use the C<debug_send> event!

=cut

sub sent_stanza_xml : event_cb {
   my ($self, $data) = @_;

   if ($DEBUG) {
      warn "<<< $self->{jid}:$self->{peer_host}:$self->{peer_port} <<<\n"
           . dump_twig_xml ($data);
   }
}

=item recv => $node

This event is emitted when an XMPP stanza has been received. C<$node>
is an L<AnyEvent::XMPP::Node> object, with meta information attached
(see C<meta> method of L<AnyEvent::XMPP::Node>).

The attached meta information is an object of type L<AnyEvent::XMPP::Meta>.

=cut

sub recv : event_cb {
   my ($self, $node) = @_;
}

=item send => $node

This event is emitted when a C<$node> (see C<recv> event about the type) is
about to be send. If you stop the event the stanza will not be transmitted.

You may attach a meta information object of L<AnyEvent::XMPP::Meta> to the C<$node>.

=cut

sub send : event_cb(ext_after) {
   my ($self, $node) = @_;

   if ($node->meta->sent_cbs) {
      push @{$self->{write_done_queue}}, $node->meta->sent_cbs
   }

   if ((defined $node->meta->{lang})
       && $node->meta->{lang} ne $self->{default_stream_lang}) {
      $node->attr_ns (xml => lang => $node->meta->{lang});
   }

   $node->meta->{lang} = $self->{default_stream_lang}
      unless defined $node->meta->{lang};

   $self->write_data (
      my $stanza_data =
         $node->as_string (0, {
            %{$self->{namespace_prefixes}},
            STREAM_NS => $self->{default_stream_namespace}
         })
   );

   $self->sent_stanza_xml ($stanza_data);
}

=item send_buffer_empty

This event is emitted each time the write buffer becomes empty.

For instance if you want to write out the stream end tag and want to disconnect
afterwards you can do this:

   $stream->reg_cb (send_buffer_empty => sub {
      my ($stream) = @_;

      $stream->unreg_me;
      $stream->disconnect ('done');
   });

   $stream->send_end;

B<NOTE>: The L<AnyEvent::XMPP::Meta> meta information, which can be attached to
L<AnyEvent::XMPP::Node> objects, provides means to set callbacks for execution
when the data for a stanza has been sent out.

=cut

sub send_buffer_empty : event_cb { }

=item debug_recv => $data

This is a debugging event, which will be emitted whenever some chunk of data
has been received. C<$data> is the received unicode character data chunk.

=cut

sub debug_recv : event_cb {
   my ($self, $data) = @_;

   if ($DEBUG > 10) {
      warn ">>> RECV($self->{peer_host}:$self->{peer_port}) >>>\n[[$data]]\n";
   }
}

=item debug_send => $data

This is a debugging event, which will be emitted whenever unicode character
C<$data> is sent outward. 

=cut

sub debug_send : event_cb {
   my ($self, $data) = @_;

   if ($DEBUG > 10) {
      warn "<<< SEND($self->{peer_host}:$self->{peer_port}) <<<\n[[$data]]\n";
   }
}

=back

=head1 AUTHOR

Robin Redeker, C<< <elmex@ta-sa.org> >>

=head1 SEE ALSO

=head1 COPYRIGHT & LICENSE

Copyright 2009, 2010 Robin Redeker, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;


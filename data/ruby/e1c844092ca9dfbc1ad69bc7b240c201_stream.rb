#
# synapse: a small XMPP server
# xmpp/stream.rb: XMPP stream library
#
# Copyright (c) 2006-2008 Eric Will <rakaur@malkier.net>
#
# $Id$
#

# Import required Ruby modules.
require 'digest/md5'
require 'idn'
require 'logger'
require 'openssl'
require 'resolv'
require 'socket'

# Import required xmppd modules.
require 'xmppd/log'

require 'xmppd/xmpp/client'

# The XMPP namespace.
module XMPP

#
# This is our base XMPP::Stream class.
# This handles socket I/O, etc. More specific
# child classes inherit from this below.
#
class Stream
    attr_accessor :auth, :socket
    attr_reader   :host, :rtime

    # Try to move away from this.
    STATE_NONE    = 0x00000000
    STATE_DEAD    = 0x00000001
    STATE_PLAIN   = 0x00000002
    STATE_ESTAB   = 0x00000004
    STATE_TLS     = 0x00000008
    STATE_SASL    = 0x00000010
    STATE_BIND    = 0x00000020
    STATE_SESSION = 0x00000040 # This is only here for state in Features.list.

    #
    # Create a new XMPP::Stream.
    # Nothing does this directly, but instead uses child classes.
    #
    # host:: [String] the remote host
    # type:: [String] either 'client' or 'server'
    # 
    # return:: [XMPP::Stream] new stream object
    #
    def initialize(host, type)
        @host     = IDN::Stringprep.nameprep(host)
        @myhost   = $config.hosts.first # A default.
        @nonce    = nil
        @resource = nil
        @rtime    = Time.now.to_f
        @state    = STATE_NONE

        @flood    = { 'stanzas'  => 0, # This is for rate limiting.
                      'mtime'    => 0,
                      'killed'   => false }

        unless type == 'server' or type == 'client'
            raise ArgumentError, "type must be 'client' or 'server'"
        end

        @type = type

        # Set up debug logging.
        if $debug
            Dir.mkdir('var/streams') unless File.exists?('var/streams')

            if client?
                unless File.exists?('var/streams/c2s')
                    Dir.mkdir('var/streams/c2s')
                end

                @logger = Logger.new("var/streams/c2s/#{@host}")
            else
                unless File.exists?('var/streams/s2s')
                    Dir.mkdir('var/streams/s2s')
                end

                @logger = Logger.new("var/streams/s2s/#{@host}")
            end

            @logger.level           = Logger::UNKNOWN
            @logger.progname        = @host
            @logger.datetime_format = '%b %d %H:%M:%S '
        else
            @logger = MyLog::DeadLogger.new
        end

        # This sets up our events for the XML parser.
        parser_initialize
    end

    ######
    public
    ######

    #
    # Generate a unique stream id.
    # I think I yanked this from a Google SoC project.
    #
    # return:: [String] random string
    #
    @@id_counter = 0

    def Stream.genid
        @@id_changed = Time.now.to_i

        time          = Time.now.to_i
        tid           = Thread.new { }.object_id
        @@id_counter += 1

        nid = (time << 48) | (tid << 16) | @@id_counter
        id  = ''

        while nid > 0
            id   += (nid & 0xFF).chr
            nid >>= 8
        end

        unless @@id_changed == time
            @@id_changed = time
            @@id_counter = 0
        end

        Digest::MD5.hexdigest(id)
    end

    #
    # Set the vhost we're serving, with IDN preps.
    #
    # value:: [String] a host from $config
    #
    # return:: [XMPP::Stream] self
    #
    def myhost=(value)
        @myhost = IDN::Stringprep.nameprep(value)
        self
    end

    #
    # Is the stream established?
    #
    # return:: [boolean] true or false
    #
    def established?
        return (STATE_ESTAB & @state != 0) ? true : false
    end

    #
    # Is the stream encrypted?
    #
    # return:: [boolean] true or false
    #
    def tls?
        return (STATE_TLS & @state != 0) ? true : false
    end

    #
    # Are we authenticated via SASL?
    #
    # return:: [boolean] true or false
    #
    def sasl?
        return (STATE_SASL & @state != 0) ? true : false
    end

    #
    # Are we ready to exchange <iq/> stanzas?
    # Right now I define this as being in TLS.
    #
    # return:: [boolean] true or false
    def iq_ready?
        tls?
    end

    #
    # Are we ready to exchange <presence/> stanzas?
    # Right now I define this as being in TLS and SASL.
    #
    # return:: [boolean] true or false
    def presence_ready?
        return true if tls? and sasl?
        return false
    end

    #
    # Are we ready to exchange <message/> stanzas?
    # Right now I define this as being in TLS and SASL
    #
    # return:: [boolean] true or false
    def message_ready?
        return true if tls? and sasl?
        return false
    end


    #
    # Have we bound a resource?
    #
    # return:: [boolean] true or false
    #
    def bind?
        return (STATE_BIND & @state != 0) ? true : false
    end

    #
    # Have they established as session?
    # This is depreciated and useless.
    #
    # return:: [boolean] true or false
    #
    def session?
        return (STATE_SESSION & @state != 0) ? true : false
    end

    #
    # Did our socket die?
    #
    # return:: [boolean] true or false
    #
    def dead?
        return (STATE_DEAD & @state != 0) ? true : false
    end

    #
    # Is this a client stream?
    #
    # return:: [boolean] true or false
    #
    def client?
        return (@type == 'client') ? true : false
    end

    #
    # Is this a server stream?
    #
    # return:: [boolean] true or false
    #
    def server?
        return (@type == 'server') ? true : false
    end

    #
    # Check for a connection timeout.
    #
    # return:: [XMPP::Stream] self
    #
    def check_timeout
        return if dead? or @socket.closed?

        error('connection-timeout') if ($time - @rtime) >= @auth.timeout

        self
    end

    #
    # Close the stream and socket, etc.
    #
    # try:: [boolean] try to write the closing tag
    #
    # return self
    #
    def close(try = true)
        write '</stream:stream>' if try

        begin
            @socket.close unless @socket.closed?
        rescue Exception => e
            # Do nothing.
        end

        @state &= ~STATE_ESTAB
        @state |= STATE_DEAD

        self
    end

    #
    # Read data from the socket, and send it off to parse.
    #
    # return:: [XMPP::Stream] self
    #
    def read
        begin
            if tls?
                data = @socket.readpartial(8192)
            else
                data = @socket.recv(8192)
            end
        rescue Errno::EAGAIN
            # Kick it back to select().
            return
        rescue Exception => e
            @logger.unknown "-> read error: #{e}"
            close
            return
        end
        if data.empty?
            @logger.unknown '-> empty read'
            close
            return
        end

        string  = ''
        string += "(#{@resource.name}) " if @resource
        string += '-> ' + data.gsub("\n", '')

        @logger.unknown string

        @rtime = $time

        parse(data)

        self
    end

    #
    # Generate a stream error, and close the stream.
    #
    # defined_condition:: [String] the error, defined in XMPP-CORE
    # application_error:: [Hash] name => error, text => description
    #
    # return:: [XMPP::Stream] self
    #
    def error(defined_condition, application_error = nil)
        err = REXML::Element.new('stream:error')
        na  = REXML::Element.new(defined_condition)

        na.add_namespace('urn:ietf:params:xml:ns:xmpp-streams')
        err << na

        if application_error
            ae      = REXML::Element.new(application_error['name'])
            ae.text = application_error['text'] if application_error['text']

            ae.add_namespace('urn:xmpp:errors')
            err << ae
        end

        establish unless established?
        @state &= ~STATE_ESTAB # Random telnet could crash us otherwise.

        write err
        close

        self
    end

    #
    # Write data to the socket.
    #
    # stanza:: [object that provides to_s] data to write
    #
    # return:: [XMPP::Stream] self
    #
    def write(stanza)
        begin
            if tls?
                @socket.write(stanza.to_s)
            else
                @socket.send(stanza.to_s, 0)
            end
        rescue Errno::EAGAIN
            retry
        rescue Exception => e
            @logger.unknown "<- write error: #{e}"
            close(false)
            return
        else
            string  = ''
            string += "(#{@resource.name}) " if @resource
            string += '<- ' + stanza.to_s
            string.gsub!("\n", '')

            @logger.unknown string
        end

        self
    end

    #######
    private
    #######

    # This module defines our parsing methods.
    include XMPP::Parser

    #
    # Resolve a host and port for a remote server.
    # First tries DNS SRV RR as per RFC3920.
    # On failure, falls back to regular DNS.
    # I think I yanked this from a Google SoC project.
    #
    # return:: [Array] addr, port
    #
    def resolve
        if client?
            rrname = '_xmpp-client._tcp.' + @host
        else
            rrname = '_xmpp-server._tcp.' + @host
        end

        resolver      = Resolv::DNS.new
        original_recs = []
        weighted_recs = []

        # See whether Ruby has the DNS SRV RR class (RUBY_VERSION >= 1.8.3)
        type        = nil
        srv_support = Resolv::DNS::Resource::IN.const_defined?('SRV')

        if srv_support
            type = Resolv::DNS::Resource::IN::SRV
        else
            type = Resolv::DNS::Resource::IN::ANY
        end

        begin
            resources = resolver.getresources(rrname, type)

            if srv_support
                resources.each do |x|
                    original_recs << { 'target'   => x.target,
                                       'port'     => x.port,
                                       'priority' => x.priority,
                                       'weight'   => x.weight }
                end
            else
                resources.each do |x|
                    classname = x.class.name.split('::').last
                    next unless classname == 'Type33_Class1'

                    priority, weight, port, target = x.data.unpack('n3a*')
                    pos  = 0
                    addr = ''

                    until target[pos] == 0
                        addr += '.' unless pos == 0
                        len   = target[pos]
                        pos  += 1
                        addr += target[pos, len]
                        pos  += len
                    end

                    original_recs << { 'target'   => addr,
                                       'port'     => port,
                                       'priority' => priority,
                                       'weight'   => weight }
                end
            end

            # Now we have all the info.
            equals = {}

            original_recs.each do |rec|
                prio         = rec['priority']
                equals[prio] = [] unless equals.include?(prio)
                equals[prio] << rec
            end

            equals.keys.sort.each do |prio|
                eqrecs = equals[prio]

                if eqrecs.size <= 1
                    rec = eqrecs.first
                    weighted_recs << [ rec['target'], rec['port'] ]
                    next
                end

                sum = 0

                eqrecs.each { |rec| sum += rec['weight'] }

                factor  = rand(sum + 1)
                sum     = 0
                allzero = true

                eqrecs.each do |rec|
                    next if rec['weight'] == 0

                    sum += rec['weight']

                    if sum >= factor
                        weighted_recs << [rec['target'], rec['port']]
                        allzero = false
                        break
                    end
                end

                if allzero
                    selectee = eqrecs[rand(eqrecs.size)]
                    weighted_recs << [selectee['target'], selectee['port']]
                end
            end
        rescue Resolv::ResolvError
            if client?
                weighted_recs << [name, 5222]
            else
                weighted_recs << [name, 5269]
            end
        end

        if block_given?
            weighted_recs.each { |x| yield(x[0], x[1]) }

            return nil
        else
            return weighted_recs.first[0, 2]
        end
    end
end

#
# This is our XMPP::ClientStream class.
# All client connections have one of these.
#
class ClientStream < Stream
    attr_reader :id, :resource

    #
    # Create a new XMPP::ClientStream.
    #
    # host:: [String] the remote host
    #
    # return:: [XMPP::ClientStream] new client stream object
    #
    def initialize(host)
        @registered = false # For In-Band Registration
        super(host, 'client')
    end

    ######
    public
    ######

    #
    # Accept a new client connection.
    #
    # return:: [XMPP::ClientStream] self
    #
    def connect
        raise RuntimeError, 'no client socket to connect with' unless @socket

        @socket.setsockopt(Socket::SOL_SOCKET, Socket::SO_REUSEADDR, 1)

        $log.c2s.info "#{@host} -> TCP connection established"

        # We don't send the first stanza, so establish isn't
        # called until they try to establish a stream first.

        self
    end

    #
    # A wrapper around XMPP::Stream#close.
    #
    # return:: [XMPP::ClientStream] self
    #
    def close(try = true)
        # If they're online, make sure to broadcast that they're not anymore.
        if try and @resource and @resource.available?
            elem = REXML::Element.new('presence')
            elem.add_attribute('type', 'unavailable')

            presence_unavailable(elem)
        end

        # Undo some refereces so GC works.
        if @resource
            @resource.user.delete_resource(@resource)
            @resouce = nil
        end

        $log.c2s.info "#{@host} -> TCP connection closed"
        super(try)
    end

    #
    # A wrapper around XMPP::Stream#error.
    #
    # return:: [XMPP::ClientStream] self
    #
    def error(*args)
        $log.c2s.error "#{@host} -> #{args[0]}"
        super(*args)
    end

    #######
    private
    #######

    #
    # Manually build and send the opening stream XML.
    # We can't use REXML here because it closes all
    # of the tags on its own.
    #
    # return:: [XMPP::ClientStream] self
    #
    def establish
        @id    = Stream.genid
        stanza = %(<?xml version='1.0'?>) +
                 %(<stream:stream ) +
                 %(xmlns='jabber:client' ) +
                 %(xmlns:stream='http://etherx.jabber.org/streams' ) +
                 %(from='#{@myhost}' ) +
                 %(id='#{@id}' ) +
                 %(version='1.0'>)

        write stanza

        if tls? and sasl?
            $log.c2s.info "#{@host} -> TLS/SASL stream established"
        elsif tls?
            $log.c2s.info "#{@host} -> TLS stream established"
        elsif sasl?
            $log.c2s.info "#{@host} -> SASL stream established"
        else
            $log.c2s.info "#{@host} -> stream established"
        end

        @state |= STATE_ESTAB
    end

    # This module contains all of our client routines.
    include XMPP::Client
end

#
# This is our XMPP::ServerStream class.
# More specific classes inherit from this below. 
#
class ServerStream < Stream
    #
    # Create a new XMPP::ServerStream.
    #
    # host:: [String] the remote host
    #
    # return:: [XMPP::ServerStream] new server stream object
    #
    def initialize(host)
        super(host, 'server')
    end

    ######
    public
    ######


    #
    # A wrapper around XMPP::Stream#close.
    #
    # return:: [XMPP::ServerStream] self
    #
    def close(*args)
        $log.s2s.info "#{@host} -> TCP connection closed"
        super(*args)
    end

    #
    # A wrapper around XMPP::Stream#error.
    #
    # return:: [XMPP::ServerStream] self
    #
    def error(*args)
        $log.s2s.error "#{@host} -> #{args[0]}"
        super(*args)
    end

    #######
    private
    #######

    #
    # Manually build and send the opening stream XML.
    # We can't use REXML here because it closes all  
    # of the tags on its own.                      
    #--                        
    # ejabberd claims '1.0' but won't even let you
    # connect without an xmlns:db attribute. If
    # it's 1.0 then how does it expect servers
    # to initiate TLS connections?
    #++
    #
    # return:: [XMPP::ServerStream] self
    #
    def establish
        stanza = %(<?xml version='1.0'?>) +
                 %(<stream:stream to='#{@host}' ) +
                 %(from='#{@myhost}' ) +
                 %(xmlns='jabber:server' ) +
                 %(xmlns:stream='http://etherx.jabber.org/streams' ) +
                 %(xmlns:db='jabber:server:dialback' ) +
                 %(version='1.0'>)

        write stanza

        $log.s2s.info "#{@host} -> stream established"

        @state |= STATE_ESTAB

        self
    end
end

#
# This is our XMPP::ServerStreamIn class.
# All incoming server connections have one of these.
#
class ServerStreamIn < ServerStream
    #
    # Create a new XMPP::ServerStreamIn.
    #
    # host:: [String] the remote host
    # socket:: [TCPSocket] the connection socket
    #
    # return:: [XMPP::ServerStreamIn] new incoming server object
    #
    def initialize(host, socket)
        super(host)

        @host   = IDN::Stringprep.nameprep(host)
        @socket = socket
    end

    ######
    public
    ######

    #
    # Accept a new incoming server connection.
    # This is an incoming socket, so stuff should be connected.
    #
    # return:: [XMPP::ServerStreamIn] self
    #
    def connect
        @socket.setsockopt(Socket::SOL_SOCKET, Socket::SO_REUSEADDR, 1)

        $log.c2s.info "#{@host} -> TCP connection established"

        # We don't send the first stanza, so establish isn't
        # called until they try to establish a stream first.

        self
    end
end

#
# This is our XMPP::ServerStreamOut class.
# All outgoing server connections have one of these.
#
class ServerStreamOut < ServerStream
    #
    # Create a new XMPP::ServerStreamOut.
    #
    # host:: [String] the remote host
    # myhost:: [String] the host we're serving from
    #
    # return:: [XMPP::ServerStringOut] new outgoing server stream object
    #
    def initialize(host, myhost)
        super(host)

        @host   = IDN::Stringprep.nameprep(host)
        @myhost = IDN::Stringprep.nameprep(myhost)
    end

    ######
    public
    ######

    #
    # Connect out to a remote server.
    #
    # return:: [XMPP::ServerStreamOut] self
    #
    def connect
        $log.s2s.info "#{@host}:5269 -> initiating TCP connection"

        addr, port = resolve

        begin
            @socket = TCPSocket.new(addr.to_s, port)
        rescue SocketError => e
            $log.s2s.info "#{host}:#{port} -> TCP connection failed"

            @state |= STATE_DEAD
        else
            @socket.setsockopt(Socket::SOL_SOCKET, Socket::SO_REUSEADDR, 1)

            $log.s2s.info "#{addr}:#{port} -> TCP connection established"

            establish
        end

        self
    end
end

end # module XMPP

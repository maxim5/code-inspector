stream = require 'stream'
buffer = require 'buffer'

pipeline = (streams) ->
  p = streams.shift()
  for stream in streams
    p = p.pipe stream if stream
  p

wait_for = (token, stream, timeout, cb) ->
  throw new Error 'cb should be a function' if typeof cb != 'function'
  chunks = []
  text = ''
  setTimeout ->
    error = new Error "Wait token timeout (#{timeout}ms)"
    cb? error, buffer.Buffer.concat chunks
    cb = null
  , timeout if timeout
  stream.on 'data', listener = (chunk) ->
    chunks.push chunk
    text += chunk.toString()
    found = text.indexOf token
    if found > -1
      stream.removeListener 'data', listener
      cb? null, buffer.Buffer.concat chunks
      cb = null

url = require 'url'
net = require 'net'
events = require 'events'

CRLF = '\r\n'
BREAK = CRLF + CRLF
CONNECT_OK = 'HTTP/1.1 200 Connection Established' + BREAK
CONNECT_BAD_GATEWAY = 'HTTP/1.1 502 Bad Gateway' + BREAK

parseHTTPHeader = (chunk) ->
  header = chunk.toString()
  lines = header.split CRLF
  req = lines[0].split ' '
  req_url = req[1]
  req_url = 'https://' + req_url if req[0] == 'CONNECT'
  dest = url.parse req_url
  return [req[0], dest, req[2], lines[1..]]

class Connection extends events.EventEmitter
  next_id = 0
  constructor: (@opts) ->
    @id = "[#{next_id++}]"
    @state = 'INITIAL'
    @client = @remote = null
    @proxy = @opts?.proxy || null
    @log_cs = @opts?.log_cs || null
    @log_sc = @opts?.log_sc || null
  listen: (socket) ->
    @client = socket
    wait_for BREAK, socket, 5000, (error, @received) =>
      if error
        @emit 'error', error, @received
        @end()
        return
      try
        [@method, @dest, @httpVersion, @headers] = parseHTTPHeader @received
      catch error
        @emit 'error', error
        @end()
        return
      @dest.port ?= {'http:': 80, 'https:': 443}[@dest.protocol]
      @state = 'REQUESTED'
      @emit 'requested', @
      @connect()
  connect: ->
    return if @state != 'REQUESTED'
    @state = 'CONNECTING'
    if @proxy
      if typeof @proxy == 'string'
        @proxy = url.parse 'http://' + @proxy
      @proxy.port ?= 80
    remote = @proxy || @dest
    @remote = net.connect remote.port, remote.hostname, =>
      pipeline [@remote, @log_sc, @client, @log_cs, @remote]
      if @proxy
        @remote.write @received
      else if @method != 'CONNECT'
        @remote.write "#{@method} #{@dest.path} #{@httpVersion}" + CRLF
        @remote.write @headers.join(CRLF)
      else
        @client.write CONNECT_OK
      @state = 'CONNECTED'
      @emit 'connected', @
    @remote.once 'error', (error) =>
      @emit 'error', error
      @bad_gateway error
    @remote.once 'end', =>
      @emit 'end'
  bad_gateway: (error) ->
    @client.write CONNECT_BAD_GATEWAY
    @client.write '<h1>502 Bad Gateway</h1>' + CRLF
    @client.write error.toString() + CRLF
    @end()
  end: ->
    @destination?.end()
    @client?.end()
    @destination = @client = null
    @state = 'DISCONNECTED'
    @emit 'end'

class Server extends events.EventEmitter
  constructor: (@opts) ->
    @server = net.createServer()
    @server.on 'connection', (socket) =>
      connection = new Connection @opts
      connection.listen(socket)
      @emit 'connection', connection
    @server.on 'error', (error) => @emit 'error', error
  listen: (port, cb) ->
    port ?= @opts.port || 8000
    @server.listen port, cb
  close: ->
    @server.close()
    @server = null

exports.createServer = (opts) -> new Server(opts)
exports.Server = Server
exports.Connection = Connection

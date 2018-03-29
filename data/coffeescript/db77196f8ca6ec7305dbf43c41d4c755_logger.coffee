stream = require 'stream'
buffer = require 'buffer'
utf8octets = require './utf8octets'

LF = '\n'
CRLF = '\r\n'

# Filter for logging
# @param format is the 1st param for console.log such as '>', '[%s]'
# @param encoding 'binary' 'utf8' 'utf8+binary'
# @param writer instead of console.log
class Logger extends stream.Stream
  constructor: (@format = '>', @encoding = 'utf8', @writer = console.log) ->
    @writable = true
  log: (data) ->
    if typeof @format != 'function'
      format = @format
    else
      format = @format()
    @writer format, data
  write: (chunk, encoding) ->
    if typeof chunk == 'string'
      @log line for line in chunk.split(LF)
    else if @encoding == 'utf8'
      utf8length = utf8octets(chunk)
      if (utf8length)
        @log line for line in chunk.toString('utf8', 0, utf8length).split(LF)
    else
      @log chunk
    @emit 'data', chunk
  end: ->
    @log '[end]'
    @emit 'end'

module.exports = (format, encoding, writer) ->
  new Logger(format, encoding, writer)

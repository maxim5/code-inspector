XRegExp = require(process.cwd() + '/lib/xregexp').XRegExp

registry = {}
def registry: registry

get '/': -> render 'default'

get '/counter': -> "# of messages so far: #{app.counter}"

at connection: ->
  app.counter ?= 0
  console.log "Connected: #{id}"
  broadcast 'connected', id: id
  
  registry.send = send

at disconnection: ->
  console.log "Disconnected: #{id}"

msg said: ->
  console.log "#{id} said: #{@text}"
  app.counter++
  send 'said', id: id, text: @text
  broadcast 'said', id: id, text: @text
  
msg networkStream: ->
  console.log "received stream data"
  console.log "#{id} said: #{@text}"
  send 'said', id: id, text: @text
  broadcast 'said', id: id, text: @text

include 'client.coffee'

view ->
  @title = 'CoffeeStat'
  @scripts = [
    'http://code.jquery.com/jquery-1.4.3.min'
    '/socket.io/socket.io'
    '/default'
    'http://github.com/DmitryBaranovskiy/raphael/raw/master/raphael-min'
    'raphael/popup'
    'highcharts/highcharts'
    'xregexp/xregexp'
  ]
  h1 @title
  div id: 'log'
  form ->
    input id: 'box'
    button id: 'say', -> 'Say'
    
  div id: "container", class: "highcharts-container", style: "height:410px; margin: 0 2em; clear:both; min-width: 600px"
          
    
## Here be data dragons

stdin = process.openStdin()
sys = require 'utils'
spawn = require('child_process').spawn

monitor_network_interface = (interface) ->
  # mad hax to get 2FPS instead of 1/2FPS
  # TODO make this programatic rather than hardcoded
  
  stats_a = spawn 'vnstat', ['-i', interface, '--live']
  setTimeout(->
    stats_b = spawn 'vnstat', ['-i', interface, '--live']
    setTimeout(->
      stats_c = spawn 'vnstat', ['-i', interface, '--live']
      setTimeout(->
        stats_d = spawn 'vnstat', ['-i', interface, '--live']
        
        monitor = ->
          data = (data) ->
            data = data.toString()
      
            data = data.replace /\s+/g, ' '
            data = data.replace /\t/g, ''
      
            data = data.trim()

            regex = XRegExp('rx: (?<rx>[\\d.]+) (?<ru>.)bit\/s (?<rp>[\\d.]+) p\/s tx: (?<tx>[\\d.]+) (?<tu>.)bit\/s (?<tp>[\\d.]+) p\/s')
      
            if regex.test data
              matches = regex.exec data
              console.log "#{matches.rx}#{matches.ru}:#{matches.tx}#{matches.tu}"
              rx = if matches.ru is 'k' then matches.rx else matches.rx * 1000
              tx = if matches.tu is 'k' then matches.tx else matches.tx * 1000
              registry.send 'network', {rx: rx, tx: tx} if registry.send

          error = (data) ->
            sys.print 'stderr: ' + data

          exit = (code) ->
            console.log 'child process exited with code ' + code

          stats_a.stdout.on 'data', data
          stats_a.on 'exit', exit
          stats_b.stdout.on 'data', data
          stats_b.on 'exit', exit
          stats_c.stdout.on 'data', data
          stats_c.on 'exit', exit
          stats_d.stdout.on 'data', data
          stats_d.on 'exit', exit
      
        setTimeout(monitor, 0)
      , 1500)
    , 1000)
  , 500)

monitor_network_interface 'en1'
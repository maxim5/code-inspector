class window.ConsoleApp extends Backbone.Router
  initialize: =>
    window.socket = io.connect();
    window.socket.on 'time_update', @time_update

  start: =>
    Backbone.history.start();
    @time_update client_time: new Date().getTime()

  routes:
    '':        'root'
    'breasts': 'breasts'
    'cheat':   'cheat'
    'display': 'display'
    'helm':    'helm'
    'minimap': 'minimap'
    'science': 'science'

  root: (callback=->) =>
    @view?.remove()
    @view = null

  breasts: (callback=->) =>
    @display_view new BreastsView

  cheat: (callback=->) =>
    window.cheat = true
    @display callback

  display: (callback=->) =>
    @display_view new DisplayView

  helm: (callback=->) =>
    @display_view new HelmView

  minimap: (callback=->) =>
    @display_view new MinimapView

  science: (callback=->) =>
    @display_view new ScienceView

  # Protecteed
  display_view: (view) =>
    @view?.remove()
    @view = view
    $('#application-view').html @view.render()
    @view.run()

  time_update: (data:client_time, data:server_time) =>
    client_time ?= 0
    server_time ?= 0

    previous_offset = _.clone window.time_offset ? 3000
    window.time_offset = (previous_offset + server_time - client_time) / 2

    _.delay =>
      window.socket.emit 'time_check', client_time: new Date().getTime()
    , 60000 - (Math.abs(previous_offset - window.time_offset) * 6000) # If off by more than 0.1 second, send another update immediatly

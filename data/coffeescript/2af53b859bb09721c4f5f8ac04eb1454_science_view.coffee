class window.ScienceView extends View
  template: JST['console/templates/science']
  className: 'science-view'

  initialize: =>
    window.socket.on 'world', (world_json) =>
      @mission = new Mission world_json.mission
      @render()
    window.socket.emit 'request_world'

  context: =>
    cid:     @cid
    mission: @mission?.toJSON()
    quantity_of_science: @quantity_of_science

  events:
    'keyup .quantity-of-science': 'change_quantity_of_science'
    'click .accept_mission':      'accept_mission'
    'click .beam_aboard':         'beam_aboard'
    'click .do_science':          'do_science'
    'click .long_range_scan':     'long_range_scan'
    'click .scan_planet':         'scan_planet'

  render: =>
    @$el.html @template @context()
    @$el

  # Event Handlers
  accept_mission: ($event) =>
    $event.preventDefault()
    return if $($event.currentTarget).hasClass 'disabled'
    window.socket.emit 'accept_mission'

  beam_aboard: ($event) =>
    $event.preventDefault()
    window.socket.emit 'beam_aboard'

  change_quantity_of_science: ($event) =>
    @quantity_of_science = @$('.quantity-of-science').val()

  do_science: ($event) =>
    $event.preventDefault()
    window.socket.emit 'do_science', @quantity_of_science

  long_range_scan: ($event) =>
    $event.preventDefault()
    window.socket.emit 'long_range_scan'

  scan_planet: ($event) =>
    $event.preventDefault()
    window.socket.emit 'scan_planet'


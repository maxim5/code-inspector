Backbone = window?.Backbone ? require 'backbone'
_        = window?._        ? require 'underscore'
{Blurbs} = window ? require './blurbs'

root = exports ? this
class root.Mission extends Backbone.Model
  defaults:
    stage:    0
    title:    null

  initialize: =>
    @blurbs = new Blurbs @get 'blurbs'
    @blurbs.on 'all', (event, args...) =>
      @trigger "blurbs:#{event}", args...

  toJSON: =>
    _.extend super,
      accepted: @is_accepted()
      blurbs: @blurbs.toJSON()

  # Instance Methods

  accept: =>
    return if @is_accepted()
    @blurbs.reset
      message: 'Cure diseases, go to a planet to collect a life form.'
      status:  ''
      type:    'Objective'
    , silent: true
    @set stage: 1, title: 'Got your baby.'

  beam_aboard: (body) =>
    if body? and @get('stage') == 1
      @set {stage: 2, timestamp: new Date().getAdjustedTime()}, silent: true
      @blurbs.add [
          message: 'Life form aboard'
          status:  'success'
          type:    'Teleport Result'
        ,
          message: 'Do science on the life form.'
          status:  ''
          type:    'Objective'
        ,
          message: "Oh no! We pissed off its Mom! We have 60 seconds to do science or we're dead!"
          status:  ''
          type:    'Objective'
        ]
      _.delay @update_mom, 30000
    else
      @blurbs.add
        message: 'Nothing to beam aboard.'
        status:  'error'
        type:    'Teleport Result'

  do_science: (quantity_of_science) =>
    quantity_of_science = parseInt quantity_of_science

    unless @get('stage') == 2
      return @blurbs.add
        message: 'Nothing to do science on.'
        status:  'error'
        type:    'Science Result'


    if quantity_of_science > 75
      @set {stage: -1}, silent: true
      @blurbs.add [
        message: 'Too much science was done, the life form died.'
        status:  'error'
        type:    'Science Result'
      ,
        message: 'You failed the mission.'
        status:  'error'
        type:    'Objective'
      ]
    else if quantity_of_science == 75
      @set {stage: -1}, silent: true
      @blurbs.add [
        message: 'Science has been done. For some reason the mother is now OK with us having her baby'
        status:  'success'
        type:    'Science Result'
      ,
        message: 'Mission Accomplished!'
        status:  'success'
        type:    'Objective'
      ]
    else
      @blurbs.add
        message: 'Nothing found. Increase science and try again.'
        status:  'error'
        type:    'Science Result'

  is_accepted: =>
    @get('stage') > 0

  long_range_scan: =>
    if @is_accepted()
      @blurbs.add
        message: 'Go to a planet.'
        status:  ''
        type:    'Long Range Scan Result'
    else
      @blurbs.add
        message: 'There is nothing out there.'
        status:  'error'
        type:    'Long Range Scan Result'

  scan: (body) =>
    @blurbs.add @scan_result body

  scan_result: (body) =>
    if body?
      switch @get 'stage'
        when 0
          message: 'No life signs detected.'
          status:  'error'
          type:    'Scan Result'
        else
          message: 'Life signs detected.'
          status:  'success'
          type:    'Scan Result'
    else
      message: 'No planets within range'
      status:  'error'
      type:    'Scan Result'

  update_mom: =>
    return unless @get('stage') == 2
    time_elapsed = parseInt((new Date().getAdjustedTime() - @get('timestamp')) / 1000)
    time_left = 60 - time_elapsed
    next_update = time_left * 500 # 1000 * time_left / 2
    _.delay @update_mom, next_update if time_left > 0

    if time_left > 0
      @blurbs.add
        message: "#{parseInt time_left} seconds left until the life form's mom catches us!"
        status:  ''
        type:    'Mission'
    else
      @set {stage: -1}, silent: true
      @blurbs.add [
        message: 'Mom caught us'
        status:  'error'
        type:    'Mission'
      ,
        message: 'You failed the mission.'
        status:  'error'
        type:    'Objective'
      ]


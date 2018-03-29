#
# batman.js
#
# Created by Nick Small
# Copyright 2011, Shopify
#

# The global namespace, the `Batman` function will also create also create a new
# instance of Batman.Object and mixin all arguments to it.
Batman = (mixins...) ->
  new Batman.Object mixins...

Batman.version = '0.8.0'

Batman.config =
  pathPrefix: '/'
  usePushState: no

Batman.container = if exports?
  module.exports = Batman
  global
else
  window.Batman = Batman
  window

# Global Helpers
# -------

# `$typeOf` returns a string that contains the built-in class of an object
# like `String`, `Array`, or `Object`. Note that only `Object` will be returned for
# the entire prototype chain.
Batman.typeOf = $typeOf = (object) ->
  return "Undefined" if typeof object == 'undefined'
  _objectToString.call(object).slice(8, -1)

# Cache this function to skip property lookups.
_objectToString = Object.prototype.toString

# `$mixin` applies every key from every argument after the first to the
# first argument. If a mixin has an `initialize` method, it will be called in
# the context of the `to` object, and it's key/values won't be applied.
Batman.mixin = $mixin = (to, mixins...) ->
  hasSet = typeof to.set is 'function'

  for mixin in mixins
    continue if $typeOf(mixin) isnt 'Object'

    for own key, value of mixin
      continue if key in  ['initialize', 'uninitialize', 'prototype']
      if hasSet
        to.set(key, value)
      else if to.nodeName?
        Batman.data to, key, value
      else
        to[key] = value

    if typeof mixin.initialize is 'function'
      mixin.initialize.call to

  to

# `$unmixin` removes every key/value from every argument after the first
# from the first argument. If a mixin has a `deinitialize` method, it will be
# called in the context of the `from` object and won't be removed.
Batman.unmixin = $unmixin = (from, mixins...) ->
  for mixin in mixins
    for key of mixin
      continue if key in ['initialize', 'uninitialize']

      delete from[key]

    if typeof mixin.uninitialize is 'function'
      mixin.uninitialize.call from

  from

# `$functionName` returns the name of a given function, if any
# Used to deal with functions not having the `name` property in IE
Batman._functionName = $functionName = (f) ->
  return f.__name__ if f.__name__
  return f.name if f.name
  f.toString().match(/\W*function\s+([\w\$]+)\(/)?[1]

# `$preventDefault` checks for preventDefault, since it's not
# always available across all browsers
Batman._preventDefault = $preventDefault = (e) ->
  if typeof e.preventDefault is "function" then e.preventDefault() else e.returnValue = false

Batman._isChildOf = $isChildOf = (parentNode, childNode) ->
  node = childNode.parentNode
  while node
    return true if node == parentNode
    node = node.parentNode
  false

$setImmediate = $clearImmediate = null
_implementImmediates = (container) ->
  canUsePostMessage = ->
    return false unless container.postMessage
    async = true
    oldMessage = container.onmessage
    container.onmessage = -> async = false
    container.postMessage("","*")
    container.onmessage = oldMessage
    async

  tasks = new Batman.SimpleHash
  count = 0
  getHandle = -> "go#{++count}"

  if container.setImmediate
    $setImmediate = container.setImmediate
    $clearImmediate = container.clearImmediate
  else if container.msSetImmediate
    $setImmediate = msSetImmediate
    $clearImmediate = msClearImmediate
  else if canUsePostMessage()
    prefix = 'com.batman.'
    functions = new Batman.SimpleHash
    handler = (e) ->
      return unless ~e.data.search(prefix)
      handle = e.data.substring(prefix.length)
      tasks.unset(handle)?()

    if container.addEventListener
      container.addEventListener('message', handler, false)
    else
      container.attachEvent('onmessage', handler)

    $setImmediate = (f) ->
      tasks.set(handle = getHandle(), f)
      container.postMessage(prefix+handle, "*")
      handle
    $clearImmediate = (handle) -> tasks.unset(handle)
  else if typeof document isnt 'undefined' && "onreadystatechange" in document.createElement("script")
    $setImmediate = (f) ->
      handle = getHandle()
      script = document.createElement("script")
      script.onreadystatechange = ->
        tasks.get(handle)?()
        script.onreadystatechange = null
        script.parentNode.removeChild(script)
        script = null
      document.documentElement.appendChild(script)
      handle
    $clearImmediate = (handle) -> tasks.unset(handle)
  else
    $setImmediate = (f) -> setTimeout(f, 0)
    $clearImmediate = (handle) -> clearTimeout(handle)

  Batman.setImmediate = $setImmediate
  Batman.clearImmediate = $clearImmediate

Batman.setImmediate = $setImmediate = ->
  _implementImmediates(Batman.container)
  Batman.setImmediate.apply(@, arguments)

Batman.clearImmediate = $clearImmediate = ->
  _implementImmediates(Batman.container)
  Batman.clearImmediate.apply(@, arguments)

Batman.forEach = $forEach = (container, iterator, ctx) ->
  if container.forEach
    container.forEach(iterator, ctx)
  else if container.indexOf
    iterator.call(ctx, e, i, container) for e,i in container
  else
    iterator.call(ctx, k, v, container) for k,v of container

Batman.objectHasKey = $objectHasKey = (object, key) ->
  if typeof object.hasKey is 'function'
    object.hasKey(key)
  else
    key of object

Batman.contains = $contains = (container, item) ->
  if container.indexOf
    item in container
  else if typeof container.has is 'function'
    container.has(item)
  else
    $objectHasKey(container, item)

Batman.get = $get = (base, key) ->
  if typeof base.get is 'function'
    base.get(key)
  else
    Batman.Property.forBaseAndKey(base, key).getValue()

Batman.escapeHTML = $escapeHTML = do ->
  replacements =
    "&": "&amp;"
    "<": "&lt;"
    ">": "&gt;"
    "\"": "&#34;"
    "'": "&#39;"

  return (s) -> (""+s).replace(/[&<>'"]/g, (c) -> replacements[c])

# `translate` is hook for the i18n extra to override and implemnent. All strings which might
# be shown to the user pass through this method. `translate` is aliased to `t` internally.
Batman.translate = (x, values = {}) -> helpers.interpolate($get(Batman.translate.messages, x), values)
Batman.translate.messages = {}
t = -> Batman.translate(arguments...)

# Developer Tooling
# -----------------

Batman.developer =
  suppressed: false
  DevelopmentError: (->
    DevelopmentError = (@message) ->
      @name = "DevelopmentError"
    DevelopmentError:: = Error::
    DevelopmentError
  )()
  _ie_console: (f, args) ->
    console?[f] "...#{f} of #{args.length} items..." unless args.length == 1
    console?[f] arg for arg in args
  suppress: (f) ->
    developer.suppressed = true
    if f
      f()
      developer.suppressed = false
  unsuppress: ->
    developer.suppressed = false
  log: ->
    return if developer.suppressed or !(console?.log?)
    if console.log.apply then console.log(arguments...) else developer._ie_console "log", arguments
  warn: ->
    return if developer.suppressed or !(console?.warn?)
    if console.warn.apply then console.warn(arguments...) else developer._ie_console "warn", arguments
  error: (message) -> throw new developer.DevelopmentError(message)
  assert: (result, message) -> developer.error(message) unless result
  do: (f) -> f() unless developer.suppressed
  addFilters: ->
    $mixin Batman.Filters,
      log: (value, key) ->
        console?.log? arguments
        value

      logStack: (value) ->
        console?.log? developer.currentFilterStack
        value

developer = Batman.developer
developer.assert (->).bind, "Error! Batman needs Function.bind to work! Please shim it using something like es5-shim or augmentjs!"

# Helpers
# -------

# Just a few random Rails-style string helpers. You can add more
# to the Batman.helpers object.

class Batman.Inflector
  plural: []
  singular: []
  uncountable: []

  @plural: (regex, replacement) -> @::plural.unshift [regex, replacement]
  @singular: (regex, replacement) -> @::singular.unshift [regex, replacement]
  @irregular: (singular, plural) ->
    if singular.charAt(0) == plural.charAt(0)
      @plural new RegExp("(#{singular.charAt(0)})#{singular.slice(1)}$", "i"), "$1" + plural.slice(1)
      @plural new RegExp("(#{singular.charAt(0)})#{plural.slice(1)}$", "i"), "$1" + plural.slice(1)
      @singular new RegExp("(#{plural.charAt(0)})#{plural.slice(1)}$", "i"), "$1" + singular.slice(1)
    else
      @plural new RegExp("#{singular}$", 'i'), plural
      @plural new RegExp("#{plural}$", 'i'), plural
      @singular new RegExp("#{plural}$", 'i'), singular

  @uncountable: (strings...) -> @::uncountable = @::uncountable.concat(strings.map((x) -> new RegExp("#{x}$", 'i')))

  @plural(/$/, 's')
  @plural(/s$/i, 's')
  @plural(/(ax|test)is$/i, '$1es')
  @plural(/(octop|vir)us$/i, '$1i')
  @plural(/(octop|vir)i$/i, '$1i')
  @plural(/(alias|status)$/i, '$1es')
  @plural(/(bu)s$/i, '$1ses')
  @plural(/(buffal|tomat)o$/i, '$1oes')
  @plural(/([ti])um$/i, '$1a')
  @plural(/([ti])a$/i, '$1a')
  @plural(/sis$/i, 'ses')
  @plural(/(?:([^f])fe|([lr])f)$/i, '$1$2ves')
  @plural(/(hive)$/i, '$1s')
  @plural(/([^aeiouy]|qu)y$/i, '$1ies')
  @plural(/(x|ch|ss|sh)$/i, '$1es')
  @plural(/(matr|vert|ind)(?:ix|ex)$/i, '$1ices')
  @plural(/([m|l])ouse$/i, '$1ice')
  @plural(/([m|l])ice$/i, '$1ice')
  @plural(/^(ox)$/i, '$1en')
  @plural(/^(oxen)$/i, '$1')
  @plural(/(quiz)$/i, '$1zes')

  @singular(/s$/i, '')
  @singular(/(n)ews$/i, '$1ews')
  @singular(/([ti])a$/i, '$1um')
  @singular(/((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)ses$/i, '$1$2sis')
  @singular(/(^analy)ses$/i, '$1sis')
  @singular(/([^f])ves$/i, '$1fe')
  @singular(/(hive)s$/i, '$1')
  @singular(/(tive)s$/i, '$1')
  @singular(/([lr])ves$/i, '$1f')
  @singular(/([^aeiouy]|qu)ies$/i, '$1y')
  @singular(/(s)eries$/i, '$1eries')
  @singular(/(m)ovies$/i, '$1ovie')
  @singular(/(x|ch|ss|sh)es$/i, '$1')
  @singular(/([m|l])ice$/i, '$1ouse')
  @singular(/(bus)es$/i, '$1')
  @singular(/(o)es$/i, '$1')
  @singular(/(shoe)s$/i, '$1')
  @singular(/(cris|ax|test)es$/i, '$1is')
  @singular(/(octop|vir)i$/i, '$1us')
  @singular(/(alias|status)es$/i, '$1')
  @singular(/^(ox)en/i, '$1')
  @singular(/(vert|ind)ices$/i, '$1ex')
  @singular(/(matr)ices$/i, '$1ix')
  @singular(/(quiz)zes$/i, '$1')
  @singular(/(database)s$/i, '$1')

  @irregular('person', 'people')
  @irregular('man', 'men')
  @irregular('child', 'children')
  @irregular('sex', 'sexes')
  @irregular('move', 'moves')
  @irregular('cow', 'kine')
  @irregular('zombie', 'zombies')

  @uncountable('equipment', 'information', 'rice', 'money', 'species', 'series', 'fish', 'sheep', 'jeans')

  ordinalize: (number) ->
    absNumber = Math.abs(parseInt(number))
    if absNumber % 100 in [11..13]
      number + "th"
    else
      switch absNumber % 10
        when 1
          number + "st"
        when 2
          number + "nd"
        when 3
          number + "rd"
        else
          number + "th"

  pluralize: (word) ->
    for uncountableRegex in @uncountable
      return word if uncountableRegex.test(word)
    for [regex, replace_string] in @plural
      return word.replace(regex, replace_string) if regex.test(word)
    word

  singularize: (word) ->
    for uncountableRegex in @uncountable
      return word if uncountableRegex.test(word)
    for [regex, replace_string] in @singular
      return word.replace(regex, replace_string)  if regex.test(word)
    word

camelize_rx = /(?:^|_|\-)(.)/g
capitalize_rx = /(^|\s)([a-z])/g
underscore_rx1 = /([A-Z]+)([A-Z][a-z])/g
underscore_rx2 = /([a-z\d])([A-Z])/g

helpers = Batman.helpers =
  inflector: new Batman.Inflector
  ordinalize: -> helpers.inflector.ordinalize.apply helpers.inflector, arguments
  singularize: -> helpers.inflector.singularize.apply helpers.inflector, arguments
  pluralize: (count, singular, plural) ->
    if arguments.length < 2
      helpers.inflector.pluralize count
    else
      "#{count || 0} " + if +count is 1 then singular else (plural || helpers.inflector.pluralize(singular))

  camelize: (string, firstLetterLower) ->
    string = string.replace camelize_rx, (str, p1) -> p1.toUpperCase()
    if firstLetterLower then string.substr(0,1).toLowerCase() + string.substr(1) else string

  underscore: (string) ->
    string.replace(underscore_rx1, '$1_$2')
          .replace(underscore_rx2, '$1_$2')
          .replace('-', '_').toLowerCase()

  capitalize: (string) -> string.replace capitalize_rx, (m,p1,p2) -> p1 + p2.toUpperCase()

  trim: (string) -> if string then string.trim() else ""

  interpolate: (stringOrObject, keys) ->
    if typeof stringOrObject is 'object'
      string = stringOrObject[keys.count]
      unless string
        string = stringOrObject['other']
    else
      string = stringOrObject

    for key, value of keys
      string = string.replace(new RegExp("%\\{#{key}\\}", "g"), value)
    string

class Batman.Event
  @forBaseAndKey: (base, key) ->
    if base.isEventEmitter
      base.event(key)
    else
      new Batman.Event(base, key)
  constructor: (@base, @key) ->
    @handlers = new Batman.SimpleSet
    @_preventCount = 0
  isEvent: true
  isEqual: (other) ->
    @constructor is other.constructor and @base is other.base and @key is other.key
  hashKey: ->
    @hashKey = -> key
    key = "<Batman.Event base: #{Batman.Hash::hashKeyFor(@base)}, key: \"#{Batman.Hash::hashKeyFor(@key)}\">"

  addHandler: (handler) ->
    @handlers.add(handler)
    @autofireHandler(handler) if @oneShot
    this
  removeHandler: (handler) ->
    @handlers.remove(handler)
    this

  eachHandler: (iterator) ->
    @handlers.forEach(iterator)
    if @base?.isEventEmitter
      key = @key
      @base._batman.ancestors (ancestor) ->
        if ancestor.isEventEmitter and ancestor.hasEvent(key)
          handlers = ancestor.event(key).handlers
          handlers.forEach(iterator)

  handlerContext: -> @base

  prevent: -> ++@_preventCount
  allow: ->
    --@_preventCount if @_preventCount
    @_preventCount
  isPrevented: -> @_preventCount > 0
  autofireHandler: (handler) ->
    if @_oneShotFired and @_oneShotArgs?
      handler.apply(@handlerContext(), @_oneShotArgs)
  resetOneShot: ->
    @_oneShotFired = false
    @_oneShotArgs = null
  fire: ->
    return false if @isPrevented() or @_oneShotFired
    context = @handlerContext()
    args = arguments
    if @oneShot
      @_oneShotFired = true
      @_oneShotArgs = arguments
    @eachHandler (handler) -> handler.apply(context, args)
  allowAndFire: ->
    @allow()
    @fire(arguments...)

Batman.EventEmitter =
  isEventEmitter: true
  hasEvent: (key) ->
    @_batman?.get?('events')?.hasKey(key)
  event: (key) ->
    Batman.initializeObject @
    eventClass = @eventClass or Batman.Event
    events = @_batman.events ||= new Batman.SimpleHash
    if events.hasKey(key)
      existingEvent = events.get(key)
    else
      existingEvents = @_batman.get('events')
      newEvent = events.set(key, new eventClass(this, key))
      newEvent.oneShot = existingEvents?.get(key)?.oneShot
      newEvent
  on: (key, handler) ->
    @event(key).addHandler(handler)
  registerAsMutableSource: ->
    Batman.Property.registerSource(@)
  mutation: (wrappedFunction) ->
    ->
      result = wrappedFunction.apply(this, arguments)
      @event('change').fire(this, this)
      result
  prevent: (key) ->
    @event(key).prevent()
    @
  allow: (key) ->
    @event(key).allow()
    @
  isPrevented: (key) ->
    @event(key).isPrevented()
  fire: (key, args...) ->
    @event(key).fire(args...)
  allowAndFire: (key, args...) ->
    @event(key).allowAndFire(args...)

class Batman.PropertyEvent extends Batman.Event
  eachHandler: (iterator) -> @base.eachObserver(iterator)
  handlerContext: -> @base.base

class Batman.Property
  $mixin @prototype, Batman.EventEmitter

  @_sourceTrackerStack: []
  @sourceTracker: -> (stack = @_sourceTrackerStack)[stack.length - 1]
  @defaultAccessor:
    get: (key) -> @[key]
    set: (key, val) -> @[key] = val
    unset: (key) -> x = @[key]; delete @[key]; x
    cachable: no
  @forBaseAndKey: (base, key) ->
    if base.isObservable
      base.property(key)
    else
      new Batman.Keypath(base, key)
  @withoutTracking: (block) ->
    @pushDummySourceTracker()
    try
      block()
    finally
      @popSourceTracker()
  @registerSource: (obj) ->
    return unless obj.isEventEmitter
    @sourceTracker()?.add(obj)

  @pushSourceTracker: -> Batman.Property._sourceTrackerStack.push(new Batman.SimpleSet)
  @pushDummySourceTracker: -> Batman.Property._sourceTrackerStack.push(null)
  @popSourceTracker: -> Batman.Property._sourceTrackerStack.pop()

  constructor: (@base, @key) ->
    developer.do =>
      keyType = $typeOf(@key)
      if keyType in ['Array', 'Object']
        developer.log "Accessing a property with an #{keyType} key. This is okay, but could be a source of memory leaks if you aren't careful."

  _isolationCount: 0
  cached: no
  value: null
  sources: null
  isProperty: true
  isDead: false
  eventClass: Batman.PropertyEvent

  isEqual: (other) ->
    @constructor is other.constructor and @base is other.base and @key is other.key
  hashKey: ->
    @hashKey = -> key
    key = "<Batman.Property base: #{Batman.Hash::hashKeyFor(@base)}, key: \"#{Batman.Hash::hashKeyFor(@key)}\">"

  changeEvent: ->
    event = @event('change')
    @changeEvent = -> event
    event
  accessor: ->
    keyAccessors = @base._batman?.get('keyAccessors')
    accessor = if keyAccessors && (val = keyAccessors.get(@key))
      val
    else
      @base._batman?.getFirst('defaultAccessor') or Batman.Property.defaultAccessor
    @accessor = -> accessor
    accessor
  eachObserver: (iterator) ->
    key = @key
    @changeEvent().handlers.forEach(iterator)
    if @base.isObservable
      @base._batman.ancestors (ancestor) ->
        if ancestor.isObservable and ancestor.hasProperty(key)
          property = ancestor.property(key)
          handlers = property.changeEvent().handlers
          handlers.forEach(iterator)
  observers: ->
    results = []
    @eachObserver (observer) -> results.push(observer)
    results
  hasObservers: -> @observers().length > 0

  updateSourcesFromTracker: ->
    newSources = @constructor.popSourceTracker()
    handler = @sourceChangeHandler()
    @_eachSourceChangeEvent (e) -> e.removeHandler(handler)
    @sources = newSources
    @_eachSourceChangeEvent (e) -> e.addHandler(handler)

  _eachSourceChangeEvent: (iterator) ->
    return unless @sources?
    @sources.forEach (source) -> iterator(source.event('change'))

  getValue: ->
    @registerAsMutableSource()
    unless @isCached()
      @constructor.pushSourceTracker()
      try
        @value = @valueFromAccessor()
        @cached = yes
      finally
        @updateSourcesFromTracker()
    @value

  isCachable: ->
    return true if @isFinal()
    cachable = @accessor().cachable
    if cachable? then !!cachable else true

  isCached: -> @isCachable() and @cached

  isFinal: -> !!@accessor()['final']

  refresh: ->
    @cached = no
    previousValue = @value
    value = @getValue()
    if value isnt previousValue and not @isIsolated()
      @fire(value, previousValue)
    @lockValue() if @value isnt undefined and @isFinal()

  sourceChangeHandler: ->
    handler = => @_handleSourceChange()
    @sourceChangeHandler = -> handler
    handler

  _handleSourceChange: ->
    if @isIsolated()
      @_needsRefresh = yes
    else if not @isFinal() && not @hasObservers()
      @cached = no
    else
      @refresh()

  valueFromAccessor: -> @accessor().get?.call(@base, @key)

  setValue: (val) ->
    return unless set = @accessor().set
    @_changeValue -> set.call(@base, @key, val)
  unsetValue: ->
    return unless unset = @accessor().unset
    @_changeValue -> unset.call(@base, @key)

  _changeValue: (block) ->
    @cached = no
    @constructor.pushDummySourceTracker()
    try
      result = block.apply(this)
      @refresh()
    finally
      @constructor.popSourceTracker()
    @die() unless @isCached() or @hasObservers()
    result

  forget: (handler) ->
    if handler?
      @changeEvent().removeHandler(handler)
    else
      @changeEvent().handlers.clear()
  observeAndFire: (handler) ->
    @observe(handler)
    handler.call(@base, @value, @value)
  observe: (handler) ->
    @changeEvent().addHandler(handler)
    @getValue() unless @sources?
    this

  _removeHandlers: ->
    handler = @sourceChangeHandler()
    @_eachSourceChangeEvent (e) -> e.removeHandler(handler)
    delete @sources
    @changeEvent().handlers.clear()

  lockValue: ->
    @_removeHandlers()
    @getValue = -> @value
    @setValue = @unsetValue = @refresh = @observe = ->

  die: ->
    @_removeHandlers()
    @base._batman?.properties?.unset(@key)
    @isDead = true

  fire: -> @changeEvent().fire(arguments...)

  isolate: ->
    if @_isolationCount is 0
      @_preIsolationValue = @getValue()
    @_isolationCount++
  expose: ->
    if @_isolationCount is 1
      @_isolationCount--
      if @_needsRefresh
        @value = @_preIsolationValue
        @refresh()
      else if @value isnt @_preIsolationValue
        @fire(@value, @_preIsolationValue)
      @_preIsolationValue = null
    else if @_isolationCount > 0
      @_isolationCount--
  isIsolated: -> @_isolationCount > 0

# Keypaths
# --------

class Batman.Keypath extends Batman.Property
  constructor: (base, key) ->
    if $typeOf(key) is 'String'
      @segments = key.split('.')
      @depth = @segments.length
    else
      @segments = [key]
      @depth = 1
    super
  slice: (begin, end=@depth) ->
    base = @base
    for segment in @segments.slice(0, begin)
      return unless base? and base = $get(base, segment)
    propertyClass = base.propertyClass or Batman.Keypath
    remainingSegments = @segments.slice(begin, end)
    remainingPath = remainingSegments.join('.')
    if propertyClass is Batman.Keypath or remainingSegments.length is 1
      Batman.Keypath.forBaseAndKey(base, remainingPath)
    else
      new Batman.Keypath(base, remainingPath)
  terminalProperty: -> @slice -1
  valueFromAccessor: ->
    if @depth is 1 then super else @terminalProperty()?.getValue()
  setValue: (val) -> if @depth is 1 then super else @terminalProperty()?.setValue(val)
  unsetValue: -> if @depth is 1 then super else @terminalProperty()?.unsetValue()



# Observable
# ----------

# Batman.Observable is a generic mixin that can be applied to any object to allow it to be bound to.
# It is applied by default to every instance of `Batman.Object` and subclasses.
Batman.Observable =
  isObservable: true
  hasProperty: (key) ->
    @_batman?.properties?.hasKey?(key)
  property: (key) ->
    Batman.initializeObject @
    propertyClass = @propertyClass or Batman.Keypath
    properties = @_batman.properties ||= new Batman.SimpleHash
    properties.get(key) or properties.set(key, new propertyClass(this, key))
  get: (key) ->
    @property(key).getValue()
  set: (key, val) ->
    @property(key).setValue(val)
  unset: (key) ->
    @property(key).unsetValue()

  getOrSet: (key, valueFunction) ->
    currentValue = @get(key)
    unless currentValue
      currentValue = valueFunction()
      @set(key, currentValue)
    currentValue

  # `forget` removes an observer from an object. If the callback is passed in,
  # its removed. If no callback but a key is passed in, all the observers on
  # that key are removed. If no key is passed in, all observers are removed.
  forget: (key, observer) ->
    if key
      @property(key).forget(observer)
    else
      @_batman.properties?.forEach (key, property) -> property.forget()
    @

# `fire` tells any observers attached to a key to fire, manually.
# `prevent` stops of a given binding from firing. `prevent` calls can be repeated such that
# the same number of calls to allow are needed before observers can be fired.
# `allow` unblocks a property for firing observers. Every call to prevent
# must have a matching call to allow later if observers are to be fired.
# `observe` takes a key and a callback. Whenever the value for that key changes, your
# callback will be called in the context of the original object.

  observe: (key, args...) ->
    @property(key).observe(args...)
    @

  observeAndFire: (key, args...) ->
    @property(key).observeAndFire(args...)
    @

# Objects
# -------

# `Batman.initializeObject` is called by all the methods in Batman.Object to ensure that the
# object's `_batman` property is initialized and it's own. Classes extending Batman.Object inherit
# methods like `get`, `set`, and `observe` by default on the class and prototype levels, such that
# both instances and the class respond to them and can be bound to. However, CoffeeScript's static
# class inheritance copies over all class level properties indiscriminately, so a parent class'
# `_batman` object will get copied to its subclasses, transferring all the information stored there and
# allowing subclasses to mutate parent state. This method prevents this undesirable behaviour by tracking
# which object the `_batman_` object was initialized upon, and reinitializing if that has changed since
# initialization.
Batman.initializeObject = (object) ->
  if object._batman?
    object._batman.check(object)
  else
    object._batman = new _Batman(object)

# _Batman provides a convienient, parent class and prototype aware place to store hidden
# object state. Things like observers, accessors, and states belong in the `_batman` object
# attached to every Batman.Object subclass and subclass instance.
Batman._Batman = class _Batman
  constructor: (@object, mixins...) ->
    $mixin(@, mixins...) if mixins.length > 0

  # Used by `Batman.initializeObject` to ensure that this `_batman` was created referencing
  # the object it is pointing to.
  check: (object) ->
    if object != @object
      object._batman = new _Batman(object)
      return false
    return true

  # `get` is a prototype and class aware property access method. `get` will traverse the prototype chain, asking
  # for the passed key at each step, and then attempting to merge the results into one object.
  # It can only do this if at each level an `Array`, `Hash`, or `Set` is found, so try to use
  # those if you need `_batman` inhertiance.
  get: (key) ->
    # Get all the keys from the ancestor chain
    results = @getAll(key)
    switch results.length
      when 0
        undefined
      when 1
        results[0]
      else
        # And then try to merge them if there is more than one. Use `concat` on arrays, and `merge` on
        # sets and hashes.
        if results[0].concat?
          results = results.reduceRight (a, b) -> a.concat(b)
        else if results[0].merge?
          results = results.reduceRight (a, b) -> a.merge(b)
        results

  # `getFirst` is a prototype and class aware property access method. `getFirst` traverses the prototype chain,
  # and returns the value of the first `_batman` object which defines the passed key. Useful for
  # times when the merged value doesn't make sense or the value is a primitive.
  getFirst: (key) ->
    results = @getAll(key)
    results[0]

  # `getAll` is a prototype and class chain iterator. When passed a key or function, it applies it to each
  # parent class or parent prototype, and returns the undefined values, closest ancestor first.
  getAll: (keyOrGetter) ->
    # Get a function which pulls out the key from the ancestor's `_batman` or use the passed function.
    if typeof keyOrGetter is 'function'
      getter = keyOrGetter
    else
      getter = (ancestor) -> ancestor._batman?[keyOrGetter]

    # Apply it to all the ancestors, and then this `_batman`'s object.
    results = @ancestors(getter)
    if val = getter(@object)
      results.unshift val
    results

  # `ancestors` traverses the prototype or class chain and returns the application of a function to each
  # object in the chain. `ancestors` does this _only_ to the `@object`'s ancestors, and not the `@object`
  # itsself.
  ancestors: (getter = (x) -> x) ->
    results = []
    # Decide if the object is a class or not, and pull out the first ancestor
    isClass = !!@object.prototype
    parent = if isClass
      @object.__super__?.constructor
    else
      if (proto = Object.getPrototypeOf(@object)) == @object
        @object.constructor.__super__
      else
        proto

    if parent?
      parent._batman?.check(parent)
      # Apply the function and store the result if it isn't undefined.
      val = getter(parent)
      results.push(val) if val?

      # Use a recursive call to `_batman.ancestors` on the ancestor, which will take the next step up the chain.
      if parent._batman?
        results = results.concat(parent._batman.ancestors(getter))
    results

  set: (key, value) ->
    @[key] = value

# `Batman.Object` is the base class for all other Batman objects. It is not abstract.
class BatmanObject extends Object
  Batman.initializeObject(this)
  Batman.initializeObject(@prototype)
  # Setting `isGlobal` to true will cause the class name to be defined on the
  # global object. For example, Batman.Model will be aliased to window.Model.
  # This should be used sparingly; it's mostly useful for debugging.
  @global: (isGlobal) ->
    return if isGlobal is false
    Batman.container[$functionName(@)] = @

  # Apply mixins to this class.
  @classMixin: -> $mixin @, arguments...

  # Apply mixins to instances of this class.
  @mixin: -> @classMixin.apply @prototype, arguments
  mixin: @classMixin

  counter = 0
  _objectID: ->
    @_objectID = -> c
    c = counter++

  hashKey: ->
    return if typeof @isEqual is 'function'
    @hashKey = -> key
    key = "<Batman.Object #{@_objectID()}>"

  toJSON: ->
    obj = {}
    for own key, value of @ when key not in ["_batman", "hashKey", "_objectID"]
      obj[key] = if value?.toJSON then value.toJSON() else value
    obj


  # Accessor implementation. Accessors are used to create properties on a class or prototype which can be fetched
  # with get, but are computed instead of just stored. This is a batman and old browser friendly version of
  # `defineProperty` without as much goodness.
  #
  # Accessors track which other properties they rely on for computation, and when those other properties change,
  # an accessor will recalculate its value and notifiy its observers. This way, when a source value is changed,
  # any dependent accessors will automatically update any bindings to them with a new value. Accessors accomplish
  # this feat by tracking `get` calls, do be sure to use `get` to retrieve properties inside accessors.
  #
  # `@accessor` or `@classAccessor` can be called with zero, one, or many keys to attach the accessor to. This
  # has the following effects:
  #
  #   * zero: create a `defaultAccessor`, which will be called when no other properties or accessors on an object
  #   match a keypath. This is similar to `method_missing` in Ruby or `#doesNotUnderstand` in Smalltalk.
  #   * one: create a `keyAccessor` at the given key, which will only be called when that key is `get`ed.
  #   * many: create `keyAccessors` for each given key, which will then be called whenever each key is `get`ed.
  #
  # Note: This function gets called in all sorts of different contexts by various
  # other pointers to it, but it acts the same way on `this` in all cases.
  getAccessorObject = (accessor) ->
    accessor = {get: accessor} if !accessor.get && !accessor.set && !accessor.unset
    accessor

  @classAccessor: (keys..., accessor) ->
    Batman.initializeObject @
    # Create a default accessor if no keys have been given.
    if keys.length is 0
      # The `accessor` argument is wrapped in `getAccessorObject` which allows functions to be passed in
      # as a shortcut to {get: function}
      @_batman.defaultAccessor = getAccessorObject(accessor)
    else
      # Otherwise, add key accessors for each key given.
      @_batman.keyAccessors ||= new Batman.SimpleHash
      @_batman.keyAccessors.set(key, getAccessorObject(accessor)) for key in keys

  # Support adding accessors to the prototype from within class defintions or after the class has been created
  # with `KlassExtendingBatmanObject.accessor(keys..., accessorObject)`
  @accessor: -> @classAccessor.apply @prototype, arguments
  # Support adding accessors to instances after creation
  accessor: @classAccessor

  constructor: (mixins...) ->
    @_batman = new _Batman(@)
    @mixin mixins...


  # Make every subclass and their instances observable.
  @classMixin Batman.EventEmitter, Batman.Observable
  @mixin Batman.EventEmitter, Batman.Observable

  # Observe this property on every instance of this class.
  @observeAll: -> @::observe.apply @prototype, arguments

  @singleton: (singletonMethodName="sharedInstance") ->
    @classAccessor singletonMethodName,
      get: -> @["_#{singletonMethodName}"] ||= new @

Batman.Object = BatmanObject

class Batman.Accessible extends Batman.Object
  constructor: -> @accessor.apply(@, arguments)

class Batman.TerminalAccessible extends Batman.Accessible
  propertyClass: Batman.Property

# Collections

Batman.Enumerable =
  isEnumerable: true
  map:   (f, ctx = Batman.container) -> r = []; @forEach(-> r.push f.apply(ctx, arguments)); r
  mapToProperty: (key) -> r = []; @forEach((item) -> r.push item.get(key)); r
  every: (f, ctx = Batman.container) -> r = true; @forEach(-> r = r && f.apply(ctx, arguments)); r
  some:  (f, ctx = Batman.container) -> r = false; @forEach(-> r = r || f.apply(ctx, arguments)); r
  reduce: (f, r) ->
    count = 0
    self = @
    @forEach -> if r? then r = f(r, arguments..., count, self) else r = arguments[0]
    r
  filter: (f) ->
    r = new @constructor
    if r.add
      wrap = (r, e) -> r.add(e) if f(e); r
    else if r.set
      wrap = (r, k, v) -> r.set(k, v) if f(k, v); r
    else
      r = [] unless r.push
      wrap = (r, e) -> r.push(e) if f(e); r
    @reduce wrap, r

# Provide this simple mixin ability so that during bootstrapping we don't have to use `$mixin`. `$mixin`
# will correctly attempt to use `set` on the mixinee, which ends up requiring the definition of
# `SimpleSet` to be complete during its definition.
$extendsEnumerable = (onto) -> onto[k] = v for k,v of Batman.Enumerable

class Batman.SimpleHash
  constructor: (obj) ->
    @_storage = {}
    @length = 0
    @update(obj) if obj?
  $extendsEnumerable(@::)
  propertyClass: Batman.Property
  hasKey: (key) ->
    if pairs = @_storage[@hashKeyFor(key)]
      for pair in pairs
        return true if @equality(pair[0], key)
    return false
  get: (key) ->
    if pairs = @_storage[@hashKeyFor(key)]
      for pair in pairs
        return pair[1] if @equality(pair[0], key)
  set: (key, val) ->
    pairs = @_storage[@hashKeyFor(key)] ||= []
    for pair in pairs
      if @equality(pair[0], key)
        return pair[1] = val
    @length++
    pairs.push([key, val])
    val
  unset: (key) ->
    hashKey = @hashKeyFor(key)
    if pairs = @_storage[hashKey]
      for [obj,value], index in pairs
        if @equality(obj, key)
          pair = pairs.splice(index,1)
          delete @_storage[hashKey] unless pairs.length
          @length--
          return pair[0][1]
  getOrSet: Batman.Observable.getOrSet
  hashKeyFor: (obj) -> obj?.hashKey?() or obj
  equality: (lhs, rhs) ->
    return true if lhs is rhs
    return true if lhs isnt lhs and rhs isnt rhs # when both are NaN
    return true if lhs?.isEqual?(rhs) and rhs?.isEqual?(lhs)
    return false
  forEach: (iterator, ctx) ->
    for key, values of @_storage
      iterator.call(ctx, obj, value, this) for [obj, value] in values.slice()
  keys: ->
    result = []
    # Explicitly reference this foreach so that if it's overriden in subclasses the new implementation isn't used.
    Batman.SimpleHash::forEach.call @, (key) -> result.push key
    result
  clear: ->
    @_storage = {}
    @length = 0
  isEmpty: ->
    @length is 0
  merge: (others...) ->
    merged = new @constructor
    others.unshift(@)
    for hash in others
      hash.forEach (obj, value) ->
        merged.set obj, value
    merged
  update: (object) -> @set(k,v) for k,v of object
  replace: (object) ->
    @forEach (key, value) =>
      @unset(key) unless key of object
    @update(object)
  toObject: ->
    obj = {}
    for key, pair of @_storage
      obj[key] = pair[0][1] # the first value for this key
    obj
  toJSON: @::toObject

class Batman.Hash extends Batman.Object
  class @Metadata extends Batman.Object
    constructor: (@hash) ->
    @accessor 'length', ->
      @hash.registerAsMutableSource()
      @hash.length
    @accessor 'isEmpty', -> @hash.isEmpty()
    @accessor 'keys', -> @hash.keys()

  constructor: ->
    @meta = new @constructor.Metadata(this)
    Batman.SimpleHash.apply(@, arguments)
    super

  $extendsEnumerable(@::)
  propertyClass: Batman.Property

  @accessor
    get: Batman.SimpleHash::get
    set: @mutation (key, value) ->
      result = Batman.SimpleHash::set.call(@, key, value)
      @fire 'itemsWereAdded', key
      result
    unset: @mutation (key) ->
      result = Batman.SimpleHash::unset.call(@, key)
      @fire 'itemsWereRemoved', key if result?
      result
    cachable: false

  _preventMutationEvents: (block) ->
    @prevent 'change'
    @prevent 'itemsWereAdded'
    @prevent 'itemsWereRemoved'
    try
      block.call(this)
    finally
      @allow 'change'
      @allow 'itemsWereAdded'
      @allow 'itemsWereRemoved'
  clear: @mutation ->
    keys = @keys()
    @_preventMutationEvents -> @forEach (k) => @unset(k)
    result = Batman.SimpleHash::clear.call(@)
    @fire 'itemsWereRemoved', keys...
    result
  update: @mutation (object) ->
    addedKeys = []
    @_preventMutationEvents ->
      Batman.forEach object, (k,v) =>
        addedKeys.push(k) unless @hasKey(k)
        @set(k,v)
    @fire('itemsWereAdded', addedKeys...) if addedKeys.length > 0
  replace: @mutation (object) ->
    addedKeys = []
    removedKeys = []
    @_preventMutationEvents ->
      @forEach (k, _) =>
        unless Batman.objectHasKey(object, k)
          @unset(k)
          removedKeys.push(k)
      Batman.forEach object, (k,v) =>
        addedKeys.push(k) unless @hasKey(k)
        @set(k,v)
    @fire('itemsWereAdded', addedKeys...) if addedKeys.length > 0
    @fire('itemsWereRemoved', removedKeys...) if removedKeys.length > 0
  equality: Batman.SimpleHash::equality
  hashKeyFor: Batman.SimpleHash::hashKeyFor

  for k in ['hasKey', 'forEach', 'isEmpty', 'keys', 'merge', 'toJSON', 'toObject']
    proto = @prototype
    do (k) ->
      proto[k] = ->
        @registerAsMutableSource()
        Batman.SimpleHash::[k].apply(@, arguments)

class Batman.SimpleSet
  constructor: ->
    @_storage = new Batman.SimpleHash
    @_indexes = new Batman.SimpleHash
    @_uniqueIndexes = new Batman.SimpleHash
    @_sorts = new Batman.SimpleHash
    @length = 0
    @add.apply @, arguments if arguments.length > 0

  $extendsEnumerable(@::)

  has: (item) ->
    @_storage.hasKey item

  add: (items...) ->
    addedItems = []
    for item in items when !@_storage.hasKey(item)
      @_storage.set item, true
      addedItems.push item
      @length++
    if @fire and addedItems.length isnt 0
      @fire('change', this, this)
      @fire('itemsWereAdded', addedItems...)
    addedItems
  remove: (items...) ->
    removedItems = []
    for item in items when @_storage.hasKey(item)
      @_storage.unset item
      removedItems.push item
      @length--
    if @fire and removedItems.length isnt 0
      @fire('change', this, this)
      @fire('itemsWereRemoved', removedItems...)
    removedItems

  find: (f) ->
    ret = undefined
    @forEach (item) ->
      if ret is undefined && f(item) is true
        ret = item
    ret

  forEach: (iterator, ctx) ->
    container = this
    @_storage.forEach (key) -> iterator.call(ctx, key, null, container)
  isEmpty: -> @length is 0
  clear: ->
    items = @toArray()
    @_storage = new Batman.SimpleHash
    @length = 0
    if @fire and items.length isnt 0
      @fire('change', this, this)
      @fire('itemsWereRemoved', items...)
    items
  replace: (other) ->
    try
      @prevent?('change')
      @clear()
      @add(other.toArray()...)
    finally
      @allowAndFire?('change', this, this)
  toArray: ->
    @_storage.keys()
  merge: (others...) ->
    merged = new @constructor
    others.unshift(@)
    for set in others
      set.forEach (v) -> merged.add v
    merged
  indexedBy: (key) ->
    @_indexes.get(key) or @_indexes.set(key, new Batman.SetIndex(@, key))
  indexedByUnique: (key) ->
    @_uniqueIndexes.get(key) or @_uniqueIndexes.set(key, new Batman.UniqueSetIndex(@, key))
  sortedBy: (key, order="asc") ->
    order = if order.toLowerCase() is "desc" then "desc" else "asc"
    sortsForKey = @_sorts.get(key) or @_sorts.set(key, new Batman.Object)
    sortsForKey.get(order) or sortsForKey.set(order, new Batman.SetSort(@, key, order))

class Batman.Set extends Batman.Object
  constructor: ->
    Batman.SimpleSet.apply @, arguments

  $extendsEnumerable(@::)

  for k in ['add', 'remove', 'find', 'clear', 'replace', 'indexedBy', 'indexedByUnique', 'sortedBy']
    @::[k] = Batman.SimpleSet::[k]

  for k in ['merge', 'forEach', 'toArray', 'isEmpty', 'has']
    proto = @prototype
    do (k) ->
      proto[k] = ->
        @registerAsMutableSource()
        Batman.SimpleSet::[k].apply(@, arguments)

  toJSON: @::toArray

applySetAccessors = (klass) ->
  klass.accessor 'first', -> @toArray()[0]
  klass.accessor 'last',  -> @toArray()[@length - 1]
  klass.accessor 'indexedBy',          -> new Batman.TerminalAccessible (key) => @indexedBy(key)
  klass.accessor 'indexedByUnique',    -> new Batman.TerminalAccessible (key) => @indexedByUnique(key)
  klass.accessor 'sortedBy',           -> new Batman.TerminalAccessible (key) => @sortedBy(key)
  klass.accessor 'sortedByDescending', -> new Batman.TerminalAccessible (key) => @sortedBy(key, 'desc')
  klass.accessor 'isEmpty', -> @isEmpty()
  klass.accessor 'toArray', -> @toArray()
  klass.accessor 'length',  ->
    @registerAsMutableSource()
    @length

applySetAccessors(Batman.Set)

class Batman.SetObserver extends Batman.Object
  constructor: (@base) ->
    @_itemObservers = new Batman.SimpleHash
    @_setObservers = new Batman.SimpleHash
    @_setObservers.set "itemsWereAdded", => @fire('itemsWereAdded', arguments...)
    @_setObservers.set "itemsWereRemoved", => @fire('itemsWereRemoved', arguments...)
    @on 'itemsWereAdded', @startObservingItems.bind(@)
    @on 'itemsWereRemoved', @stopObservingItems.bind(@)

  observedItemKeys: []
  observerForItemAndKey: (item, key) ->

  _getOrSetObserverForItemAndKey: (item, key) ->
    @_itemObservers.getOrSet item, =>
      observersByKey = new Batman.SimpleHash
      observersByKey.getOrSet key, =>
        @observerForItemAndKey(item, key)
  startObserving: ->
    @_manageItemObservers("observe")
    @_manageSetObservers("addHandler")
  stopObserving: ->
    @_manageItemObservers("forget")
    @_manageSetObservers("removeHandler")
  startObservingItems: (items...) ->
    @_manageObserversForItem(item, "observe") for item in items
  stopObservingItems: (items...) ->
    @_manageObserversForItem(item, "forget") for item in items
  _manageObserversForItem: (item, method) ->
    return unless item.isObservable
    for key in @observedItemKeys
      item[method] key, @_getOrSetObserverForItemAndKey(item, key)
    @_itemObservers.unset(item) if method is "forget"
  _manageItemObservers: (method) ->
    @base.forEach (item) => @_manageObserversForItem(item, method)
  _manageSetObservers: (method) ->
    return unless @base.isObservable
    @_setObservers.forEach (key, observer) =>
      @base.event(key)[method](observer)

class Batman.SetProxy extends Batman.Object
  constructor: () ->
    super()
    @length = 0
    @base.on 'itemsWereAdded', (items...) => @fire('itemsWereAdded', items...)
    @base.on 'itemsWereRemoved', (items...) => @fire('itemsWereRemoved', items...)

  $extendsEnumerable(@::)

  filter: (f) ->
    r = new Batman.Set()
    @reduce(((r, e) -> r.add(e) if f(e); r), r)

  for k in ['add', 'remove', 'find', 'clear', 'replace']
    do (k) =>
      @::[k] = ->
        results = @base[k](arguments...)
        @length = @set('length', @base.get 'length')
        results

  for k in ['has', 'merge', 'toArray', 'isEmpty', 'indexedBy', 'indexedByUnique', 'sortedBy']
    do (k) =>
      @::[k] = -> @base[k](arguments...)

  applySetAccessors(@)

  @accessor 'length',
    get: ->
      @registerAsMutableSource()
      @length
    set: (_, v) -> @length = v

class Batman.SetSort extends Batman.SetProxy
  constructor: (@base, @key, order="asc") ->
    super()
    @descending = order.toLowerCase() is "desc"
    if @base.isObservable
      @_setObserver = new Batman.SetObserver(@base)
      @_setObserver.observedItemKeys = [@key]
      boundReIndex = @_reIndex.bind(@)
      @_setObserver.observerForItemAndKey = -> boundReIndex
      @_setObserver.on 'itemsWereAdded', boundReIndex
      @_setObserver.on 'itemsWereRemoved', boundReIndex
      @startObserving()
    @_reIndex()
  startObserving: -> @_setObserver?.startObserving()
  stopObserving: -> @_setObserver?.stopObserving()
  toArray: -> @get('_storage')
  forEach: (iterator, ctx) -> iterator.call(ctx,e,i,this) for e,i in @get('_storage')
  compare: (a,b) ->
    return 0 if a is b
    return 1 if a is undefined
    return -1 if b is undefined
    return 1 if a is null
    return -1 if b is null
    return 1 if a is false
    return -1 if b is false
    return 1 if a is true
    return -1 if b is true
    if a isnt a
      if b isnt b
        return 0 # both are NaN
      else
        return 1 # a is NaN
    return -1 if b isnt b # b is NaN
    return 1 if a > b
    return -1 if a < b
    return 0
  _reIndex: ->
    newOrder = @base.toArray().sort (a,b) =>
      valueA = $get(a, @key)
      valueA = valueA.valueOf() if valueA?
      valueB = $get(b, @key)
      valueB = valueB.valueOf() if valueB?
      multiple = if @descending then -1 else 1
      @compare.call(@, valueA, valueB) * multiple
    @_setObserver?.startObservingItems(newOrder...)
    @set('_storage', newOrder)

class Batman.SetIndex extends Batman.Object
  propertyClass: Batman.Property
  constructor: (@base, @key) ->
    super()
    @_storage = new Batman.SimpleHash
    if @base.isEventEmitter
      @_setObserver = new Batman.SetObserver(@base)
      @_setObserver.observedItemKeys = [@key]
      @_setObserver.observerForItemAndKey = @observerForItemAndKey.bind(@)
      @_setObserver.on 'itemsWereAdded', (items...) =>
        @_addItem(item) for item in items
      @_setObserver.on 'itemsWereRemoved', (items...) =>
        @_removeItem(item) for item in items
    @base.forEach @_addItem.bind(@)
    @startObserving()
  @accessor (key) -> @_resultSetForKey(key)
  startObserving: ->@_setObserver?.startObserving()
  stopObserving: -> @_setObserver?.stopObserving()
  observerForItemAndKey: (item, key) ->
    (newValue, oldValue) =>
      @_removeItemFromKey(item, oldValue)
      @_addItemToKey(item, newValue)
  _addItem: (item) -> @_addItemToKey(item, @_keyForItem(item))
  _addItemToKey: (item, key) ->
    @_resultSetForKey(key).add item
  _removeItem: (item) -> @_removeItemFromKey(item, @_keyForItem(item))
  _removeItemFromKey: (item, key) -> @_resultSetForKey(key).remove(item)
  _resultSetForKey: (key) ->
    @_storage.getOrSet(key, -> new Batman.Set)
  _keyForItem: (item) ->
    Batman.Keypath.forBaseAndKey(item, @key).getValue()

class Batman.UniqueSetIndex extends Batman.SetIndex
  constructor: ->
    @_uniqueIndex = new Batman.Hash
    super
  @accessor (key) -> @_uniqueIndex.get(key)
  _addItemToKey: (item, key) ->
    @_resultSetForKey(key).add item
    unless @_uniqueIndex.hasKey(key)
      @_uniqueIndex.set(key, item)
  _removeItemFromKey: (item, key) ->
    resultSet = @_resultSetForKey(key)
    super
    if resultSet.isEmpty()
      @_uniqueIndex.unset(key)
    else
      @_uniqueIndex.set(key, resultSet.toArray()[0])

class Batman.BinarySetOperation extends Batman.Set
  constructor: (@left, @right) ->
    super()
    @_setup @left, @right
    @_setup @right, @left

  _setup: (set, opposite) =>
    set.on 'itemsWereAdded', (items...) =>
      @_itemsWereAddedToSource(set, opposite, items...)
    set.on 'itemsWereRemoved', (items...) =>
      @_itemsWereRemovedFromSource(set, opposite, items...)
    @_itemsWereAddedToSource set, opposite, set.toArray()...

  merge: (others...) ->
    merged = new Batman.Set
    others.unshift(@)
    for set in others
      set.forEach (v) -> merged.add v
    merged

  filter: Batman.SetProxy::filter

class Batman.SetUnion extends Batman.BinarySetOperation
  _itemsWereAddedToSource: (source, opposite, items...) ->  @add items...
  _itemsWereRemovedFromSource: (source, opposite, items...) ->
    itemsToRemove = (item for item in items when !opposite.has(item))
    @remove itemsToRemove...

class Batman.SetIntersection extends Batman.BinarySetOperation
  _itemsWereAddedToSource: (source, opposite, items...) ->
    itemsToAdd = (item for item in items when opposite.has(item))
    @add itemsToAdd...
  _itemsWereRemovedFromSource: (source, opposite, items...) -> @remove items...

# State Machines
# --------------

Batman.StateMachine = {
  initialize: ->
    Batman.initializeObject @
    if not @_batman.states
      @_batman.states = new Batman.SimpleHash

  state: (name, callback) ->
    Batman.StateMachine.initialize.call @

    return @_batman.getFirst 'state' unless name
    developer.assert @isEventEmitter, "StateMachine requires EventEmitter"

    @[name] ||= (callback) -> _stateMachine_setState.call(@, name)
    @on(name, callback) if typeof callback is 'function'

  transition: (from, to, callback) ->
    Batman.StateMachine.initialize.call @
    @state from
    @state to
    @on("#{from}->#{to}", callback) if callback
}

# A special method to alias state machine methods to class methods
Batman.Object.actsAsStateMachine = (includeInstanceMethods=true) ->
    Batman.StateMachine.initialize.call @
    Batman.StateMachine.initialize.call @prototype

    @classState = -> Batman.StateMachine.state.apply @, arguments
    @state = -> @classState.apply @prototype, arguments
    @::state = @classState if includeInstanceMethods

    @classTransition = -> Batman.StateMachine.transition.apply @, arguments
    @transition = -> @classTransition.apply @prototype, arguments
    @::transition = @classTransition if includeInstanceMethods

# This is cached here so it doesn't need to be recompiled for every setter
_stateMachine_setState = (newState) ->
  Batman.StateMachine.initialize.call @

  if @_batman.isTransitioning
    (@_batman.nextState ||= []).push(newState)
    return false

  @_batman.isTransitioning = yes

  oldState = @state()
  @_batman.state = newState

  if newState and oldState
    @fire("#{oldState}->#{newState}", newState, oldState)

  if newState
    @fire(newState, newState, oldState)

  @_batman.isTransitioning = no
  @[@_batman.nextState.shift()]() if @_batman.nextState?.length

  newState

# App, Requests, and Routing
# --------------------------

# `Batman.Request` is a normalizer for XHR requests in the Batman world.
class Batman.Request extends Batman.Object
  @objectToFormData: (data) ->
    pairForList = (key, object, first = false) ->
      list = switch Batman.typeOf(object)
        when 'Object'
          list = for k, v of object
            pairForList((if first then k else "#{key}[#{k}]"), v)
          list.reduce((acc, list) ->
            acc.concat list
          , [])
        when 'Array'
          object.reduce((acc, element) ->
            acc.concat pairForList("#{key}[]", element)
          , [])
        else
          [[key, object]]

    formData = new Batman.container.FormData()
    for [key, val] in pairForList("", data, true)
      formData.append(key, val)
    formData

  url: ''
  data: ''
  method: 'GET'
  formData: false
  response: null
  status: null
  headers: {}

  @accessor 'method', $mixin {}, Batman.Property.defaultAccessor,
    set: (k,val) -> @[k] = val?.toUpperCase?()

  # Set the content type explicitly for PUT and POST requests.
  contentType: 'application/x-www-form-urlencoded'

  constructor: (options) ->
    handlers = {}
    for k, handler of options when k in ['success', 'error', 'loading', 'loaded']
      handlers[k] = handler
      delete options[k]

    super(options)
    @on k, handler for k, handler of handlers

  # After the URL gets set, we'll try to automatically send
  # your request after a short period. If this behavior is
  # not desired, use @cancel() after setting the URL.
  @observeAll 'url', (url) ->
    @_autosendTimeout = $setImmediate =>
      @send()

  # `send` is implmented in the platform layer files. One of those must be required for
  # `Batman.Request` to be useful.
  send: -> developer.error "Please source a dependency file for a request implementation"

  cancel: ->
    $clearImmediate(@_autosendTimeout) if @_autosendTimeout

## Routes

class Batman.Route extends Batman.Object
  # Route regexes courtesy of Backbone
  @regexps =
    namedParam: /:([\w\d]+)/g
    splatParam: /\*([\w\d]+)/g
    queryParam: '(?:\\?.+)?'
    namedOrSplat: /[:|\*]([\w\d]+)/g
    namePrefix: '[:|\*]'
    escapeRegExp: /[-[\]{}()+?.,\\^$|#\s]/g

  optionKeys: ['member', 'collection']
  testKeys: ['controller', 'action']
  isRoute: true
  constructor: (templatePath, baseParams) ->
    regexps = @constructor.regexps
    templatePath = "/#{templatePath}" if templatePath.indexOf('/') isnt 0
    pattern = templatePath.replace(regexps.escapeRegExp, '\\$&')

    regexp = ///
      ^
      #{pattern
          .replace(regexps.namedParam, '([^\/]+)')
          .replace(regexps.splatParam, '(.*?)') }
      #{regexps.queryParam}
      $
    ///

    namedArguments = (matches[1] while matches = regexps.namedOrSplat.exec(pattern))

    properties = {templatePath, pattern, regexp, namedArguments, baseParams}
    for k in @optionKeys
      properties[k] = baseParams[k]
      delete baseParams[k]

    super(properties)

  paramsFromPath: (path) ->
    [path, query] = path.split '?'
    namedArguments = @get('namedArguments')
    params = $mixin {path}, @get('baseParams')

    matches = @get('regexp').exec(path).slice(1)
    for match, index in matches
      name = namedArguments[index]
      params[name] = match

    if query
      for pair in query.split('&')
        [key, value] = pair.split '='
        params[key] = value

    params

  pathFromParams: (argumentParams) ->
    params = $mixin {}, argumentParams
    path = @get('templatePath')

    # Replace the names in the template with their values from params
    for name in @get('namedArguments')
      regexp = ///#{@constructor.regexps.namePrefix}#{name}///
      newPath = path.replace regexp, (if params[name]? then params[name] else '')
      if newPath != path
        delete params[name]
        path = newPath

    for key in @testKeys
      delete params[key]
    # Append the rest of the params as a query string
    queryParams = ("#{key}=#{value}" for key, value of params)
    if queryParams.length > 0
      path += "?" + queryParams.join("&")

    path

  test: (pathOrParams) ->
    if typeof pathOrParams is 'string'
      path = pathOrParams
    else if pathOrParams.path?
      path = pathOrParams.path
    else
      path = @pathFromParams(pathOrParams)
      for key in @testKeys
        if (value = @get(key))?
          return false unless pathOrParams[key] == value

    @get('regexp').test(path)

  dispatch: (pathOrParams) ->
    return false unless @test(pathOrParams)
    if typeof pathOrParams is 'string'
      params = @paramsFromPath(pathOrParams)
      path = pathOrParams
    else
      params = pathOrParams
      path = @pathFromParams(pathOrParams)
    @get('callback')(params)
    return path

  callback: -> throw new Batman.DevelopmentError "Override callback in a Route subclass"

class Batman.CallbackActionRoute extends Batman.Route
  optionKeys: ['member', 'collection', 'callback', 'app']
  controller: false
  action: false

class Batman.ControllerActionRoute extends Batman.Route
  optionKeys: ['member', 'collection', 'app', 'controller', 'action']
  constructor: (templatePath, options) ->
    if options.signature
      [controller, action] = options.signature.split('#')
      action ||= 'index'
      options.controller = controller
      options.action = action
      delete options.signature

    super(templatePath, options)

  callback: (params) =>
    controller = @get("app.dispatcher.controllers.#{@get('controller')}")
    controller.dispatch(@get('action'), params)

class Batman.Dispatcher extends Batman.Object
  @canInferRoute: (argument) ->
    argument instanceof Batman.Model ||
    argument instanceof Batman.AssociationProxy ||
    argument.prototype instanceof Batman.Model

  @paramsFromArgument: (argument) ->
    resourceNameFromModel = (model) ->
      helpers.underscore(helpers.pluralize($functionName(model)))

    return argument unless @canInferRoute(argument)

    if argument instanceof Batman.Model || argument instanceof Batman.AssociationProxy
      argument = argument.get('target') if argument.isProxy
      if argument?
        {
          controller: resourceNameFromModel(argument.constructor)
          action: 'show'
          id: argument.get('id')
        }
      else
        {}
    else if argument.prototype instanceof Batman.Model
      {
        controller: resourceNameFromModel(argument)
        action: 'index'
      }
    else
      argument

  class ControllerDirectory extends Batman.Object
    @accessor '__app', Batman.Property.defaultAccessor
    @accessor (key) -> @get("__app.#{helpers.capitalize(key)}Controller.sharedController")

  @accessor 'controllers', -> new ControllerDirectory(__app: @get('app'))

  constructor: (app, routeMap) ->
    super({app, routeMap})

  routeForParams: (params) ->
    params = @constructor.paramsFromArgument(params)
    @get('routeMap').routeForParams(params)

  pathFromParams: (params) ->
    return params if typeof params is 'string'
    params = @constructor.paramsFromArgument(params)
    @routeForParams(params)?.pathFromParams(params)

  dispatch: (params) ->
    inferredParams = @constructor.paramsFromArgument(params)
    route = @routeForParams(inferredParams)
    if route
      path = route.dispatch(inferredParams)
    else
      # No route matching the parameters was found, but an object which might be for
      # use with the params replacer has been passed. If its an object like a model
      # or a record we could have inferred a route for it (but didn't), so we make
      # sure that it isn't by running it through canInferRoute.
      if $typeOf(params) is 'Object' && !@constructor.canInferRoute(params)
        return @get('app.currentParams').replace(params)
      else
        @get('app.currentParams').clear()

      return Batman.redirect('/404') unless path is '/404' or params is '/404'

    @set 'app.currentURL', path
    @set 'app.currentRoute', route

    path

class Batman.RouteMap
  memberRoute: null
  collectionRoute: null

  constructor: ->
    @childrenByOrder = []
    @childrenByName = {}

  routeForParams: (params) ->
    for route in @childrenByOrder
      return route if route.test(params)

    return undefined

  addRoute: (name, route) ->
    @childrenByOrder.push(route)
    if name.length > 0 && (names = name.split('.')).length > 0
      base = names.shift()
      unless @childrenByName[base]
        @childrenByName[base] = new Batman.RouteMap
      @childrenByName[base].addRoute(names.join('.'), route)
    else
      if route.get('member')
        developer.do =>
          Batman.developer.error("Member route with name #{name} already exists!") if @memberRoute
        @memberRoute = route
      else
        developer.do =>
          Batman.developer.error("Collection route with name #{name} already exists!") if @collectionRoute
        @collectionRoute = route
    true

class Batman.NamedRouteQuery extends Batman.Object
  isNamedRouteQuery: true

  developer.do =>
    class NonWarningProperty extends Batman.Keypath
      constructor: ->
        developer.suppress()
        super
        developer.unsuppress()

    @::propertyClass = NonWarningProperty

  constructor: (routeMap, args = []) ->
    super({routeMap, args})

  @accessor 'route', ->
    {memberRoute, collectionRoute} = @get('routeMap')
    for route in [memberRoute, collectionRoute] when route?
      return route if route.namedArguments.length == @get('args').length
    return collectionRoute || memberRoute

  @accessor 'path', ->
    params = {}
    namedArguments = @get('route.namedArguments')
    for argumentName, index in namedArguments
      if (argumentValue = @get('args')[index])?
        params[argumentName] = @_toParam(argumentValue)

    @get('route').pathFromParams(params)

  @accessor 'routeMap', 'args', 'cardinality', Batman.Property.defaultAccessor

  @accessor
    get: (key) ->
      return if !key?
      if typeof key is 'string'
        @nextQueryForName(key)
      else
        @nextQueryWithArgument(key)
    set: ->
    cacheable: false

  nextQueryForName: (key) ->
    if map = @get('routeMap').childrenByName[key]
      return new Batman.NamedRouteQuery(map, @args)
    else
      Batman.developer.error "Couldn't find a route for the name #{key}!"

  nextQueryWithArgument: (arg) ->
    args = @args.slice(0)
    args.push arg
    @clone(args)

  _toParam: (arg) ->
    if arg instanceof Batman.AssociationProxy
      arg = arg.get('target')
    if arg.toParam isnt 'undefined' then arg.toParam() else arg

  _paramName: (arg) ->
    string = helpers.singularize($functionName(arg)) + "Id"
    string.charAt(0).toLowerCase() + string.slice(1)

  clone: (args = @args) -> new Batman.NamedRouteQuery(@routeMap, args)

class Batman.RouteMapBuilder
  @BUILDER_FUNCTIONS = ['resources', 'member', 'collection', 'route', 'root']
  @ROUTES =
    index:
      cardinality: 'collection'
      path: (resource) -> resource
      name: (resource) -> resource
    new:
      cardinality: 'collection'
      path: (resource) -> "#{resource}/new"
      name: (resource) -> "#{resource}.new"
    show:
      cardinality: 'member'
      path: (resource) -> "#{resource}/:id"
      name: (resource) -> resource
    edit:
      cardinality: 'member'
      path: (resource) -> "#{resource}/:id/edit"
      name: (resource) -> "#{resource}.edit"
    collection:
      cardinality: 'collection'
      path: (resource, name) -> "#{resource}/#{name}"
      name: (resource, name) -> "#{resource}.#{name}"
    member:
      cardinality: 'member'
      path: (resource, name) -> "#{resource}/:id/#{name}"
      name: (resource, name) -> "#{resource}.#{name}"

  constructor: (@app, @routeMap, @parent, @baseOptions = {}) ->
    if @parent
      @rootPath = @parent._nestingPath()
      @rootName = @parent._nestingName()
    else
      @rootPath = ''
      @rootName = ''

  resources: (args...) ->
    resourceNames = (arg for arg in args when typeof arg is 'string')
    callback = args.pop() if typeof args[args.length - 1] is 'function'
    if typeof args[args.length - 1] is 'object'
      options = args.pop()
    else
      options = {}

    actions = {index: true, new: true, show: true, edit: true}

    if options.except
      actions[k] = false for k in options.except
      delete options.except
    else if options.only
      actions[k] = false for k, v of actions
      actions[k] = true for k in options.only
      delete options.only

    for resourceName in resourceNames
      controller = resourceRoot = helpers.pluralize(resourceName)
      childBuilder = @_childBuilder({controller})

      # Call the callback so that routes defined within it are matched
      # before the standard routes defined by `resources`.
      callback?.call(childBuilder)

      for action, included of actions when included
        route = @constructor.ROUTES[action]
        as = route.name(resourceRoot)
        path = route.path(resourceRoot)
        routeOptions = $mixin {controller, action, path, as}, options
        childBuilder[route.cardinality](action, routeOptions)

    true

  member: -> @_addRoutesWithCardinality('member', arguments...)
  collection: -> @_addRoutesWithCardinality('collection', arguments...)

  root: (signature, options) -> @route '/', signature, options

  route: (path, signature, options, callback) ->
    if !callback
      if typeof options is 'function'
        callback = options
        options = undefined
      else if typeof signature is 'function'
        callback = signature
        signature = undefined

    if !options
      if typeof signature is 'string'
        options = {signature}
      else
        options = signature
      options ||= {}
    else
      options.signature = signature if signature
    options.callback = callback if callback
    options.as ||= @_nameFromPath(path)
    options.path = path
    @_addRoute(options)

  _addRoutesWithCardinality: (cardinality, names..., options) ->
    if typeof options is 'string'
      names.push options
      options = {}
    options = $mixin {}, @baseOptions, options
    options[cardinality] = true
    route = @constructor.ROUTES[cardinality]
    resourceRoot = options.controller
    for name in names
      routeOptions = $mixin {action: name}, options
      unless routeOptions.path?
        routeOptions.path = route.path(resourceRoot, name)
      unless routeOptions.as?
        routeOptions.as = route.name(resourceRoot, name)
      @_addRoute(routeOptions)
    true

  _addRoute: (options = {}) ->
    path = @rootPath + options.path
    name = @rootName + options.as
    delete options.as
    delete options.path
    klass = if options.callback then Batman.CallbackActionRoute else Batman.ControllerActionRoute
    options.app = @app
    route = new klass(path, options)
    @routeMap.addRoute(name, route)

  _nameFromPath: (path) ->
    underscored = path
      .replace(Batman.Route.regexps.namedOrSplat, '')
      .replace(/\/+/g, '_')
      .replace(/(^_)|(_$)/g, '')

    name = helpers.camelize(underscored)
    name.charAt(0).toLowerCase() + name.slice(1)

  _nestingPath: ->
    unless @parent
      ""
    else
      nestingParam = ":" + helpers.singularize(@baseOptions.controller) + "Id"
      "#{@parent._nestingPath()}/#{@baseOptions.controller}/#{nestingParam}/"

  _nestingName: ->
    unless @parent
      ""
    else
      @baseOptions.controller + "."

  _childBuilder: (baseOptions = {}) -> new Batman.RouteMapBuilder(@app, @routeMap, @, baseOptions)

class Batman.Navigator
  @defaultClass: ->
    if Batman.config.usePushState and Batman.PushStateNavigator.isSupported()
      Batman.PushStateNavigator
    else
      Batman.HashbangNavigator
  @forApp: (app) -> new (@defaultClass())(app)
  constructor: (@app) ->
  start: ->
    return if typeof window is 'undefined'
    return if @started
    @started = yes
    @startWatching()
    Batman.currentApp.prevent 'ready'
    $setImmediate =>
      if @started && Batman.currentApp
        @handleCurrentLocation()
        Batman.currentApp.allowAndFire 'ready'
  stop: ->
    @stopWatching()
    @started = no
  handleLocation: (location) ->
    path = @pathFromLocation(location)
    return if path is @cachedPath
    @dispatch(path)
  handleCurrentLocation: => @handleLocation(window.location)
  dispatch: (params) ->
    @cachedPath = @app.get('dispatcher').dispatch(params)
  push: (params) ->
    path = @dispatch(params)
    @pushState(null, '', path)
    path
  replace: (params) ->
    path = @dispatch(params)
    @replaceState(null, '', path)
    path
  redirect: @::push
  normalizePath: (segments...) ->
    segments = for seg, i in segments
      "#{seg}".replace(/^(?!\/)/, '/').replace(/\/+$/,'')
    segments.join('') or '/'
  @normalizePath: @::normalizePath

class Batman.PushStateNavigator extends Batman.Navigator
  @isSupported: -> window?.history?.pushState?
  startWatching: ->
    $addEventListener window, 'popstate', @handleCurrentLocation
  stopWatching: ->
    $removeEventListener window, 'popstate', @handleCurrentLocation
  pushState: (stateObject, title, path) ->
    window.history.pushState(stateObject, title, @linkTo(path))
  replaceState: (stateObject, title, path) ->
    window.history.replaceState(stateObject, title, @linkTo(path))
  linkTo: (url) ->
    @normalizePath(Batman.config.pathPrefix, url)
  pathFromLocation: (location) ->
    fullPath = "#{location.pathname or ''}#{location.search or ''}"
    prefixPattern = new RegExp("^#{@normalizePath(Batman.config.pathPrefix)}")
    @normalizePath(fullPath.replace(prefixPattern, ''))
  handleLocation: (location) ->
    path = @pathFromLocation(location)
    if path is '/' and (hashbangPath = Batman.HashbangNavigator::pathFromLocation(location)) isnt '/'
      @replace(hashbangPath)
    else
      super

class Batman.HashbangNavigator extends Batman.Navigator
  HASH_PREFIX: '#!'
  if window? and 'onhashchange' of window
    @::startWatching = ->
      $addEventListener window, 'hashchange', @handleCurrentLocation
    @::stopWatching = ->
      $removeEventListener window, 'hashchange', @handleCurrentLocation
  else
    @::startWatching = ->
      @interval = setInterval @handleCurrentLocation, 100
    @::stopWatching = ->
      @interval = clearInterval @interval
  pushState: (stateObject, title, path) ->
    window.location.hash = @linkTo(path)
  replaceState: (stateObject, title, path) ->
    loc = window.location
    loc.replace("#{loc.pathname}#{loc.search}#{@linkTo(path)}")
  linkTo: (url) -> @HASH_PREFIX + url
  pathFromLocation: (location) ->
    hash = location.hash
    if hash?.substr(0,2) is @HASH_PREFIX
      @normalizePath(hash.substr(2))
    else
      '/'
  handleLocation: (location) ->
    return super unless Batman.config.usePushState
    realPath = Batman.PushStateNavigator::pathFromLocation(location)
    if realPath is '/'
      super
    else
      location.replace(@normalizePath("#{Batman.config.pathPrefix}#{@linkTo(realPath)}"))

Batman.redirect = $redirect = (url) ->
  Batman.navigator?.redirect url

class Batman.ParamsReplacer extends Batman.Object
  constructor: (@navigator, @params) ->
  redirect: -> @navigator.replace(@toObject())
  replace: (params) ->
    @params.replace(params)
    @redirect()
  update: (params) ->
    @params.update(params)
    @redirect()
  clear: () ->
    @params.clear()
    @redirect()
  toObject: -> @params.toObject()
  @accessor
    get: (k) -> @params.get(k)
    set: (k,v) ->
      oldValue = @params.get(k)
      result = @params.set(k,v)
      @redirect() if oldValue isnt v
      result
    unset: (k) ->
      hadKey = @params.hasKey(k)
      result = @params.unset(k)
      @redirect() if hadKey
      result

class Batman.ParamsPusher extends Batman.ParamsReplacer
  redirect: -> @navigator.push(@toObject())

# `Batman.App` manages requiring files and acts as a namespace for all code subclassing
# Batman objects.
class Batman.App extends Batman.Object
  @classAccessor 'currentParams',
    get: -> new Batman.Hash
    'final': true

  @classAccessor 'paramsManager',
    get: ->
      return unless nav = @get('navigator')
      params = @get('currentParams')
      params.replacer = new Batman.ParamsReplacer(nav, params)
    'final': true

  @classAccessor 'paramsPusher',
    get: ->
      return unless nav = @get('navigator')
      params = @get('currentParams')
      params.pusher = new Batman.ParamsPusher(nav, params)
    'final': true

  @classAccessor 'routes', -> new Batman.NamedRouteQuery(@get('routeMap'))
  @classAccessor 'routeMap', -> new Batman.RouteMap
  @classAccessor 'routeMapBuilder', -> new Batman.RouteMapBuilder(@, @get('routeMap'))
  @classAccessor 'dispatcher', -> new Batman.Dispatcher(@, @get('routeMap'))
  @classAccessor 'controllers', -> @get('dispatcher.controllers')

  @classAccessor '_renderContext', -> Batman.RenderContext.base.descend(@)

  # Require path tells the require methods which base directory to look in.
  @requirePath: ''

  # The require class methods (`controller`, `model`, `view`) simply tells
  # your app where to look for coffeescript source files. This
  # implementation may change in the future.
  developer.do =>
    App.require = (path, names...) ->
      base = @requirePath + path
      for name in names
        @prevent 'run'

        path = base + '/' + name + '.coffee'
        new Batman.Request
          url: path
          type: 'html'
          success: (response) =>
            CoffeeScript.eval response
            @allow 'run'
            if not @isPrevented 'run'
              @fire 'loaded'

            @run() if @wantsToRun
      @

    @controller = (names...) ->
      names = names.map (n) -> n + '_controller'
      @require 'controllers', names...

    @model = ->
      @require 'models', arguments...

    @view = ->
      @require 'views', arguments...

  # Layout is the base view that other views can be yielded into. The
  # default behavior is that when `app.run()` is called, a new view will
  # be created for the layout using the `document` node as its content.
  # Use `MyApp.layout = null` to turn off the default behavior.
  @layout: undefined

  # Routes for the app are built using a RouteMapBuilder, so delegate the
  # functions used to build routes to it.
  for name in Batman.RouteMapBuilder.BUILDER_FUNCTIONS
    do (name) =>
      @[name] = -> @get('routeMapBuilder')[name](arguments...)

  # Call `MyApp.run()` to start up an app. Batman level initializers will
  # be run to bootstrap the application.
  @event('ready').oneShot = true
  @event('run').oneShot = true
  @run: ->
    if Batman.currentApp
      return if Batman.currentApp is @
      Batman.currentApp.stop()

    return false if @hasRun

    if @isPrevented 'run'
      @wantsToRun = true
      return false
    else
      delete @wantsToRun

    Batman.currentApp = @
    Batman.App.set('current', @)

    unless @get('dispatcher')?
      @set 'dispatcher', new Batman.Dispatcher(@, @get('routeMap'))
      @set 'controllers', @get('dispatcher.controllers')

    unless @get('navigator')?
      @set('navigator', Batman.Navigator.forApp(@))
      @on 'run', =>
        Batman.navigator = @get('navigator')
        Batman.navigator.start() if Object.keys(@get('dispatcher').routeMap).length > 0

    @observe 'layout', (layout) =>
      layout?.on 'ready', => @fire 'ready'

    if typeof @layout is 'undefined'
      @set 'layout', new Batman.View
        context: @
        node: document

    else if typeof @layout is 'string'
      @set 'layout', new @[helpers.camelize(@layout) + 'View']

    @hasRun = yes
    @fire('run')
    @

  @event('ready').oneShot = true
  @event('stop').oneShot = true

  @stop: ->
    @navigator?.stop()
    Batman.navigator = null
    @hasRun = no
    @fire('stop')
    @

# Controllers
# -----------

class Batman.Controller extends Batman.Object
  @singleton 'sharedController'

  @accessor 'controllerName', -> @_controllerName ||= helpers.underscore($functionName(@constructor).replace('Controller', ''))
  @accessor '_renderContext', -> Batman.RenderContext.root().descend(@)

  @beforeFilter: (options, nameOrFunction) ->
    if not nameOrFunction
      nameOrFunction = options
      options = {}
    else
      options.only = [options.only] if options.only and $typeOf(options.only) isnt 'Array'
      options.except = [options.except] if options.except and $typeOf(options.except) isnt 'Array'

    Batman.initializeObject @
    options.block = nameOrFunction
    filters = @_batman.beforeFilters ||= new Batman.Hash
    filters.set(nameOrFunction, options)

  @afterFilter: (options, nameOrFunction) ->
    if not nameOrFunction
      nameOrFunction = options
      options = {}
    else
      options.only = [options.only] if options.only and $typeOf(options.only) isnt 'Array'
      options.except = [options.except] if options.except and $typeOf(options.except) isnt 'Array'

    Batman.initializeObject @
    options.block = nameOrFunction
    filters = @_batman.afterFilters ||= new Batman.Hash
    filters.set(nameOrFunction, options)

  runFilters: (params, filters) ->
    action = params.action
    if filters = @constructor._batman?.get(filters)
      filters.forEach (_, options) =>
        return if options.only and action not in options.only
        return if options.except and action in options.except

        block = options.block
        if typeof block is 'function' then block.call(@, params) else @[block]?(params)

  # You shouldn't call this method directly. It will be called by the dispatcher when a route is called.
  # If you need to call a route manually, use `$redirect()`.
  dispatch: (action, params = {}) ->
    params.controller ||= @get 'controllerName'
    params.action ||= action
    params.target ||= @

    oldRedirect = Batman.navigator?.redirect
    Batman.navigator?.redirect = @redirect

    @_inAction = yes
    @_actedDuringAction = no
    @set 'action', action
    @set 'params', params

    @runFilters params, 'beforeFilters'

    developer.assert @[action], "Error! Controller action #{@get 'controllerName'}.#{action} couldn't be found!"
    @[action](params)

    if not @_actedDuringAction
      @render()

    @runFilters params, 'afterFilters'

    delete @_actedDuringAction
    delete @_inAction

    Batman.navigator?.redirect = oldRedirect

    redirectTo = @_afterFilterRedirect
    delete @_afterFilterRedirect

    $redirect(redirectTo) if redirectTo

  redirect: (url) =>
    if @_actedDuringAction && @_inAction
      developer.warn "Warning! Trying to redirect but an action has already be taken during #{@get('controllerName')}.#{@get('action')}}"

    if @_inAction
      @_actedDuringAction = yes
      @_afterFilterRedirect = url
    else
      if $typeOf(url) is 'Object'
        url.controller = @ if not url.controller

      $redirect url

  render: (options = {}) ->
    if @_actedDuringAction && @_inAction
      developer.warn "Warning! Trying to render but an action has already be taken during #{@get('controllerName')}.#{@get('action')}"

    @_actedDuringAction = yes

    return if options is false

    if not options.view
      options.context ||= @get('_renderContext')
      options.source ||= helpers.underscore(@get('controllerName') + '/' + @get('action'))
      options.view = new (Batman.currentApp?[helpers.camelize("#{@get('controllerName')}_#{@get('action')}_view")] || Batman.View)(options)

    if view = options.view
      Batman.currentApp?.prevent 'ready'
      view.on 'ready', =>
        Batman.DOM.replace options.into || 'main', view.get('node'), view.hasContainer
        Batman.currentApp?.allowAndFire 'ready'
        view.ready?(@params)
    view

# Models
# ------

class Batman.Model extends Batman.Object
  # ## Model API
  # Override this property if your model is indexed by a key other than `id`
  @primaryKey: 'id'

  # Override this property to define the key which storage adapters will use to store instances of this model under.
  #  - For RestStorage, this ends up being part of the url built to store this model
  #  - For LocalStorage, this ends up being the namespace in localStorage in which JSON is stored
  @storageKey: null

  # Pick one or many mechanisms with which this model should be persisted. The mechanisms
  # can be already instantiated or just the class defining them.
  @persist: (mechanisms...) ->
    Batman.initializeObject @prototype
    storage = @::_batman.storage ||= []
    results = for mechanism in mechanisms
      mechanism = if mechanism.isStorageAdapter then mechanism else new mechanism(@)
      storage.push mechanism
      mechanism
    if results.length > 1
      results
    else
      results[0]

  # Encoders are the tiny bits of logic which manage marshalling Batman models to and from their
  # storage representations. Encoders do things like stringifying dates and parsing them back out again,
  # pulling out nested model collections and instantiating them (and JSON.stringifying them back again),
  # and marshalling otherwise un-storable object.
  @encode: (keys..., encoderOrLastKey) ->
    Batman.initializeObject @prototype
    @::_batman.encoders ||= new Batman.SimpleHash
    @::_batman.decoders ||= new Batman.SimpleHash
    encoder = {}
    switch $typeOf(encoderOrLastKey)
      when 'String'
        keys.push encoderOrLastKey
      when 'Function'
        encoder.encode = encoderOrLastKey
      else
        encoder.encode = encoderOrLastKey.encode
        encoder.decode = encoderOrLastKey.decode

    encoder = $mixin {}, @defaultEncoder, encoder

    for operation in ['encode', 'decode']
      for key in keys
        hash = @::_batman["#{operation}rs"]
        if encoder[operation]
          hash.set(key, encoder[operation])
        else
          hash.unset(key)
    #true

  # Set up the unit functions as the default for both
  @defaultEncoder:
    encode: (x) -> x
    decode: (x) -> x

  # Attach encoders and decoders for the primary key, and update them if the primary key changes.
  @observeAndFire 'primaryKey', (newPrimaryKey) -> @encode newPrimaryKey, {encode: false, decode: @defaultEncoder.decode}

  # Validations allow a model to be marked as 'valid' or 'invalid' based on a set of programmatic rules.
  # By validating our data before it gets to the server we can provide immediate feedback to the user about
  # what they have entered and forgo waiting on a round trip to the server.
  # `validate` allows the attachment of validations to the model on particular keys, where the validation is
  # either a built in one (by use of options to pass to them) or a custom one (by use of a custom function as
  # the second argument). Custom validators should have the signature `(errors, record, key, callback)`. They
  # should add strings to the `errors` set based on the record (maybe depending on the `key` they were attached
  # to) and then always call the callback. Again: the callback must always be called.
  @validate: (keys..., optionsOrFunction) ->
    Batman.initializeObject @prototype
    validators = @::_batman.validators ||= []

    if typeof optionsOrFunction is 'function'
      # Given a function, use that as the actual validator, expecting it to conform to the API
      # the built in validators do.
      validators.push
        keys: keys
        callback: optionsOrFunction
    else
      # Given options, find the validations which match the given options, and add them to the validators
      # array.
      options = optionsOrFunction
      for validator in Validators
        if (matches = validator.matches(options))
          delete options[match] for match in matches
          validators.push
            keys: keys
            validator: new validator(matches)

  @urlNestsUnder: (key) ->
    parent = Batman.helpers.pluralize(key)
    children = Batman.helpers.pluralize(Batman._functionName(@).toLowerCase())

    @url = (options) ->
      parentID = options.data[key + '_id']
      delete options.data[key + '_id']
      "#{parent}/#{parentID}/#{children}"

    @::url = ->
      url = "#{parent}/#{@get(key + '_id')}/#{children}"
      if id = @get('id')
        url += '/' + id
      url

  # ### Query methods
  @classAccessor 'all',
    get: ->
      @load() if @::hasStorage() and @classState() not in ['loaded', 'loading']
      @get('loaded')

    set: (k, v) -> @set('loaded', v)

  @classAccessor 'loaded',
    get: -> @_loaded ||= new Batman.Set
    set: (k, v) -> @_loaded = v

  @classAccessor 'first', -> @get('all').toArray()[0]
  @classAccessor 'last', -> x = @get('all').toArray(); x[x.length - 1]

  @clear: ->
    Batman.initializeObject(@)
    result = @get('loaded').clear()
    @_batman.get('associations')?.reset()
    result

  @find: (id, callback) ->
    developer.assert callback, "Must call find with a callback!"
    record = new @()
    record.set 'id', id
    newRecord = @_mapIdentity(record)
    newRecord.load callback
    return newRecord

  # `load` fetches records from all sources possible
  @load: (options, callback) ->
    if typeof options in ['function', 'undefined']
      callback = options
      options = {}

    developer.assert @::_batman.getAll('storage').length, "Can't load model #{$functionName(@)} without any storage adapters!"

    @loading()
    @::_doStorageOperation 'readAll', options, (err, records) =>
      if err?
        callback?(err, [])
      else
        mappedRecords = (@_mapIdentity(record) for record in records)
        @loaded()
        callback?(err, mappedRecords)

  # `create` takes an attributes hash, creates a record from it, and saves it given the callback.
  @create: (attrs, callback) ->
    if !callback
      [attrs, callback] = [{}, attrs]
    obj = new this(attrs)
    obj.save(callback)
    obj

  # `findOrCreate` takes an attributes hash, optionally containing a primary key, and returns to you a saved record
  # representing those attributes, either from the server or from the identity map.
  @findOrCreate: (attrs, callback) ->
    record = new this(attrs)
    if record.isNew()
      record.save(callback)
    else
      foundRecord = @_mapIdentity(record)
      foundRecord.updateAttributes(attrs)
      callback(undefined, foundRecord)

  @_mapIdentity: (record) ->
    if typeof (id = record.get('id')) == 'undefined' || id == ''
      return record
    else
      existing = @get("loaded.indexedBy.id").get(id)?.toArray()[0]
      if existing
        existing.updateAttributes(record._batman.attributes || {})
        return existing
      else
        @get('loaded').add(record)
        return record

  associationProxy: (association) ->
    Batman.initializeObject(@)
    proxies = @_batman.associationProxies ||= new Batman.SimpleHash
    proxies.get(association.label) or proxies.set(association.label, new association.proxyClass(association, @))

  # ### Record API

  # Add a universally accessible accessor for retrieving the primrary key, regardless of which key its stored under.
  @accessor 'id',
    get: ->
      pk = @constructor.primaryKey
      if pk == 'id'
        @id
      else
        @get(pk)
    set: (k, v) ->
      # naively coerce string ids into integers
      if typeof v is "string" and v.match(/[^0-9]/) is null
        v = parseInt(v, 10)

      pk = @constructor.primaryKey
      if pk == 'id'
        @id = v
      else
        @set(pk, v)

  # Add normal accessors for the dirty keys and errors attributes of a record, so these accesses don't fall to the
  # default accessor.
  @accessor 'dirtyKeys', 'errors', Batman.Property.defaultAccessor

  # Add an accessor for the internal batman state under `batmanState`, so that the `state` key can be a valid
  # attribute.
  @accessor 'batmanState'
    get: -> @state()
    set: (k, v) -> @state(v)

  # Add a default accessor to make models store their attributes under a namespace by default.
  @accessor Model.defaultAccessor =
    get: (k) ->
      attribute = (@_batman.attributes ||= {})[k]
      if typeof attribute isnt 'undefined'
        attribute
      else
        @[k]
    set: (k, v) -> (@_batman.attributes ||= {})[k] = v
    unset: (k) ->
      x = (@_batman.attributes ||={})[k]
      delete @_batman.attributes[k]
      x

  # New records can be constructed by passing either an ID or a hash of attributes (potentially
  # containing an ID) to the Model constructor. By not passing an ID, the model is marked as new.
  constructor: (idOrAttributes = {}) ->
    developer.assert  @ instanceof Batman.Object, "constructors must be called with new"

    # We have to do this ahead of super, because mixins will call set which calls things on dirtyKeys.
    @dirtyKeys = new Batman.Hash
    @errors = new Batman.ErrorsSet

    # Find the ID from either the first argument or the attributes.
    if $typeOf(idOrAttributes) is 'Object'
      super(idOrAttributes)
    else
      super()
      @set('id', idOrAttributes)

    @empty() if not @state()

  # Override the `Batman.Observable` implementation of `set` to implement dirty tracking.
  set: (key, value) ->
    # Optimize setting where the value is the same as what's already been set.
    oldValue = @get(key)
    return if oldValue is value

    # Actually set the value and note what the old value was in the tracking array.
    result = super
    @dirtyKeys.set(key, oldValue)

    # Mark the model as dirty if isn't already.
    @dirty() unless @state() in ['dirty', 'loading', 'creating']
    result

  updateAttributes: (attrs) ->
    @mixin(attrs)
    @

  toString: ->
    "#{$functionName(@constructor)}: #{@get('id')}"

  # `toJSON` uses the various encoders for each key to grab a storable representation of the record.
  toJSON: ->
    obj = {}

    # Encode each key into a new object
    encoders = @_batman.get('encoders')
    unless !encoders or encoders.isEmpty()
      encoders.forEach (key, encoder) =>
        val = @get key
        if typeof val isnt 'undefined'
          encodedVal = encoder(val, key, obj, @)
          if typeof encodedVal isnt 'undefined'
            obj[key] = encodedVal

    if @constructor.primaryKey isnt 'id'
      obj[@constructor.primaryKey] = @get('id')
      delete obj.id

    obj

  # `fromJSON` uses the various decoders for each key to generate a record instance from the JSON
  # stored in whichever storage mechanism.
  fromJSON: (data) ->
    obj = {}

    decoders = @_batman.get('decoders')
    # If no decoders were specified, do the best we can to interpret the given JSON by camelizing
    # each key and just setting the values.
    if !decoders or decoders.isEmpty()
      for key, value of data
        obj[key] = value
    else
      # If we do have decoders, use them to get the data.
      decoders.forEach (key, decoder) =>
        obj[key] = decoder(data[key], key, data, obj, @) unless typeof data[key] is 'undefined'

    if @constructor.primaryKey isnt 'id'
      obj.id = data[@constructor.primaryKey]

    developer.do =>
      if (!decoders) || decoders.length <= 1
        developer.warn "Warning: Model #{$functionName(@constructor)} has suspiciously few decoders!"

    # Mixin the buffer object to use optimized and event-preventing sets used by `mixin`.
    @mixin obj

  toParam: -> @get('id')

  # Each model instance (each record) can be in one of many states throughout its lifetime. Since various
  # operations on the model are asynchronous, these states are used to indicate exactly what point the
  # record is at in it's lifetime, which can often be during a save or load operation.
  @actsAsStateMachine yes

  # Add the various states to the model.
  for k in ['empty', 'dirty', 'loading', 'loaded', 'saving', 'saved', 'creating', 'created', 'validating', 'validated', 'destroying', 'destroyed']
    @state k

  for k in ['loading', 'loaded']
    @classState k

  _doStorageOperation: (operation, options, callback) ->
    developer.assert @hasStorage(), "Can't #{operation} model #{$functionName(@constructor)} without any storage adapters!"
    adapters = @_batman.get('storage')
    for adapter in adapters
      adapter.perform operation, @, {data: options}, callback
    true

  hasStorage: -> (@_batman.get('storage') || []).length > 0

  # `load` fetches the record from all sources possible
  load: (callback) =>
    if @state() in ['destroying', 'destroyed']
      callback?(new Error("Can't load a destroyed record!"))
      return

    @loading()
    @_doStorageOperation 'read', {}, (err, record) =>
      unless err
        @loaded()
        record = @constructor._mapIdentity(record)
      callback?(err, record)

  # `save` persists a record to all the storage mechanisms added using `@persist`. `save` will only save
  # a model if it is valid.
  save: (callback) =>
    if @state() in ['destroying', 'destroyed']
      callback?(new Error("Can't save a destroyed record!"))
      return

    @validate (isValid, errors) =>
      if !isValid
        callback?(errors)
        return
      creating = @isNew()

      do @saving
      do @creating if creating

      associations = @constructor._batman.get('associations')
      # Save belongsTo models immediately since we don't need this model's id
      associations?.getByType('belongsTo')?.forEach (association, label) => association.apply(@)

      @_doStorageOperation (if creating then 'create' else 'update'), {}, (err, record) =>
        unless err
          if creating
            do @created
          do @saved
          @dirtyKeys.clear()

          associations?.getByType('hasOne')?.forEach (association) -> association.apply(err, record)
          associations?.getByType('hasMany')?.forEach (association) -> association.apply(err, record)

          record = @constructor._mapIdentity(record)
        callback?(err, record)

  # `destroy` destroys a record in all the stores.
  destroy: (callback) =>
    do @destroying
    @_doStorageOperation 'destroy', {}, (err, record) =>
      unless err
        @constructor.get('all').remove(@)
        do @destroyed
      callback?(err)

  # `validate` performs the record level validations determining the record's validity. These may be asynchronous,
  # in which case `validate` has no useful return value. Results from asynchronous validations can be received by
  # listening to the `afterValidation` lifecycle callback.
  validate: (callback) ->
    oldState = @state()
    @errors.clear()
    do @validating

    finish = () =>
      do @validated
      @[oldState]()
      callback?(@errors.length == 0, @errors)

    validators = @_batman.get('validators') || []
    unless validators.length > 0
      finish()
    else
      count = validators.length
      validationCallback = =>
        if --count == 0
          finish()
      for validator in validators
        v = validator.validator

        # Run the validator `v` or the custom callback on each key it validates by instantiating a new promise
        # and passing it to the appropriate function along with the key and the value to be validated.
        for key in validator.keys
          if v
            v.validateEach @errors, @, key, validationCallback
          else
            validator.callback @errors, @, key, validationCallback
    return

  isNew: -> typeof @get('id') is 'undefined'

# ## Associations
class Batman.AssociationProxy extends Batman.Object
  isProxy: true
  constructor: (@association, @model) ->
  loaded: false

  toJSON: ->
    target = @get('target')
    @get('target').toJSON() if target?

  load: (callback) ->
    @fetch (err, proxiedRecord) =>
      unless err
        @set 'loaded', true
        @set 'target', proxiedRecord
      callback?(err, proxiedRecord)
    @get('target')

  fetch: (callback) ->
    unless (@get('foreignValue') || @get('primaryValue'))?
      return callback(undefined, undefined)
    record = @fetchFromLocal()
    if record
      return callback(undefined, record)
    else
      @fetchFromRemote(callback)

  @accessor 'loaded', Batman.Property.defaultAccessor

  @accessor 'target',
    get: -> @fetchFromLocal()
    set: (_, v) -> v # This just needs to bust the cache

  @accessor
    get: (k) -> @get('target')?.get(k)
    set: (k, v) -> @get('target')?.set(k, v)

class Batman.BelongsToProxy extends Batman.AssociationProxy
  @accessor 'foreignValue', -> @model.get(@association.foreignKey)

  fetchFromLocal: -> @association.setIndex().get(@get('foreignValue'))

  fetchFromRemote: (callback) ->
    @association.getRelatedModel().find @get('foreignValue'), (error, loadedRecord) =>
      throw error if error
      callback undefined, loadedRecord

class Batman.HasOneProxy extends Batman.AssociationProxy
  @accessor 'primaryValue', -> @model.get(@association.primaryKey)

  fetchFromLocal: -> @association.setIndex().get(@get('primaryValue'))

  fetchFromRemote: (callback) ->
    loadOptions = {}
    loadOptions[@association.foreignKey] = @get('primaryValue')
    @association.getRelatedModel().load loadOptions, (error, loadedRecords) =>
      throw error if error
      if !loadedRecords or loadedRecords.length <= 0
        callback new Error("Couldn't find related record!"), undefined
      else
        callback undefined, loadedRecords[0]

class Batman.AssociationSet extends Batman.SetSort
  constructor: (@value, @association) ->
    base = new Batman.Set
    super(base, 'hashKey')
  loaded: false
  load: (callback) ->
    return callback(undefined, @) unless @value?
    loadOptions = {}
    loadOptions[@association.foreignKey] = @value
    @association.getRelatedModel().load loadOptions, (err, records) =>
      @loaded = true unless err
      @fire 'loaded'
      callback(err, @)

class Batman.UniqueAssociationSetIndex extends Batman.UniqueSetIndex
  constructor: (@association, key) ->
    super @association.getRelatedModel().get('loaded'), key

class Batman.AssociationSetIndex extends Batman.SetIndex
  constructor: (@association, key) ->
    super @association.getRelatedModel().get('loaded'), key

  _resultSetForKey: (key) ->
    @_storage.getOrSet key, =>
      new Batman.AssociationSet(key, @association)

  _setResultSet: (key, set) ->
    @_storage.set key, set

class Batman.AssociationCurator extends Batman.SimpleHash
  @availableAssociations: ['belongsTo', 'hasOne', 'hasMany']
  constructor: (@model) ->
    super()
    # Contains (association, label) pairs mapped by association type
    # ie. @storage = {<Association.associationType>: [<Association>, <Association>]}
    @_byTypeStorage = new Batman.SimpleHash

  add: (association) ->
    @set association.label, association
    unless associationTypeSet = @_byTypeStorage.get(association.associationType)
      associationTypeSet = new Batman.SimpleSet
      @_byTypeStorage.set association.associationType, associationTypeSet
    associationTypeSet.add association

  getByType: (type) -> @_byTypeStorage.get(type)
  getByLabel: (label) -> @get(label)

  reset: ->
    @forEach (label, association) -> association.reset()
    true

  merge: (others...) ->
    result = super
    result._byTypeStorage = @_byTypeStorage.merge(others.map (other) -> other._byTypeStorage)
    result

class Batman.Association
  associationType: ''
  defaultOptions:
    saveInline: true
    autoload: true

  constructor: (@model, @label, options = {}) ->
    defaultOptions =
      namespace: Batman.currentApp
      name: helpers.camelize(helpers.singularize(@label))
    @options = $mixin defaultOptions, @defaultOptions, options

    # Setup encoders and accessors for this association. The accessor needs reference to this
    # association object, so curry the association info into the getAccessor, which has the
    # model applied as the context
    @model.encode label, @encoder()

    self = @
    getAccessor = -> return self.getAccessor.call(@, self, @model, @label)
    @model.accessor @label,
      get: getAccessor
      set: model.defaultAccessor.set
      unset: model.defaultAccessor.unset

    if @url
      @model.url ||= (recordOptions) ->
        return self.url(recordOptions)

  getRelatedModel: ->
    scope = @options.namespace or Batman.currentApp
    modelName = @options.name
    relatedModel = scope?[modelName]
    developer.do ->
      if Batman.currentApp? and not relatedModel
        developer.warn "Related model #{modelName} hasn't loaded yet."
    relatedModel

  getFromAttributes: (record) ->
    record.constructor.defaultAccessor.get.call(record, @label)
  setIntoAttributes: (record, value) ->
    record.constructor.defaultAccessor.set.call(record, @label, value)

  encoder: -> developer.error "You must override encoder in Batman.Association subclasses."
  setIndex: -> developer.error "You must override setIndex in Batman.Association subclasses."
  inverse: ->
    if relatedAssocs = @getRelatedModel()._batman.get('associations')
      if @options.inverseOf
        return relatedAssocs.getByLabel(@options.inverseOf)

      inverse = null
      relatedAssocs.forEach (label, assoc) =>
        if assoc.getRelatedModel() is @model
          inverse = assoc
      inverse

  reset: ->
    delete @index
    true

class Batman.SingularAssociation extends Batman.Association
  isSingular: true

  getAccessor: (self, model, label) ->
    # Check whether the relation has already been set on this model
    if recordInAttributes = self.getFromAttributes(@)
      return recordInAttributes

    # Make sure the related model has been loaded
    if self.getRelatedModel()
      proxy = @associationProxy(self)
      Batman.Property.withoutTracking ->
        if not proxy.get('loaded') and self.options.autoload
          proxy.load()
      proxy

  setIndex: ->
    @index ||= new Batman.UniqueAssociationSetIndex(@, @[@indexRelatedModelOn])
    @index

class Batman.PluralAssociation extends Batman.Association
  isSingular: false

  setForRecord: (record) ->
    Batman.Property.withoutTracking =>
      if id = record.get(@primaryKey)
        @setIndex().get(id)
      else
        new Batman.AssociationSet(undefined, @)

  getAccessor: (self, model, label) ->
    return unless self.getRelatedModel()

    # Check whether the relation has already been set on this model
    if setInAttributes = self.getFromAttributes(@)
      setInAttributes
    else
      relatedRecords = self.setForRecord(@)
      self.setIntoAttributes(@, relatedRecords)

      Batman.Property.withoutTracking =>
        if self.options.autoload and not @isNew() and not relatedRecords.loaded
          relatedRecords.load (error, records) -> throw error if error

      relatedRecords

  setIndex: ->
    @index ||= new Batman.AssociationSetIndex(@, @[@indexRelatedModelOn])
    @index

class Batman.BelongsToAssociation extends Batman.SingularAssociation
  associationType: 'belongsTo'
  proxyClass: Batman.BelongsToProxy
  indexRelatedModelOn: 'primaryKey'
  defaultOptions:
    saveInline: false
    autoload: true

  constructor: ->
    super
    @foreignKey = @options.foreignKey or "#{@label}_id"
    @primaryKey = @options.primaryKey or "id"
    @model.encode @foreignKey

  url: (recordOptions) ->
    if inverse = @inverse()
      root = Batman.helpers.pluralize(@label)
      id = recordOptions.data?["#{@label}_id"]
      helper = if inverse.isSingular then "singularize" else "pluralize"
      ending = Batman.helpers[helper](inverse.label)

      return "/#{root}/#{id}/#{ending}"

  encoder: ->
    association = @
    encoder =
      encode: false
      decode: (data, _, __, ___, childRecord) ->
        relatedModel = association.getRelatedModel()
        record = new relatedModel()
        record.fromJSON(data)
        record = relatedModel._mapIdentity(record)
        if association.options.inverseOf
          if inverse = association.inverse()
            if inverse instanceof Batman.HasManyAssociation
              # Rely on the parent's set index to get this out.
              childRecord.set(association.foreignKey, record.get(association.primaryKey))
            else
              record.set(inverse.label, childRecord)
        childRecord.set(association.label, record)
        record
    if @options.saveInline
      encoder.encode = (val) -> val.toJSON()
    encoder

  apply: (base) ->
    if model = base.get(@label)
      foreignValue = model.get(@primaryKey)
      if foreignValue isnt undefined
        base.set @foreignKey, foreignValue

class Batman.HasOneAssociation extends Batman.SingularAssociation
  associationType: 'hasOne'
  proxyClass: Batman.HasOneProxy
  indexRelatedModelOn: 'foreignKey'

  constructor: ->
    super
    @primaryKey = @options.primaryKey or "id"
    @foreignKey = @options.foreignKey or "#{helpers.underscore($functionName(@model))}_id"

  apply: (baseSaveError, base) ->
    if relation = @getFromAttributes(base)
      relation.set @foreignKey, base.get(@primaryKey)

  encoder: ->
    association = @
    return {
      encode: (val, key, object, record) ->
        return unless association.options.saveInline
        if json = val.toJSON()
          json[association.foreignKey] = record.get(association.primaryKey)
        json
      decode: (data, _, __, ___, parentRecord) ->
        relatedModel = association.getRelatedModel()
        record = new (relatedModel)()
        record.fromJSON(data)
        if association.options.inverseOf
          record.set association.options.inverseOf, parentRecord
        record = relatedModel._mapIdentity(record)
        record
    }

class Batman.HasManyAssociation extends Batman.PluralAssociation
  associationType: 'hasMany'
  indexRelatedModelOn: 'foreignKey'

  constructor: ->
    super
    @primaryKey = @options.primaryKey or "id"
    @foreignKey = @options.foreignKey or "#{helpers.underscore($functionName(@model))}_id"

  apply: (baseSaveError, base) ->
    unless baseSaveError
      if relations = @getFromAttributes(base)
        relations.forEach (model) =>
          model.set @foreignKey, base.get(@primaryKey)
      base.set @label, @setForRecord(base)

  encoder: ->
    association = @
    return {
      encode: (relationSet, _, __, record) ->
        return if association._beingEncoded
        association._beingEncoded = true

        return unless association.options.saveInline
        if relationSet?
          jsonArray = []
          relationSet.forEach (relation) ->
            relationJSON = relation.toJSON()
            relationJSON[association.foreignKey] = record.get(association.primaryKey)
            jsonArray.push relationJSON

        delete association._beingEncoded
        jsonArray

      decode: (data, key, _, __, parentRecord) ->
        if relatedModel = association.getRelatedModel()
          existingRelations = association.getFromAttributes(parentRecord) || association.setForRecord(parentRecord)
          existingArray = existingRelations?.toArray()
          for jsonObject, i in data
            record = if existingArray && existingArray[i]
              existing = true
              existingArray[i]
            else
              existing = false
              new relatedModel()
            record.fromJSON jsonObject

            if association.options.inverseOf
              record.set association.options.inverseOf, parentRecord

            record = relatedModel._mapIdentity(record)
            existingRelations.add record
        else
          developer.error "Can't decode model #{association.options.name} because it hasn't been loaded yet!"
        existingRelations
    }

# ### Model Associations API
for k in Batman.AssociationCurator.availableAssociations
  do (k) =>
    Batman.Model[k] = (label, scope) ->
      Batman.initializeObject(@)
      collection = @_batman.associations ||= new Batman.AssociationCurator(@)
      collection.add new Batman["#{helpers.capitalize(k)}Association"](@, label, scope)

class Batman.ValidationError extends Batman.Object
  constructor: (attribute, message) -> super({attribute, message})

# `ErrorSet` is a simple subclass of `Set` which makes it a bit easier to
# manage the errors on a model.
class Batman.ErrorsSet extends Batman.Set
  # Define a default accessor to get the set of errors on a key
  @accessor (key) -> @indexedBy('attribute').get(key)

  # Define a shorthand method for adding errors to a key.
  add: (key, error) -> super(new Batman.ValidationError(key, error))

class Batman.Validator extends Batman.Object
  constructor: (@options, mixins...) ->
    super mixins...

  validate: (record) -> developer.error "You must override validate in Batman.Validator subclasses."
  format: (key, messageKey, interpolations) -> t('errors.format', {attribute: key, message: t("errors.messages.#{messageKey}", interpolations)})

  @options: (options...) ->
    Batman.initializeObject @
    if @_batman.options then @_batman.options.concat(options) else @_batman.options = options

  @matches: (options) ->
    results = {}
    shouldReturn = no
    for key, value of options
      if ~@_batman?.options?.indexOf(key)
        results[key] = value
        shouldReturn = yes
    return results if shouldReturn

Validators = Batman.Validators = [
  class Batman.LengthValidator extends Batman.Validator
    @options 'minLength', 'maxLength', 'length', 'lengthWithin', 'lengthIn'

    constructor: (options) ->
      if range = (options.lengthIn or options.lengthWithin)
        options.minLength = range[0]
        options.maxLength = range[1] || -1
        delete options.lengthWithin
        delete options.lengthIn

      super

    validateEach: (errors, record, key, callback) ->
      options = @options
      value = record.get(key) ? []
      if options.minLength and value.length < options.minLength
        errors.add key, @format(key, 'too_short', {count: options.minLength})
      if options.maxLength and value.length > options.maxLength
        errors.add key, @format(key, 'too_long', {count: options.maxLength})
      if options.length and value.length isnt options.length
        errors.add key, @format(key, 'wrong_length', {count: options.length})
      callback()

  class Batman.PresenceValidator extends Batman.Validator
    @options 'presence'
    validateEach: (errors, record, key, callback) ->
      value = record.get(key)
      if @options.presence && (!value? || value is '')
        errors.add key, @format(key, 'blank')
      callback()
]

$mixin Batman.translate.messages,
  errors:
    format: "%{attribute} %{message}"
    messages:
      too_short: "must be at least %{count} characters"
      too_long: "must be less than %{count} characters"
      wrong_length: "must be %{count} characters"
      blank: "can't be blank"

class Batman.StorageAdapter extends Batman.Object

  class @StorageError extends Error
    name: "StorageError"
    constructor: (message) ->
      super
      @message = message

  class @RecordExistsError extends @StorageError
    name: 'RecordExistsError'
    constructor: (message) ->
      super(message || "Can't create this record because it already exists in the store!")

  class @NotFoundError extends @StorageError
    name: 'NotFoundError'
    constructor: (message) ->
      super(message || "Record couldn't be found in storage!")

  constructor: (model) -> super(model: model)

  isStorageAdapter: true

  storageKey: (record) ->
    model = record?.constructor || @model
    model.get('storageKey') || helpers.pluralize(helpers.underscore($functionName(model)))

  getRecordFromData: (attributes, constructor = @model) ->
    record = new constructor()
    record.fromJSON(attributes)
    record

  @skipIfError: (f) ->
    return (env, next) ->
      if env.error?
        next()
      else
        f.call(@, env, next)

  before: -> @_addFilter('before', arguments...)
  after: -> @_addFilter('after', arguments...)

  _inheritFilters: ->
    if !@_batman.check(@) || !@_batman.filters
      oldFilters = @_batman.getFirst('filters')
      @_batman.filters = {before: {}, after: {}}
      if oldFilters?
        for position, filtersByKey of oldFilters
          for key, filtersList of filtersByKey
            @_batman.filters[position][key] = filtersList.slice(0)

  _addFilter: (position, keys..., filter) ->
    @_inheritFilters()
    for key in keys
      @_batman.filters[position][key] ||= []
      @_batman.filters[position][key].push filter
    true

  runFilter: (position, action, env, callback) ->
    @_inheritFilters()
    allFilters = @_batman.filters[position].all || []
    actionFilters = @_batman.filters[position][action] || []
    env.action = action

    # Action specific filters execute first, and then the `all` filters.
    filters = actionFilters.concat(allFilters)
    next = (newEnv) =>
      env = newEnv if newEnv?
      if (nextFilter = filters.shift())?
        nextFilter.call @, env, next
      else
        callback.call @, env

    next()

  runBeforeFilter: -> @runFilter 'before', arguments...
  runAfterFilter: (action, env, callback) -> @runFilter 'after', action, env, @exportResult(callback)
  exportResult: (callback) -> (env) -> callback(env.error, env.result, env)

  _jsonToAttributes: (json) -> JSON.parse(json)

  perform: (key, recordOrProto, options, callback) ->
    options ||= {}
    env = {options}
    if key == 'readAll'
      env.proto = recordOrProto
    else
      env.record = recordOrProto

    next = (newEnv) =>
      env = newEnv if newEnv?
      @runAfterFilter key, env, callback

    @runBeforeFilter key, env, (env) ->
      @[key](env, next)

class Batman.LocalStorage extends Batman.StorageAdapter
  constructor: ->
    return null if typeof window.localStorage is 'undefined'
    super
    @storage = localStorage

  storageRegExpForRecord: (record) -> new RegExp("^#{@storageKey(record)}(\\d+)$")

  nextIdForRecord: (record) ->
    re = @storageRegExpForRecord(record)
    nextId = 1
    @_forAllStorageEntries (k, v) ->
      if matches = re.exec(k)
        nextId = Math.max(nextId, parseInt(matches[1], 10) + 1)
    nextId

  _forAllStorageEntries: (iterator) ->
    for i in [0...@storage.length]
      key = @storage.key(i)
      iterator.call(@, key, @storage.getItem(key))
    true

  _storageEntriesMatching: (proto, options) ->
    re = @storageRegExpForRecord(proto)
    records = []
    @_forAllStorageEntries (storageKey, storageString) ->
      if keyMatches = re.exec(storageKey)
        data = @_jsonToAttributes(storageString)
        data[proto.constructor.primaryKey] = keyMatches[1]
        records.push data if @_dataMatches(options, data)
    records

  _dataMatches: (conditions, data) ->
    match = true
    for k, v of conditions
      if data[k] != v
        match = false
        break
    match

  @::before 'read', 'create', 'update', 'destroy', @skipIfError (env, next) ->
    if env.action == 'create'
      env.id = env.record.get('id') || env.record.set('id', @nextIdForRecord(env.record))
    else
      env.id = env.record.get('id')

    unless env.id?
      env.error = new @constructor.StorageError("Couldn't get/set record primary key on #{env.action}!")
    else
      env.key = @storageKey(env.record) + env.id

    next()

  @::before 'create', 'update', @skipIfError (env, next) ->
    env.recordAttributes = JSON.stringify(env.record)
    next()

  @::after 'read', @skipIfError (env, next) ->
    if typeof env.recordAttributes is 'string'
      try
        env.recordAttributes = @_jsonToAttributes(env.recordAttributes)
      catch error
        env.error = error
        return next()
    env.record.fromJSON env.recordAttributes
    next()

  @::after 'read', 'create', 'update', 'destroy', @skipIfError (env, next) ->
    env.result = env.record
    next()

  @::after 'readAll', @skipIfError (env, next) ->
    env.result = env.records = for recordAttributes in env.recordsAttributes
      @getRecordFromData(recordAttributes, env.proto.constructor)
    next()

  read: @skipIfError (env, next) ->
    env.recordAttributes = @storage.getItem(env.key)
    if !env.recordAttributes
      env.error = new @constructor.NotFoundError()
    next()

  create: @skipIfError ({key, recordAttributes}, next) ->
    if @storage.getItem(key)
      arguments[0].error = new @constructor.RecordExistsError
    else
      @storage.setItem(key, recordAttributes)
    next()

  update: @skipIfError ({key, recordAttributes}, next) ->
    @storage.setItem(key, recordAttributes)
    next()

  destroy: @skipIfError ({key}, next) ->
    @storage.removeItem(key)
    next()

  readAll: @skipIfError ({proto, options}, next) ->
    try
      arguments[0].recordsAttributes = @_storageEntriesMatching(proto, options.data)
    catch error
      arguments[0].error = error
    next()

class Batman.SessionStorage extends Batman.LocalStorage
  constructor: ->
    if typeof window.sessionStorage is 'undefined'
      return null
    super
    @storage = sessionStorage

class Batman.RestStorage extends Batman.StorageAdapter
  @JSONContentType: 'application/json'
  @PostBodyContentType: 'application/x-www-form-urlencoded'

  defaultRequestOptions:
    type: 'json'

  serializeAsForm: true

  constructor: ->
    super
    @defaultRequestOptions = $mixin {}, @defaultRequestOptions

  recordJsonNamespace: (record) -> helpers.singularize(@storageKey(record))
  collectionJsonNamespace: (proto) -> helpers.pluralize(@storageKey(proto))

  _execWithOptions: (object, key, options) -> if typeof object[key] is 'function' then object[key](options) else object[key]
  _defaultCollectionUrl: (record) -> "/#{@storageKey(record)}"

  urlForRecord: (record, env) ->
    if record.url
      url = @_execWithOptions(record, 'url', env.options)
    else
      url = if record.constructor.url
        @_execWithOptions(record.constructor, 'url', env.options)
      else
        @_defaultCollectionUrl(record)

      if env.action != 'create'
        if (id = record.get('id'))?
          url = url + "/" + id
        else
          throw new @constructor.StorageError("Couldn't get/set record primary key on #{env.action}!")

    @urlPrefix(record, env) + url + @urlSuffix(record, env)

  urlForCollection: (model, env) ->
    url = if model.url
      @_execWithOptions(model, 'url', env.options)
    else
      @_defaultCollectionUrl(model::, env.options)

    @urlPrefix(model, env) + url + @urlSuffix(model, env)

  urlPrefix: (object, env) ->
    @_execWithOptions(object, 'urlPrefix', env.options) || ''

  urlSuffix: (object, env) ->
    @_execWithOptions(object, 'urlSuffix', env.options) || ''

  request: (env, next) ->
    options = $mixin env.options,
      success: (data) -> env.data = data
      error: (error) -> env.error = error
      loaded: ->
        env.response = req.get('response')
        next()

    req = new Batman.Request(options)

  perform: (key, record, options, callback) ->
    $mixin (options ||= {}), @defaultRequestOptions
    super

  @::before 'create', 'read', 'update', 'destroy', @skipIfError (env, next) ->
    try
      env.options.url = @urlForRecord(env.record, env)
    catch error
      env.error = error
    next()

  @::before 'readAll', @skipIfError (env, next) ->
    try
      env.options.url = @urlForCollection(env.proto.constructor, env)
    catch error
      env.error = error
    next()

  @::before 'create', 'update', @skipIfError (env, next) ->
    json = env.record.toJSON()
    if namespace = @recordJsonNamespace(env.record)
      data = {}
      data[namespace] = json
    else
      data = json

    if @serializeAsForm
      # Leave the POJO in the data for the request adapter to serialize to a body
      env.options.contentType = @constructor.PostBodyContentType
    else
      data = JSON.stringify(data)
      env.options.contentType = @constructor.JSONContentType

    env.options.data = data
    next()

  @::after 'create', 'read', 'update', @skipIfError (env, next) ->
    if typeof env.data is 'string'
      try
        json = @_jsonToAttributes(env.data)
      catch error
        env.error = error
        return next()
    else
      json = env.data
    namespace = @recordJsonNamespace(env.record)
    json = json[namespace] if namespace && json[namespace]?
    env.record.fromJSON(json)
    env.result = env.record
    next()

  @::after 'readAll', @skipIfError (env, next) ->
    if typeof env.data is 'string'
      try
        env.data = JSON.parse(env.env)
      catch jsonError
        env.error = jsonError
        return next()

    namespace = @collectionJsonNamespace(env.proto)
    env.recordsAttributes = if namespace && env.data[namespace]?
      env.data[namespace]
    else
      env.data

    env.result = env.records = for jsonRecordAttributes in env.recordsAttributes
      @getRecordFromData(jsonRecordAttributes, env.proto.constructor)
    next()

  @HTTPMethods =
    create: 'POST'
    update: 'PUT'
    read: 'GET'
    readAll: 'GET'
    destroy: 'DELETE'

  for key in ['create', 'read', 'update', 'destroy', 'readAll']
    do (key) =>
      @::[key] = @skipIfError (env, next) ->
        env.options.method = @constructor.HTTPMethods[key]
        @request(env, next)

# Views
# -----------

class Batman.ViewStore extends Batman.Object
  @prefix: 'views'

  constructor: ->
    super
    @_viewContents = {}
    @_requestedPaths = new Batman.SimpleSet

  propertyClass: Batman.Property

  fetchView: (path) ->
    developer.do ->
      unless typeof Batman.View::prefix is 'undefined'
        developer.warn "Batman.View.prototype.prefix has been removed, please use Batman.ViewStore.prefix instead."
    new Batman.Request
      url: Batman.Navigator.normalizePath(@constructor.prefix, "#{path}.html")
      type: 'html'
      success: (response) => @set(path, response)
      error: (response) -> throw new Error("Could not load view from #{path}")

  @accessor
    'final': true
    get: (path) ->
      return @get("/#{path}") unless path[0] is '/'
      return @_viewContents[path] if @_viewContents[path]
      return if @_requestedPaths.has(path)
      @fetchView(path)
      return
    set: (path, content) ->
      return @set("/#{path}", content) unless path[0] is '/'
      @_requestedPaths.add(path)
      @_viewContents[path] = content

  prefetch: (path) ->
    @get(path)
    true

# A `Batman.View` can function two ways: a mechanism to load and/or parse html files
# or a root of a subclass hierarchy to create rich UI classes, like in Cocoa.
class Batman.View extends Batman.Object
  isView: true
  constructor: (options = {}) ->
    context = options.context
    if context
      unless context instanceof Batman.RenderContext
        context = Batman.RenderContext.root().descend(context)
    else
      context = Batman.RenderContext.root()
    options.context = context.descend(@)
    super(options)

    # Start the rendering by asking for the node
    Batman.Property.withoutTracking =>
      if node = @get('node')
        @render node
      else
        @observe 'node', (node) => @render(node)

  @store: new Batman.ViewStore()

  # Set the source attribute to an html file to have that file loaded.
  source: ''

  # Set the html to a string of html to have that html parsed.
  html: ''

  # Set an existing DOM node to parse immediately.
  node: null

  # Fires once a node is parsed.
  @::event('ready').oneShot = true

  @accessor 'html',
    get: ->
      return @html if @html && @html.length > 0
      return unless source = @get 'source'
      source = Batman.Navigator.normalizePath(source)
      @html = @constructor.store.get(source)
    set: (_, html) -> @html = html

  @accessor 'node'
    get: ->
      unless @node
        html = @get('html')
        return unless html && html.length > 0
        @hasContainer = true
        @node = document.createElement 'div'
        $setInnerHTML(@node, html)
        if @node.children.length > 0
          Batman.data(@node.children[0], 'view', @)
      return @node
    set: (_, node) ->
      @node = node
      Batman.data(@node, 'view', @)
      updateHTML = (html) =>
        if html?
          $setInnerHTML(@get('node'), html)
          @forget('html', updateHTML)
      @observeAndFire 'html', updateHTML

  render: (node) ->
    @event('ready').resetOneShot()
    @_renderer?.forgetAll()

    # We use a renderer with the continuation style rendering engine to not
    # block user interaction for too long during the render.
    if node
      @_renderer = new Batman.Renderer(node, null, @context, @)
      @_renderer.on 'rendered', => @fire('ready', node)

  @::on 'appear', -> @viewDidAppear? arguments...
  @::on 'disappear', -> @viewDidDisappear? arguments...
  @::on 'beforeAppear', -> @viewWillAppear? arguments...
  @::on 'beforeDisappear', -> @viewWillDisappear? arguments...

# DOM Helpers
# -----------

# `Batman.Renderer` will take a node and parse all recognized data attributes out of it and its children.
# It is a continuation style parser, designed not to block for longer than 50ms at a time if the document
# fragment is particularly long.
class Batman.Renderer extends Batman.Object
  deferEvery: 50

  constructor: (@node, callback, @context, view) ->
    super()
    @on('parsed', callback) if callback?
    developer.error "Must pass a RenderContext to a renderer for rendering" unless @context instanceof Batman.RenderContext
    @immediate = $setImmediate @start

  start: =>
    @startTime = new Date
    @parseNode @node

  resume: =>
    @startTime = new Date
    @parseNode @resumeNode

  finish: ->
    @startTime = null
    @prevent 'stopped'
    @fire 'parsed'
    @fire 'rendered'

  stop: ->
    $clearImmediate @immediate
    @fire 'stopped'

  forgetAll: ->

  for k in ['parsed', 'rendered', 'stopped']
    @::event(k).oneShot = true

  bindingRegexp = /^data\-(.*)/

  bindingSortOrder = ["renderif", "foreach", "formfor", "context", "bind", "source", "target"]

  bindingSortPositions = {}
  bindingSortPositions[name] = pos for name, pos in bindingSortOrder

  _sortBindings: (a,b) ->
    aindex = bindingSortPositions[a[0]]
    bindex = bindingSortPositions[b[0]]
    aindex ?= bindingSortOrder.length # put unspecified bindings last
    bindex ?= bindingSortOrder.length
    if aindex > bindex
      1
    else if bindex > aindex
      -1
    else if a[0] > b[0]
      1
    else if b[0] > a[0]
      -1
    else
      0

  parseNode: (node) ->
    if @deferEvery && (new Date - @startTime) > @deferEvery
      @resumeNode = node
      @timeout = $setImmediate @resume
      return

    if node.getAttribute and node.attributes
      bindings = for attr in node.attributes
        name = attr.nodeName.match(bindingRegexp)?[1]
        continue if not name
        if ~(varIndex = name.indexOf('-'))
          [name.substr(0, varIndex), name.substr(varIndex + 1), attr.value]
        else
          [name, attr.value]

      for readerArgs in bindings.sort(@_sortBindings)
        key = readerArgs[1]
        result = if readerArgs.length == 2
          Batman.DOM.readers[readerArgs[0]]?(node, key, @context, @)
        else
          Batman.DOM.attrReaders[readerArgs[0]]?(node, key, readerArgs[2], @context, @)

        if result is false
          skipChildren = true
          break
        else if result instanceof Batman.RenderContext
          oldContext = @context
          @context = result
          $onParseExit(node, => @context = oldContext)

    if (nextNode = @nextNode(node, skipChildren)) then @parseNode(nextNode) else @finish()

  nextNode: (node, skipChildren) ->
    if not skipChildren
      children = node.childNodes
      return children[0] if children?.length

    sibling = node.nextSibling # Grab the reference before onParseExit may remove the node
    $onParseExit(node).forEach (callback) -> callback()
    $forgetParseExit(node)
    return if @node == node
    return sibling if sibling

    nextParent = node
    while nextParent = nextParent.parentNode
      $onParseExit(nextParent).forEach (callback) -> callback()
      $forgetParseExit(nextParent)
      return if @node == nextParent

      parentSibling = nextParent.nextSibling
      return parentSibling if parentSibling

    return

# The RenderContext class manages the stack of contexts accessible to a view during rendering.
class Batman.RenderContext
  @deProxy: (object) -> if object? && object.isContextProxy then object.get('proxiedObject') else object
  @root: ->
    if Batman.currentApp?
      Batman.currentApp.get('_renderContext')
    else
      @base
  constructor: (@object, @parent) ->

  findKey: (key) ->
    base = key.split('.')[0].split('|')[0].trim()
    currentNode = @
    while currentNode
      # We define the behaviour of the context stack as latching a get when the first key exists,
      # so we need to check only if the basekey exists, not if all intermediary keys do as well.
      val = $get(currentNode.object, base)
      if typeof val isnt 'undefined'
        val = $get(currentNode.object, key)
        return [val, currentNode.object].map(@constructor.deProxy)
      currentNode = currentNode.parent

    @windowWrapper ||= window: Batman.container
    [$get(@windowWrapper, key), @windowWrapper]

  get: (key) -> @findKey(key)[0]

  # Below are the three primitives that all the `Batman.DOM` helpers are composed of.
  # `descend` takes an `object`, and optionally a `scopedKey`. It creates a new `RenderContext` leaf node
  # in the tree with either the object available on the stack or the object available at the `scopedKey`
  # on the stack.
  descend: (object, scopedKey) ->
    if scopedKey
      oldObject = object
      object = new Batman.Object()
      object[scopedKey] = oldObject
    return new @constructor(object, @)

  # `descendWithKey` takes a `key` and optionally a `scopedKey`. It creates a new `RenderContext` leaf node
  # with the runtime value of the `key` available on the stack or under the `scopedKey` if given. This
  # differs from a normal `descend` in that it looks up the `key` at runtime (in the parent `RenderContext`)
  # and will correctly reflect changes if the value at the `key` changes. A normal `descend` takes a concrete
  # reference to an object which never changes.
  descendWithKey: (key, scopedKey) ->
   proxy = new ContextProxy(@, key)
   return @descend(proxy, scopedKey)

  # `chain` flattens a `RenderContext`'s path to the root.
  chain: ->
    x = []
    parent = this
    while parent
      x.push parent.object
      parent = parent.parent
    x

  # `ContextProxy` is a simple class which assists in pushing dynamic contexts onto the `RenderContext` tree.
  # This happens when a `data-context` is descended into, for each iteration in a `data-foreach`,
  # and in other specific HTML bindings like `data-formfor`. `ContextProxy`s use accessors so that if the
  # value of the object they proxy changes, the changes will be propagated to any thing observing the `ContextProxy`.
  # This is good because it allows `data-context` to take keys which will change, filtered keys, and even filters
  # which take keypath arguments. It will calculate the context to descend into when any of those keys change
  # because it preserves the property of a binding, and importantly it exposes a friendly `Batman.Object`
  # interface for the rest of the `Binding` code to work with.
  @ContextProxy = class ContextProxy extends Batman.Object
    isContextProxy: true

    # Reveal the binding's final value.
    @accessor 'proxiedObject', -> @binding.get('filteredValue')
    # Proxy all gets to the proxied object.
    @accessor
      get: (key) -> @get("proxiedObject.#{key}")
      set: (key, value) -> @set("proxiedObject.#{key}", value)
      unset: (key) -> @unset("proxiedObject.#{key}")

    constructor: (@renderContext, @keyPath, @localKey) ->
      @binding = new Batman.DOM.AbstractBinding(undefined, @keyPath, @renderContext)

Batman.RenderContext.base = new Batman.RenderContext(window: Batman.container)

Batman.DOM = {
  # `Batman.DOM.readers` contains the functions used for binding a node's value or innerHTML, showing/hiding nodes,
  # and any other `data-#{name}=""` style DOM directives.
  readers: {
    target: (node, key, context, renderer) ->
      Batman.DOM.readers.bind(node, key, context, renderer, 'nodeChange')
      true

    source: (node, key, context, renderer) ->
      Batman.DOM.readers.bind(node, key, context, renderer, 'dataChange')
      true

    bind: (node, key, context, renderer, only) ->
      bindingClass = false
      switch node.nodeName.toLowerCase()
        when 'input'
          switch node.getAttribute('type')
            when 'checkbox'
              Batman.DOM.attrReaders.bind(node, 'checked', key, context, renderer, only)
              return true
            when 'radio'
              bindingClass = Batman.DOM.RadioBinding
            when 'file'
              bindingClass = Batman.DOM.FileBinding
        when 'select'
          bindingClass = Batman.DOM.SelectBinding
      bindingClass ||= Batman.DOM.Binding
      new bindingClass(arguments...)
      true

    context: (node, key, context, renderer) -> return context.descendWithKey(key)

    mixin: (node, key, context, renderer) ->
      new Batman.DOM.MixinBinding(node, key, context.descend(Batman.mixins), renderer)
      true

    showif: (node, key, context, parentRenderer, invert) ->
      new Batman.DOM.ShowHideBinding(node, key, context, parentRenderer, false, invert)
      true

    hideif: -> Batman.DOM.readers.showif(arguments..., yes)

    route: ->
      new Batman.DOM.RouteBinding(arguments...)
      true

    view: ->
      new Batman.DOM.ViewBinding arguments...
      false

    partial: (node, path, context, renderer) ->
      Batman.DOM.partial node, path, context, renderer
      true

    defineview: (node, name, context, renderer) ->
      $onParseExit(node, -> $removeNode(node))
      Batman.View.store.set(Batman.Navigator.normalizePath(name), node.innerHTML)
      false

    renderif: (node, key, context, renderer) ->
      new Batman.DOM.DeferredRenderingBinding(node, key, context, renderer)
      false

    yield: (node, key) ->
      $setImmediate -> Batman.DOM.yield key, node
      true
    contentfor: (node, key) ->
      $setImmediate -> Batman.DOM.contentFor key, node
      true
    replace: (node, key) ->
      $setImmediate -> Batman.DOM.replace key, node
      true
  }
  _yielders: {} # name/fn pairs of yielding functions
  _yields: {}   # name/container pairs of yielding nodes

  # `Batman.DOM.attrReaders` contains all the DOM directives which take an argument in their name, in the
  # `data-dosomething-argument="keypath"` style. This means things like foreach, binding attributes like
  # disabled or anything arbitrary, descending into a context, binding specific classes, or binding to events.
  attrReaders: {
    _parseAttribute: (value) ->
      if value is 'false' then value = false
      if value is 'true' then value = true
      value

    source: (node, attr, key, context, renderer) ->
      Batman.DOM.attrReaders.bind node, attr, key, context, renderer, 'dataChange'

    bind: (node, attr, key, context, renderer, only) ->
      bindingClass = switch attr
        when 'checked', 'disabled', 'selected'
          Batman.DOM.CheckedBinding
        when 'value', 'href', 'src', 'size'
          Batman.DOM.NodeAttributeBinding
        when 'class'
          Batman.DOM.ClassBinding
        when 'style'
          Batman.DOM.StyleBinding
        else
          Batman.DOM.AttributeBinding
      new bindingClass(arguments...)
      true

    context: (node, contextName, key, context) -> return context.descendWithKey(key, contextName)

    event: (node, eventName, key, context) ->
      new Batman.DOM.EventBinding(arguments...)
      true

    addclass: (node, className, key, context, parentRenderer, invert) ->
      new Batman.DOM.AddClassBinding(node, className, key, context, parentRenderer, false, invert)
      true

    removeclass: (node, className, key, context, parentRenderer) -> Batman.DOM.attrReaders.addclass node, className, key, context, parentRenderer, yes

    foreach: (node, iteratorName, key, context, parentRenderer) ->
      new Batman.DOM.IteratorBinding(arguments...)
      false # Return false so the Renderer doesn't descend into this node's children.

    formfor: (node, localName, key, context) ->
      new Batman.DOM.FormBinding(arguments...)
      context.descendWithKey(key, localName)

    view: (node, bindKey, contextKey, context) ->
      [_, parent] = context.findKey contextKey
      view = null
      parent.observeAndFire contextKey, (newValue) ->
        view ||= Batman.data node, 'view'
        view?.set bindKey, newValue
  }

  # `Batman.DOM.events` contains the helpers used for binding to events. These aren't called by
  # DOM directives, but are used to handle specific events by the `data-event-#{name}` helper.
  events: {
    click: (node, callback, context, eventName = 'click') ->
      $addEventListener node, eventName, (args...) ->
        callback node, args..., context
        $preventDefault args[0]

      if node.nodeName.toUpperCase() is 'A' and not node.href
        node.href = '#'

      node

    doubleclick: (node, callback, context) ->
      # The actual DOM event is called `dblclick`
      Batman.DOM.events.click node, callback, context, 'dblclick'

    change: (node, callback, context) ->
      eventNames = switch node.nodeName.toUpperCase()
        when 'TEXTAREA' then ['keyup', 'change']
        when 'INPUT'
          if node.type.toLowerCase() in Batman.DOM.textInputTypes
            oldCallback = callback
            callback = (e) ->
              return if e.type == 'keyup' && 13 <= e.keyCode <= 14
              oldCallback(arguments...)
            ['keyup', 'change']
          else
            ['change']
        else ['change']

      for eventName in eventNames
        $addEventListener node, eventName, (args...) ->
          callback node, args..., context

    isEnter: (ev) -> ev.keyCode is 13 || ev.which is 13 || ev.keyIdentifier is 'Enter' || ev.key is 'Enter'

    submit: (node, callback, context) ->
      if Batman.DOM.nodeIsEditable(node)
        $addEventListener node, 'keydown', (args...) ->
          if Batman.DOM.events.isEnter(args[0])
            Batman.DOM._keyCapturingNode = node
        $addEventListener node, 'keyup', (args...) ->
          if Batman.DOM.events.isEnter(args[0])
            if Batman.DOM._keyCapturingNode is node
              $preventDefault args[0]
              callback node, args..., context
            Batman.DOM._keyCapturingNode = null
      else
        $addEventListener node, 'submit', (args...) ->
          $preventDefault args[0]
          callback node, args..., context

      node

    other: (node, eventName, callback, context) -> $addEventListener node, eventName, (args...) -> callback node, args..., context
  }

  # List of input type="types" for which we can use keyup events to track
  textInputTypes: ['text', 'search', 'tel', 'url', 'email', 'password']

  # `yield` and `contentFor` are used to declare partial views and then pull them in elsewhere.
  # `replace` is used to replace yielded content.
  # This can be used for abstraction as well as repetition.
  yield: (name, node) ->
    Batman.DOM._yields[name] = node

    # render any content for this yield
    if yielders = Batman.DOM._yielders[name]
      fn(node) for fn in yielders

  contentFor: (name, node, _replaceContent, _yieldChildren) ->
    yieldingNode = Batman.DOM._yields[name]

    # Clone the node if it's a child in case the parent gets cleared during the yield
    if yieldingNode and $isChildOf(yieldingNode, node)
      node = $cloneNode node

    yieldFn = (yieldingNode) ->
      if _replaceContent || !Batman._data(yieldingNode, 'yielded')
        $setInnerHTML yieldingNode, '', true
        Batman.DOM.didRemoveNode(child) for child in yieldingNode.children

      if _yieldChildren
        while node.childNodes.length > 0
          child = node.childNodes[0]
          view = Batman.data(child, 'view')
          view?.fire 'beforeAppear', child

          $appendChild yieldingNode, node.childNodes[0], true

          view?.fire 'appear', child
      else
        view = Batman.data(node, 'view')
        view?.fire 'beforeAppear', child

        $appendChild yieldingNode, node, true

        view?.fire 'appear', child

      Batman._data node, 'yielded', true
      Batman._data yieldingNode, 'yielded', true
      delete Batman.DOM._yielders[name]

    if contents = Batman.DOM._yielders[name]
      contents.push yieldFn
    else
      Batman.DOM._yielders[name] = [yieldFn]

    if yieldingNode
      Batman.DOM.yield name, yieldingNode

  replace: (name, node, _yieldChildren) ->
    Batman.DOM.contentFor name, node, true, _yieldChildren

  partial: (container, path, context, renderer) ->
    renderer.prevent 'rendered'

    view = new Batman.View
      source: path
      context: context

    view.on 'ready', ->
      $setInnerHTML container, ''
      # Render the partial content into the data-partial node
      # Text nodes move when they are appended, so copy childNodes
      children = (node for node in view.get('node').childNodes)
      $appendChild(container, child) for child in children
      renderer.allowAndFire 'rendered'

  # Adds a binding or binding-like object to the `bindings` set in a node's
  # data, so that upon node removal we can unset the binding and any other objects
  # it retains.
  trackBinding: $trackBinding = (binding, node) ->
    if bindings = Batman._data node, 'bindings'
      bindings.add binding
    else
      Batman._data node, 'bindings', new Batman.SimpleSet(binding)

    Batman.DOM.fire('bindingAdded', binding)
    true

  # Removes listeners and bindings tied to `node`, allowing it to be cleared
  # or removed without leaking memory
  unbindNode: $unbindNode = (node) ->
    # break down all bindings
    if bindings = Batman._data node, 'bindings'
      bindings.forEach (binding) -> binding.die()

    # remove all event listeners
    if listeners = Batman._data node, 'listeners'
      for eventName, eventListeners of listeners
        eventListeners.forEach (listener) ->
          $removeEventListener node, eventName, listener

    # remove all bindings and other data associated with this node
    Batman.removeData node                   # external data (Batman.data)
    Batman.removeData node, undefined, true  # internal data (Batman._data)

  # Unbinds the tree rooted at `node`.
  # When set to `false`, `unbindRoot` skips the `node` before unbinding all of its children.
  unbindTree: $unbindTree = (node, unbindRoot = true) ->
    $unbindNode node if unbindRoot
    $unbindTree(child) for child in node.childNodes

  # Copy the event handlers from src node to dst node
  copyNodeEventListeners: $copyNodeEventListeners = (dst, src) ->
    if listeners = Batman._data src, 'listeners'
      for eventName, eventListeners of listeners
        eventListeners.forEach (listener) ->
          $addEventListener dst, eventName, listener

  # Copy all event handlers from the src tree to the dst tree.  Note that the
  # trees must have identical structures.
  copyTreeEventListeners: $copyTreeEventListeners = (dst, src) ->
    $copyNodeEventListeners dst, src
    for i in [0...src.childNodes.length]
      $copyTreeEventListeners dst.childNodes[i], src.childNodes[i]

  # Enhance the base cloneNode method to copy event handlers over to the new
  # instance
  cloneNode: $cloneNode = (node, deep=true) ->
    newNode = node.cloneNode(deep)
    if deep
      $copyTreeEventListeners newNode, node
    else
      $copyNodeEventListeners newNode, node
    newNode

  # Memory-safe setting of a node's innerHTML property
  setInnerHTML: $setInnerHTML = (node, html, args...) ->
    hide.apply(child, args) for child in node.childNodes when hide = Batman.data(child, 'hide')
    $unbindTree node, false
    node?.innerHTML = html

  setStyleProperty: $setStyleProperty = (node, property, value, importance) ->
    if node.style.setAttribute
      node.style.setAttribute(property, value, importance)
    else
      node.style.setProperty(property, value, importance)

  # Memory-safe removal of a node from the DOM
  removeNode: $removeNode = (node) ->
    node.parentNode?.removeChild node
    Batman.DOM.didRemoveNode(node)

  appendChild: $appendChild = (parent, child, args...) ->
    view = Batman.data(child, 'view')
    view?.fire 'beforeAppear', child

    Batman.data(child, 'show')?.apply(child, args)
    parent.appendChild(child)

    view?.fire 'appear', child

  insertBefore: $insertBefore = (parentNode, newNode, referenceNode = null) ->
    if !referenceNode or parentNode.childNodes.length <= 0
      $appendChild parentNode, newNode
    else
      parentNode.insertBefore newNode, referenceNode

  valueForNode: (node, value = '', escapeValue = true) ->
    isSetting = arguments.length > 1
    switch node.nodeName.toUpperCase()
      when 'INPUT', 'TEXTAREA'
        if isSetting then (node.value = value) else node.value
      when 'SELECT'
        if isSetting then node.value = value
      else
        if isSetting
          $setInnerHTML node, if escapeValue then $escapeHTML(value) else value
        else node.innerHTML

  nodeIsEditable: (node) ->
    node.nodeName.toUpperCase() in ['INPUT', 'TEXTAREA', 'SELECT']

  # `$addEventListener uses attachEvent when necessary
  addEventListener: $addEventListener = (node, eventName, callback) ->
    # store the listener in Batman.data
    unless listeners = Batman._data node, 'listeners'
      listeners = Batman._data node, 'listeners', {}
    unless listeners[eventName]
      listeners[eventName] = new Batman.Set
    listeners[eventName].add callback

    if $hasAddEventListener
      node.addEventListener eventName, callback, false
    else
      node.attachEvent "on#{eventName}", callback

  # `$removeEventListener` uses detachEvent when necessary
  removeEventListener: $removeEventListener = (node, eventName, callback) ->
    # remove the listener from Batman.data
    if listeners = Batman._data node, 'listeners'
      if eventListeners = listeners[eventName]
        eventListeners.remove callback

    if $hasAddEventListener
      node.removeEventListener eventName, callback, false
    else
      node.detachEvent 'on'+eventName, callback

  hasAddEventListener: $hasAddEventListener = !!window?.addEventListener

  didRemoveNode: (node) ->
    view = Batman.data node, 'view'
    view?.fire 'beforeDisappear', node

    $unbindTree node

    view?.fire 'disappear', node

  onParseExit: $onParseExit = (node, callback) ->
    set = Batman._data(node, 'onParseExit') || Batman._data(node, 'onParseExit', new Batman.SimpleSet)
    set.add callback if callback?
    set

  forgetParseExit: $forgetParseExit = (node, callback) -> Batman.removeData(node, 'onParseExit', true)
}

$mixin Batman.DOM, Batman.EventEmitter, Batman.Observable
Batman.DOM.event('bindingAdded')

# Bindings are shortlived objects which manage the observation of any keypaths a `data` attribute depends on.
# Bindings parse any keypaths which are filtered and use an accessor to apply the filters, and thus enjoy
# the automatic trigger and dependency system that Batman.Objects use. Every, and I really mean every method
# which uses filters has to be defined in terms of a new binding. This is so that the proper order of
# objects is traversed and any observers are properly attached.
class Batman.DOM.AbstractBinding extends Batman.Object
  # A beastly regular expression for pulling keypaths out of the JSON arguments to a filter.
  # It makes the following matches:
  #
  # + `foo` and `baz.qux` in `foo, "bar", baz.qux`
  # + `foo.bar.baz` in `true, false, "true", "false", foo.bar.baz`
  # + `true.bar` in `2, true.bar`
  # + `truesay` in truesay
  # + no matches in `"bar", 2, {"x":"y", "Z": foo.bar.baz}, "baz"`
  keypath_rx = ///
    (^|,)             # Match either the start of an arguments list or the start of a space inbetween commas.
    \s*               # Be insensitive to whitespace between the comma and the actual arguments.
    (?!               # Use a lookahead to ensure we aren't matching true or false:
      (?:true|false)  # Match either true or false ...
      \s*             # and make sure that there's nothing else that comes after the true or false ...
      (?:$|,)         # before the end of this argument in the list.
    )
    (
      [a-zA-Z][\w\.]* # Now that true and false can't be matched, match a dot delimited list of keys.
      [\?\!]?         # Allow ? and ! at the end of a keypath to support Ruby's methods
    )
    \s*               # Be insensitive to whitespace before the next comma or end of the filter arguments list.
    (?=$|,)             # Match either the next comma or the end of the filter arguments list.
    ///g

  # A less beastly pair of regular expressions for pulling out the [] syntax `get`s in a binding string, and
  # dotted names that follow them.
  get_dot_rx = /(?:\]\.)(.+?)(?=[\[\.]|\s*\||$)/
  get_rx = /(?!^\s*)\[(.*?)\]/g

  # The `filteredValue` which calculates the final result by reducing the initial value through all the filters.
  @accessor 'filteredValue'
    get: ->
      unfilteredValue = @get('unfilteredValue')
      self = @
      renderContext = @get('renderContext')
      if @filterFunctions.length > 0
        developer.currentFilterStack = renderContext

        result = @filterFunctions.reduce((value, fn, i) ->
          # Get any argument keypaths from the context stored at parse time.
          args = self.filterArguments[i].map (argument) ->
            if argument._keypath
              self.renderContext.get(argument._keypath)
            else
              argument

          # Apply the filter.
          args.unshift value
          args.push self
          fn.apply(renderContext, args)
        , unfilteredValue)
        developer.currentFilterStack = null
        result
      else
        unfilteredValue

    # We ignore any filters for setting, because they often aren't reversible.
    set: (_, newValue) -> @set('unfilteredValue', newValue)

  # The `unfilteredValue` is whats evaluated each time any dependents change.
  @accessor 'unfilteredValue'
    get: ->
      # If we're working with an `@key` and not an `@value`, find the context the key belongs to so we can
      # hold a reference to it for passing to the `dataChange` and `nodeChange` observers.
      if k = @get('key')
        Batman.RenderContext.deProxy(@get("keyContext.#{k}"))
      else
        @get('value')
    set: (_, value) ->
      if k = @get('key')
        keyContext = @get('keyContext')
        # Supress sets on the window
        if keyContext != Batman.container
          @set("keyContext.#{k}", value)
      else
        @set('value', value)


  # The `keyContext` accessor is
  @accessor 'keyContext', -> @renderContext.findKey(@key)[1]

  bindImmediately: true
  shouldSet: true
  isInputBinding: false
  escapeValue: true

  constructor: (@node, @keyPath, @renderContext, @renderer, @only = false) ->

    # Pull out the `@key` and filter from the `@keyPath`.
    @parseFilter()

    # Observe the node and the data.
    @bind() if @bindImmediately

  isTwoWay: -> @key? && @filterFunctions.length is 0

  bind: ->
    # Attach the observers.
    if @node? && @only in [false, 'nodeChange'] and Batman.DOM.nodeIsEditable(@node)
      Batman.DOM.events.change @node, @_fireNodeChange, @renderContext

      # Usually, we let the HTML value get updated upon binding by `observeAndFire`ing the dataChange
      # function below. When dataChange isn't attached, we update the JS land value such that the
      # sync between DOM and JS is maintained.
      if @only is 'nodeChange'
        @_fireNodeChange()

    # Observe the value of this binding's `filteredValue` and fire it immediately to update the node.
    if @only in [false, 'dataChange']
      @observeAndFire 'filteredValue', @_fireDataChange

    Batman.DOM.trackBinding(@, @node) if @node?

  _fireNodeChange: =>
    @shouldSet = false
    @nodeChange?(@node, @value || @get('keyContext'))
    @shouldSet = true

  _fireDataChange: (value) =>
    if @shouldSet
      @dataChange?(value, @node)

  die: ->
    @forget()
    @_batman.properties?.forEach (key, property) -> property.die()

  parseFilter: ->
    # Store the function which does the filtering and the arguments (all except the actual value to apply the
    # filter to) in these arrays.
    @filterFunctions = []
    @filterArguments = []

    # Rewrite [] style gets, replace quotes to be JSON friendly, and split the string by pipes to see if there are any filters.
    keyPath = @keyPath
    keyPath = keyPath.replace(get_dot_rx, "]['$1']") while get_dot_rx.test(keyPath)  # Stupid lack of lookbehind assertions...
    filters = keyPath.replace(get_rx, " | get $1 ").replace(/'/g, '"').split(/(?!")\s+\|\s+(?!")/)

    # The key will is always the first token before the pipe.
    try
      key = @parseSegment(orig = filters.shift())[0]
    catch e
      developer.warn e
      developer.error "Error! Couldn't parse keypath in \"#{orig}\". Parsing error above."
    if key and key._keypath
      @key = key._keypath
    else
      @value = key

    if filters.length
      while filterString = filters.shift()
        # For each filter, get the name and the arguments by splitting on the first space.
        split = filterString.indexOf(' ')
        if ~split
          filterName = filterString.substr(0, split)
          args = filterString.substr(split)
        else
          filterName = filterString

        # If the filter exists, grab it.
        if filter = Batman.Filters[filterName]
          @filterFunctions.push filter

          # Get the arguments for the filter by parsing the args as JSON, or
          # just pushing an placeholder array
          if args
            try
              @filterArguments.push @parseSegment(args)
            catch e
              developer.error "Bad filter arguments \"#{args}\"!"
          else
            @filterArguments.push []
        else
          developer.error "Unrecognized filter '#{filterName}' in key \"#{@keyPath}\"!"
      true

  # Turn a piece of a `data` keypath into a usable javascript object.
  #  + replacing keypaths using the above regular expression
  #  + wrapping the `,` delimited list in square brackets
  #  + and `JSON.parse`ing them as an array.
  parseSegment: (segment) ->
    JSON.parse( "[" + segment.replace(keypath_rx, "$1{\"_keypath\": \"$2\"}") + "]" )

class Batman.DOM.AbstractAttributeBinding extends Batman.DOM.AbstractBinding
  constructor: (node, @attributeName, args...) -> super(node, args...)

class Batman.DOM.AbstractCollectionBinding extends Batman.DOM.AbstractAttributeBinding

  bindCollection: (newCollection) ->
    unless newCollection == @collection
      @unbindCollection()
      @collection = newCollection
      if @collection
        if @collection.isObservable && @collection.toArray
          @collection.observe 'toArray', @handleArrayChanged
        else if @collection.isEventEmitter
          @collection.on 'itemsWereAdded', @handleItemsWereAdded
          @collection.on 'itemsWereRemoved', @handleItemsWereRemoved
        else
          return false
        return true
    return false

  unbindCollection: ->
    if @collection
      if @collection.isObservable && @collection.toArray
        @collection.forget('toArray', @handleArrayChanged)
      else if @collection.isEventEmitter
        @collection.event('itemsWereAdded').removeHandler(@handleItemsWereAdded)
        @collection.event('itemsWereRemoved').removeHandler(@handleItemsWereRemoved)

  handleItemsWereAdded: ->
  handleItemsWereRemoved: ->
  handleArrayChanged: ->

  die: ->
    @unbindCollection()
    super

class Batman.DOM.Binding extends Batman.DOM.AbstractBinding
  constructor: (node) ->
    @isInputBinding = node.nodeName.toLowerCase() in ['input', 'textarea']
    super

  nodeChange: (node, context) ->
    if @isTwoWay()
      @set 'filteredValue', @node.value

  dataChange: (value, node) ->
    Batman.DOM.valueForNode @node, value, @escapeValue

class Batman.DOM.AttributeBinding extends Batman.DOM.AbstractAttributeBinding
  dataChange: (value) -> @node.setAttribute(@attributeName, value)
  nodeChange: (node) ->
    if @isTwoWay()
      @set 'filteredValue', Batman.DOM.attrReaders._parseAttribute(node.getAttribute(@attributeName))

class Batman.DOM.NodeAttributeBinding extends Batman.DOM.AbstractAttributeBinding
  dataChange: (value = "") -> @node[@attributeName] = value
  nodeChange: (node) ->
    if @isTwoWay()
      @set 'filteredValue', Batman.DOM.attrReaders._parseAttribute(node[@attributeName])

class Batman.DOM.ShowHideBinding extends Batman.DOM.AbstractBinding
  constructor: (node, className, key, context, parentRenderer, @invert = false) ->
    display = node.style.display
    display = '' if not display or display is 'none'
    @originalDisplay = display
    super

  dataChange: (value) ->
    view = Batman.data @node, 'view'
    if !!value is not @invert
      view?.fire 'beforeAppear', @node

      Batman.data(@node, 'show')?.call(@node)
      @node.style.display = @originalDisplay

      view?.fire 'appear', @node
    else
      view?.fire 'beforeDisappear', @node

      if typeof (hide = Batman.data(@node, 'hide')) is 'function'
        hide.call @node
      else
        $setStyleProperty(@node, 'display', 'none', 'important')

      view?.fire 'disappear', @node

class Batman.DOM.CheckedBinding extends Batman.DOM.NodeAttributeBinding
  isInputBinding: true
  dataChange: (value) ->
    @node[@attributeName] = !!value
    # Update the parent's binding if necessary
    if @node.parentNode
      Batman._data(@node.parentNode, 'updateBinding')?()

  constructor: ->
    super
    # Attach this binding to the node under the attribute name so that parent
    # bindings can query this binding and modify its state. This is useful
    # for <options> within a select or radio buttons.
    Batman._data @node, @attributeName, @

class Batman.DOM.ClassBinding extends Batman.DOM.AbstractCollectionBinding
  dataChange: (value) ->
    if value?
      @unbindCollection()
      if typeof value is 'string'
        @node.className = value
      else
        @bindCollection(value)
        @updateFromCollection()

  updateFromCollection: ->
    if @collection
      array = if @collection.map
        @collection.map((x) -> x)
      else
        (k for own k,v of @collection)
      array = array.toArray() if array.toArray?
      @node.className = array.join ' '

  handleArrayChanged: => @updateFromCollection()
  handleItemsWereRemoved: => @updateFromCollection()
  handleItemsWereAdded: => @updateFromCollection()

class Batman.DOM.DeferredRenderingBinding extends Batman.DOM.AbstractBinding
  rendered: false
  constructor: ->
    super
    @node.removeAttribute "data-renderif"

  nodeChange: ->
  dataChange: (value) ->
    if value && !@rendered
      @render()

  render: ->
    new Batman.Renderer(@node, null, @renderContext)
    @rendered = true

class Batman.DOM.AddClassBinding extends Batman.DOM.AbstractAttributeBinding
  constructor: (node, className, keyPath, renderContext, renderer, only, @invert = false) ->
    names = className.split('|')
    @classes = for name in names
      {
        name: name
        pattern: new RegExp("(?:^|\\s)#{name}(?:$|\\s)", 'i')
      }
    super
    delete @attributeName

  dataChange: (value) ->
    currentName = @node.className
    for {name, pattern} in @classes
      includesClassName = pattern.test(currentName)
      if !!value is !@invert
        @node.className = "#{currentName} #{name}" if !includesClassName
      else
        @node.className = currentName.replace(pattern, '') if includesClassName
    true

class Batman.DOM.EventBinding extends Batman.DOM.AbstractAttributeBinding
  bindImmediately: false
  constructor: (node, eventName, key, context) ->
    super
    confirmText = @node.getAttribute('data-confirm')
    callback = =>
      return if confirmText and not confirm(confirmText)
      @get('filteredValue')?.apply @get('callbackContext'), arguments

    if attacher = Batman.DOM.events[@attributeName]
      attacher @node, callback, context
    else
      Batman.DOM.events.other @node, @attributeName, callback, context

  @accessor 'callbackContext', ->
    contextKeySegments = @key.split('.')
    contextKeySegments.pop()
    if contextKeySegments.length > 0
      @get('keyContext').get(contextKeySegments.join('.'))
    else
      @get('keyContext')

class Batman.DOM.RadioBinding extends Batman.DOM.AbstractBinding
  isInputBinding: true
  dataChange: (value) ->
    # don't overwrite `checked` attributes in the HTML unless a bound
    # value is defined in the context. if no bound value is found, bind
    # to the key if the node is checked.
    if (boundValue = @get('filteredValue'))?
      @node.checked = boundValue == @node.value
    else if @node.checked
      @set 'filteredValue', @node.value

  nodeChange: (node) ->
    if @isTwoWay()
      @set('filteredValue', Batman.DOM.attrReaders._parseAttribute(node.value))

class Batman.DOM.FileBinding extends Batman.DOM.AbstractBinding
  isInputBinding: true
  nodeChange: (node, subContext) ->
    return if !@isTwoWay()

    segments = @key.split('.')
    if segments.length > 1
      keyContext = subContext.get(segments.slice(0, -1).join('.'))
    else
      keyContext = subContext

    actualObject = Batman.RenderContext.deProxy(keyContext)

    if actualObject.hasStorage && actualObject.hasStorage()
      for adapter in actualObject._batman.get('storage') when adapter instanceof Batman.RestStorage
        adapter.defaultRequestOptions.formData = true

    if node.hasAttribute('multiple')
      @set 'filteredValue', Array::slice.call(node.files)
    else
      @set 'filteredValue', node.files[0]

class Batman.DOM.MixinBinding extends Batman.DOM.AbstractBinding
  dataChange: (value) -> $mixin @node, value if value?

class Batman.DOM.SelectBinding extends Batman.DOM.AbstractBinding
  isInputBinding: true
  bindImmediately: false
  firstBind: true

  constructor: ->
    super
    # wait for the select to render before binding to it
    @renderer.on 'rendered', =>
      if @node?
        Batman._data @node, 'updateBinding', @updateSelectBinding
        @bind()

  dataChange: (newValue) =>
    # For multi-select boxes, the `value` property only holds the first
    # selection, so go through the child options and update as necessary.
    if newValue instanceof Array
      # Use a hash to map values to their nodes to avoid O(n^2).
      valueToChild = {}
      for child in @node.children
        # Clear all options.
        child.selected = false
        # Avoid collisions among options with same values.
        matches = valueToChild[child.value]
        if matches then matches.push child else matches = [child]
        valueToChild[child.value] = matches
      # Select options corresponding to the new values
      for value in newValue
        for match in valueToChild[value]
          match.selected = yes
    # For a regular select box, update the value.
    else
      if typeof newValue is 'undefined' && @firstBind
        @firstBind = false
        @set('unfilteredValue', @node.value)
      else
        Batman.DOM.valueForNode(@node, newValue, @escapeValue)

    # Finally, update the options' `selected` bindings
    @updateOptionBindings()

  nodeChange: =>
    if @isTwoWay()
      @updateSelectBinding()
      @updateOptionBindings()

  updateSelectBinding: =>
    # Gather the selected options and update the binding
    selections = if @node.multiple then (c.value for c in @node.children when c.selected) else @node.value
    selections = selections[0] if typeof selections is Array && selections.length == 1
    @set 'unfilteredValue', selections
    true

  updateOptionBindings: =>
    # Go through the option nodes and update their bindings using the
    # context and key attached to the node via Batman.data
    for child in @node.children
      if selectedBinding = Batman._data(child, 'selected')
        selectedBinding.nodeChange(selectedBinding.node)
    true

class Batman.DOM.StyleBinding extends Batman.DOM.AbstractCollectionBinding

  class @SingleStyleBinding extends Batman.DOM.AbstractAttributeBinding
    constructor: (args..., @parent) ->
      super(args...)
    dataChange: (value) -> @parent.setStyle(@attributeName, value)

  constructor: ->
    @oldStyles = {}
    super

  dataChange: (value) ->
    unless value
      @reapplyOldStyles()
      return

    @unbindCollection()

    if typeof value is 'string'
      @reapplyOldStyles()
      for style in value.split(';')
        # handle a case when css value contains colons itself (absolute URI)
        # split and rejoin because IE7/8 don't splice values of capturing regexes into split's return array
        [cssName, colonSplitCSSValues...] = style.split(":")
        @setStyle cssName, colonSplitCSSValues.join(":")
      return

    if value instanceof Batman.Hash
      if @bindCollection(value)
        value.forEach (key, value) => @setStyle key, value
    else if value instanceof Object
      @reapplyOldStyles()
      for own key, keyValue of value
        # Check whether the value is an existing keypath, and if so bind this attribute to it
        [keypathValue, keypathContext] = @renderContext.findKey(keyValue)
        if keypathValue
          @bindSingleAttribute key, keyValue
          @setStyle key, keypathValue
        else
          @setStyle key, keyValue

  handleItemsWereAdded: (newKey) => @setStyle newKey, @collection.get(newKey); return
  handleItemsWereRemoved: (oldKey) => @setStyle oldKey, ''; return

  bindSingleAttribute: (attr, keyPath) -> new @constructor.SingleStyleBinding(@node, attr, keyPath, @renderContext, @renderer, @only, @)

  setStyle: (key, value) =>
    return unless key
    key = helpers.camelize(key.trim(), true)
    @oldStyles[key] = @node.style[key]
    @node.style[key] = if value then value.trim() else ""

  reapplyOldStyles: ->
    @setStyle(cssName, cssValue) for own cssName, cssValue of @oldStyles

class Batman.DOM.RouteBinding extends Batman.DOM.AbstractBinding
  onATag: false

  @accessor 'dispatcher', ->
    @renderContext.get('dispatcher') || Batman.App.get('current.dispatcher')

  bind: ->
    if @node.nodeName.toUpperCase() is 'A'
      @onATag = true
    super
    Batman.DOM.events.click @node, =>
      params = @pathFromValue(@get('filteredValue'))
      Batman.redirect params if params?

  dataChange: (value) ->
    if value?
      path = @pathFromValue(value)

    if @onATag
      if path?
        path = Batman.navigator.linkTo path
      else
        path = "#"
      @node.href = path

  pathFromValue: (value) ->
    if value.isNamedRouteQuery
      value.get('path')
    else
      @get('dispatcher')?.pathFromParams(value)

class Batman.DOM.ViewBinding extends Batman.DOM.AbstractBinding
  constructor: ->
    super
    @renderer.prevent 'rendered'
    @node.removeAttribute 'data-view'

  dataChange: (viewClassOrInstance) ->
    return unless viewClassOrInstance?
    if viewClassOrInstance.isView
      @view = viewClassOrInstance
      @view.set 'node', @node
      @view.set 'context', @renderContext
    else
      @view = new viewClassOrInstance
        node: @node
        context: @renderContext
        parentView: @renderContext.findKey('isView')?[1]

    Batman.data @node, 'view', @view

    @view.on 'ready', =>
      @view.awakeFromHTML? @node
      @view.fire 'beforeAppear', @node
      @renderer.allowAndFire 'rendered'
      @view.fire 'appear', @node

    @die()

class Batman.DOM.FormBinding extends Batman.DOM.AbstractAttributeBinding
  @current: null
  errorClass: 'error'
  defaultErrorsListSelector: 'div.errors'

  @accessor 'errorsListSelector', ->
    @get('node').getAttribute('data-errors-list') || @defaultErrorsListSelector

  constructor: (node, contextName, keyPath, renderContext, renderer, only) ->
    super
    @contextName = contextName
    delete @attributeName

    Batman.DOM.events.submit @get('node'), (node, e) -> $preventDefault e
    Batman.DOM.on 'bindingAdded', @bindingWasAdded

    @setupErrorsList()

  bindingWasAdded: (binding) =>
    if binding.isInputBinding && $isChildOf(@get('node'), binding.get('node'))
      if ~(index = binding.get('key').indexOf(@contextName)) # If the binding is to a key on the thing passed to formfor
        node = binding.get('node')
        field = binding.get('key').slice(index + @contextName.length + 1) # Slice off up until the context and the following dot
        new Batman.DOM.AddClassBinding(node, @errorClass, @get('keyPath') + " | get 'errors.#{field}.length'", @renderContext, @renderer)

  setupErrorsList: ->
    if @errorsListNode = @get('node').querySelector?(@get('errorsListSelector'))
      $setInnerHTML @errorsListNode, @errorsListHTML()

      unless @errorsListNode.getAttribute 'data-showif'
        @errorsListNode.setAttribute 'data-showif', "#{@contextName}.errors.length"

  errorsListHTML: ->
    """
    <ul>
      <li data-foreach-error="#{@contextName}.errors" data-bind="error.attribute | append ' ' | append error.message"></li>
    </ul>
    """

  die: ->
    Batman.DOM.forget 'bindingAdded', @bindingWasAdded
    super

class Batman.DOM.IteratorBinding extends Batman.DOM.AbstractCollectionBinding
  deferEvery: 50
  currentActionNumber: 0
  queuedActionNumber: 0
  bindImmediately: false

  constructor: (sourceNode, @iteratorName, @key, @context, @parentRenderer) ->
    @nodeMap = new Batman.SimpleHash
    @actionMap = new Batman.SimpleHash
    @rendererMap = new Batman.SimpleHash
    @actions = []

    @prototypeNode = sourceNode.cloneNode(true)
    @prototypeNode.removeAttribute "data-foreach-#{@iteratorName}"
    @pNode = sourceNode.parentNode
    previousSiblingNode = sourceNode.nextSibling
    @siblingNode = document.createComment "end #{@iteratorName}"
    @siblingNode[Batman.expando] = sourceNode[Batman.expando]
    delete sourceNode[Batman.expando] if Batman.canDeleteExpando
    $insertBefore sourceNode.parentNode, @siblingNode, previousSiblingNode
    # Remove the original node once the parent has moved past it.
    @parentRenderer.on 'parsed', =>
      # Move any Batman._data from the sourceNode to the sibling; we need to
      # retain the bindings, and we want to dispose of the node.
      $removeNode sourceNode
      # Attach observers.
      @bind()

    # Don't let the parent emit its rendered event until all the children have.
    # This `prevent`'s matching allow is run once the queue is empty in `processActionQueue`.
    @parentRenderer.prevent 'rendered'

    # Tie this binding to a node using the default behaviour in the AbstractBinding
    super(@siblingNode, @iteratorName, @key, @context, @parentRenderer)

    @fragment = document.createDocumentFragment()

  parentNode: -> @siblingNode.parentNode

  die: ->
    super
    @dead = true

  unbindCollection: ->
    if @collection
      @nodeMap.forEach (item) => @cancelExistingItem(item)
      super

  dataChange: (newCollection) ->
    if @collection != newCollection
      @removeAll()

    @bindCollection(newCollection) # Unbinds the old collection as well.
    if @collection
      if @collection.toArray
        @handleArrayChanged()
      else if @collection.forEach
        @collection.forEach (item) => @addOrInsertItem(item)
      else
        @addOrInsertItem(key) for own key, value of @collection

    else
      developer.warn "Warning! data-foreach-#{@iteratorName} called with an undefined binding. Key was: #{@key}."
    @processActionQueue()

  handleItemsWereAdded: (items...) => @addOrInsertItem(item, {fragment: false}) for item in items; return
  handleItemsWereRemoved: (items...) => @removeItem(item) for item in items; return

  handleArrayChanged: =>
    newItemsInOrder = @collection.toArray()
    nodesToRemove = (new Batman.SimpleHash).merge(@nodeMap)
    for item in newItemsInOrder
      @addOrInsertItem(item, {fragment: false})
      nodesToRemove.unset(item)

    nodesToRemove.forEach (item, node) => @removeItem(item)

  addOrInsertItem: (item, options = {}) ->
    existingNode = @nodeMap.get(item)
    if existingNode
      @insertItem(item, existingNode)
    else
      @addItem(item, options)

  addItem: (item, options = {fragment: true}) ->
    @parentRenderer.prevent 'rendered'

    # Remove any renderers in progress or actions lined up for an item, since we now know
    # this item belongs at the end of the queue.
    @cancelExistingItemActions(item) if @actionMap.get(item)?

    self = @
    options.actionNumber = @queuedActionNumber++

    # Render out the child in the custom context, and insert it once the render has completed the parse.
    childRenderer = new Batman.Renderer @_nodeForItem(item), (->
      self.rendererMap.unset(item)
      self.insertItem(item, @node, options)
    ), @renderContext.descend(item, @iteratorName)

    @rendererMap.set(item, childRenderer)

    finish = =>
      return if @dead
      @parentRenderer.allowAndFire 'rendered'

    childRenderer.on 'rendered', finish
    childRenderer.on 'stopped', =>
      return if @dead
      @actions[options.actionNumber] = false
      finish()
      @processActionQueue()
    item

  removeItem: (item) ->
    return if @dead || !item?
    oldNode = @nodeMap.unset(item)
    @cancelExistingItem(item)
    if oldNode
      if hideFunction = Batman.data oldNode, 'hide'
        hideFunction.call(oldNode)
      else
        $removeNode(oldNode)

  removeAll: -> @nodeMap.forEach (item) => @removeItem(item)

  insertItem: (item, node, options = {}) ->
    return if @dead
    if !options.actionNumber?
      options.actionNumber = @queuedActionNumber++

    existingActionNumber = @actionMap.get(item)
    if existingActionNumber > options.actionNumber
      # Another action for this item is scheduled for the future, do it then instead of now. Actions
      # added later enforce order, so we make this one a noop and let the later one have its proper effects.
      @actions[options.actionNumber] = ->
    else
      # Another action has been scheduled for this item. It hasn't been done yet because
      # its in the actionmap, but this insert is scheduled to happen after it. Skip it since its now defunct.
      if existingActionNumber
        @cancelExistingItemActions(item)

      # Update the action number map to now reflect this new action which will go on the end of the queue.
      @actionMap.set item, options.actionNumber
      @actions[options.actionNumber] = ->
        show = Batman.data node, 'show'
        if typeof show is 'function'
          show.call node, before: @siblingNode
        else
          if options.fragment
            @fragment.appendChild node
          else
            $insertBefore @parentNode(), node, @siblingNode

        if addItem = node.getAttribute 'data-additem'
          [_, context] = @renderer.context.findKey addItem
          context?[addItem]?(item, node)

      @actions[options.actionNumber].item = item
    @processActionQueue()

  cancelExistingItem: (item) ->
    @cancelExistingItemActions(item)
    @cancelExistingItemRender(item)

  cancelExistingItemActions: (item) ->
    oldActionNumber = @actionMap.get(item)
    # Only remove actions which haven't been completed yet.
    if oldActionNumber? && oldActionNumber >= @currentActionNumber
      @actions[oldActionNumber] = false

    @actionMap.unset item

  cancelExistingItemRender: (item) ->
    oldRenderer = @rendererMap.get(item)
    if oldRenderer
      oldRenderer.stop()
      $removeNode(oldRenderer.node)

    @rendererMap.unset item

  processActionQueue: ->
    return if @dead
    unless @actionQueueTimeout
      # Prevent the parent which will then be allowed when the timeout actually runs
      @actionQueueTimeout = $setImmediate =>
        return if @dead
        delete @actionQueueTimeout
        startTime = new Date

        while (f = @actions[@currentActionNumber])?
          delete @actions[@currentActionNumber]
          @actionMap.unset f.item
          f.call(@) if f
          @currentActionNumber++

          if @deferEvery && (new Date - startTime) > @deferEvery
            return @processActionQueue()

        if @fragment && @rendererMap.length is 0 && @fragment.hasChildNodes()
          $insertBefore @parentNode(), @fragment, @siblingNode
          @fragment = document.createDocumentFragment()

        if @currentActionNumber == @queuedActionNumber
          @parentRenderer.allowAndFire 'rendered'

  _nodeForItem: (item) ->
    newNode = @prototypeNode.cloneNode(true)
    @nodeMap.set(item, newNode)
    newNode

# Filters
# -------
#
# `Batman.Filters` contains the simple, determininistic tranforms used in view bindings to
# make life a little easier.
buntUndefined = (f) ->
  (value) ->
    if typeof value is 'undefined'
      undefined
    else
      f.apply(@, arguments)

filters = Batman.Filters =
  raw: buntUndefined (value, binding) ->
    binding.escapeValue = false
    value

  get: buntUndefined (value, key) ->
    if value.get?
      value.get(key)
    else
      value[key]

  equals: buntUndefined (lhs, rhs, binding) ->
    lhs is rhs

  not: (value, binding) ->
    ! !!value

  matches: buntUndefined (value, searchFor) ->
    value.indexOf(searchFor) isnt -1

  truncate: buntUndefined (value, length, end = "...", binding) ->
    if !binding
      binding = end
      end = "..."
    if value.length > length
      value = value.substr(0, length-end.length) + end
    value

  default: (value, string, binding) ->
    value || string

  prepend: (value, string, binding) ->
    string + value

  append: (value, string, binding) ->
    value + string

  replace: buntUndefined (value, searchFor, replaceWith, flags, binding) ->
    if !binding
      binding = flags
      flags = undefined
    # Work around FF issue, "foo".replace("foo","bar",undefined) throws an error
    if flags is undefined
      value.replace searchFor, replaceWith
    else
      value.replace searchFor, replaceWith, flags

  downcase: buntUndefined (value) ->
    value.toLowerCase()

  upcase: buntUndefined (value) ->
    value.toUpperCase()

  pluralize: buntUndefined (string, count) -> helpers.pluralize(count, string)

  join: buntUndefined (value, withWhat = '', binding) ->
    if !binding
      binding = withWhat
      withWhat = ''
    value.join(withWhat)

  sort: buntUndefined (value) ->
    value.sort()

  map: buntUndefined (value, key) ->
    value.map((x) -> $get(x, key))

  has: (set, item) ->
    return false unless set?
    Batman.contains(set, item)

  first: buntUndefined (value) ->
    value[0]

  meta: buntUndefined (value, keypath) ->
    developer.assert value.meta, "Error, value doesn't have a meta to filter on!"
    value.meta.get(keypath)

  interpolate: (string, interpolationKeypaths, binding) ->
    if not binding
      binding = interpolationKeypaths
      interpolationKeypaths = undefined
    return if not string
    values = {}
    for k, v of interpolationKeypaths
      values[k] = @findKey(v)[0]
      if !values[k]?
        Batman.developer.warn "Warning! Undefined interpolation key #{k} for interpolation", string
        values[k] = ''

    Batman.helpers.interpolate(string, values)

  # allows you to curry arguments to a function via a filter
  withArguments: (block, curryArgs..., binding) ->
    return if not block
    return (regularArgs...) -> block.call @, curryArgs..., regularArgs...

  routeToAction: buntUndefined (model, action) ->
    params = Batman.Dispatcher.paramsFromArgument(model)
    params.action = action
    params

  escape: buntUndefined($escapeHTML)

for k in ['capitalize', 'singularize', 'underscore', 'camelize']
  filters[k] = buntUndefined helpers[k]

developer.addFilters()

# Data
# ----
$mixin Batman,
  cache: {}
  uuid: 0
  expando: "batman" + Math.random().toString().replace(/\D/g, '')
  # Test to see if it's possible to delete an expando from an element
  # Fails in Internet Explorer
  canDeleteExpando: try
      div = document.createElement 'div'
      delete div.test
    catch e
      Batman.canDeleteExpando = false
  noData: # these throw exceptions if you attempt to add expandos to them
    "embed": true,
    # Ban all objects except for Flash (which handle expandos)
    "object": "clsid:D27CDB6E-AE6D-11cf-96B8-444553540000",
    "applet": true

  hasData: (elem) ->
    elem = (if elem.nodeType then Batman.cache[elem[Batman.expando]] else elem[Batman.expando])
    !!elem and !isEmptyDataObject(elem)

  data: (elem, name, data, pvt) -> # pvt is for internal use only
    return  unless Batman.acceptData(elem)
    internalKey = Batman.expando
    getByName = typeof name == "string"
    cache = Batman.cache
    # Only defining an ID for JS objects if its cache already exists allows
    # the code to shortcut on the same path as a DOM node with no cache
    id = elem[Batman.expando]

    # Avoid doing any more work than we need to when trying to get data on an
    # object that has no data at all
    if (not id or (pvt and id and (cache[id] and not cache[id][internalKey]))) and getByName and data == undefined
      return

    unless id
      # Also check that it's not a text node; IE can't set expandos on them
      if elem.nodeType isnt 3
        elem[Batman.expando] = id = ++Batman.uuid
      else
        id = Batman.expando

    cache[id] = {} unless cache[id]

    # An object can be passed to Batman._data instead of a key/value pair; this gets
    # shallow copied over onto the existing cache
    if typeof name == "object" or typeof name == "function"
      if pvt
        cache[id][internalKey] = $mixin(cache[id][internalKey], name)
      else
        cache[id] = $mixin(cache[id], name)

    thisCache = cache[id]

    # Internal Batman data is stored in a separate object inside the object's data
    # cache in order to avoid key collisions between internal data and user-defined
    # data
    if pvt
      thisCache[internalKey] ||= {}
      thisCache = thisCache[internalKey]

    if data != undefined
      thisCache[name] = data

    # Check for both converted-to-camel and non-converted data property names
    # If a data property was specified
    if getByName
      # First try to find as-is property data
      ret = thisCache[name]
    else
      ret = thisCache

    return ret

  removeData: (elem, name, pvt) -> # pvt is for internal use only
    return unless Batman.acceptData(elem)
    internalKey = Batman.expando
    isNode = elem.nodeType
    # non DOM-nodes have their data attached directly
    cache = Batman.cache
    id = elem[Batman.expando]

    # If there is already no cache entry for this object, there is no
    # purpose in continuing
    return unless cache[id]

    if name
      thisCache = if pvt then cache[id][internalKey] else cache[id]
      if thisCache
        # Support interoperable removal of hyphenated or camelcased keys
        delete thisCache[name]
        # If there is no data left in the cache, we want to continue
        # and let the cache object itself get destroyed
        return unless isEmptyDataObject(thisCache)

    if pvt
      delete cache[id][internalKey]
      # Don't destroy the parent cache unless the internal data object
      # had been the only thing left in it
      return unless isEmptyDataObject(cache[id])

    internalCache = cache[id][internalKey]

    # Browsers that fail expando deletion also refuse to delete expandos on
    # the window, but it will allow it on all other JS objects; other browsers
    # don't care
    # Ensure that `cache` is not a window object
    if Batman.canDeleteExpando or !cache.setInterval
      delete cache[id]
    else
      cache[id] = null

    # We destroyed the entire user cache at once because it's faster than
    # iterating through each key, but we need to continue to persist internal
    # data if it existed
    if internalCache
      cache[id] = {}
      cache[id][internalKey] = internalCache
    # Otherwise, we need to eliminate the expando on the node to avoid
    # false lookups in the cache for entries that no longer exist
    else
      if Batman.canDeleteExpando
        delete elem[Batman.expando]
      else if elem.removeAttribute
        elem.removeAttribute Batman.expando
      else
        elem[Batman.expando] = null

  # For internal use only
  _data: (elem, name, data) ->
    Batman.data elem, name, data, true

  # A method for determining if a DOM node can handle the data expando
  acceptData: (elem) ->
    if elem.nodeName
      match = Batman.noData[elem.nodeName.toLowerCase()]
      if match
        return !(match == true or elem.getAttribute("classid") != match)
    return true

isEmptyDataObject = (obj) ->
  for name of obj
    return false
  return true

# Mixins
# ------
mixins = Batman.mixins = new Batman.Object()

# Encoders
# ------
Batman.Encoders = {}

class Batman.Paginator extends Batman.Object
  class @Range
    constructor: (@offset, @limit) ->
      @reach = offset + limit
    coversOffsetAndLimit: (offset, limit) ->
      offset >= @offset and (offset + limit) <= @reach

  class @Cache extends @Range
    constructor: (@offset, @limit, @items) ->
      super
      @length = items.length
    itemsForOffsetAndLimit: (offset, limit) ->
      begin = offset-@offset
      end = begin + limit
      if begin < 0
        padding = new Array(-begin)
        begin = 0
      slice = @items.slice(begin, end)
      if padding
        padding.concat(slice)
      else
        slice

  offset: 0
  limit: 10
  totalCount: 0

  markAsLoadingOffsetAndLimit: (offset, limit) -> @loadingRange = new Batman.Paginator.Range(offset, limit)
  markAsFinishedLoading: -> delete @loadingRange

  offsetFromPageAndLimit: (page, limit) -> Math.round((+page - 1) * limit)
  pageFromOffsetAndLimit: (offset, limit) -> offset / limit + 1

  _load: (offset, limit) ->
    return if @loadingRange?.coversOffsetAndLimit(offset, limit)
    @markAsLoadingOffsetAndLimit(offset, limit)
    @loadItemsForOffsetAndLimit(offset, limit)

  toArray: ->
    cache = @get('cache')
    offset = @get('offset')
    limit = @get('limit')
    @_load(offset, limit) unless cache?.coversOffsetAndLimit(offset, limit)
    cache?.itemsForOffsetAndLimit(offset, limit) or []
  page: ->
    @pageFromOffsetAndLimit(@get('offset'), @get('limit'))
  pageCount: ->
    Math.ceil(@get('totalCount') / @get('limit'))

  previousPage: -> @set('page', @get('page')-1)
  nextPage: -> @set('page', @get('page')+1)

  loadItemsForOffsetAndLimit: (offset, limit) -> # override on subclasses or instances
  updateCache: (offset, limit, items) ->
    cache = new Batman.Paginator.Cache(offset, limit, items)
    return if @loadingRange? and not cache.coversOffsetAndLimit(@loadingRange.offset, @loadingRange.limit)
    @markAsFinishedLoading()
    @set('cache', cache)
  @accessor 'toArray', @::toArray
  @accessor 'offset', 'limit', 'totalCount'
    get: Batman.Property.defaultAccessor.get
    set: (key, value) -> Batman.Property.defaultAccessor.set.call(this, key, +value)
  @accessor 'page',
    get: @::page
    set: (_,value) ->
      value = +value
      @set('offset', @offsetFromPageAndLimit(value, @get('limit')))
      value
  @accessor 'pageCount', @::pageCount

class Batman.ModelPaginator extends Batman.Paginator
  cachePadding: 0
  paddedOffset: (offset) ->
    offset -= @cachePadding
    if offset < 0 then 0 else offset
  paddedLimit: (limit) ->
    limit + @cachePadding * 2

  loadItemsForOffsetAndLimit: (offset, limit) ->
    params = @paramsForOffsetAndLimit(offset, limit)
    params[k] = v for k,v of @params
    @model.load params, (err, records) =>
      if err?
        @markAsFinishedLoading()
        @fire('error', err)
      else
        @updateCache(@offsetFromParams(params), @limitFromParams(params), records)

  # override these to fetch records however you like:
  paramsForOffsetAndLimit: (offset, limit) ->
    offset: @paddedOffset(offset), limit: @paddedLimit(limit)
  offsetFromParams: (params) -> params.offset
  limitFromParams: (params) -> params.limit

# Support AMD loaders
if typeof define is 'function'
  define 'batman', [], -> Batman

# Optionally export global sugar. Not sure what to do with this.
Batman.exportHelpers = (onto) ->
  for k in ['mixin', 'unmixin', 'route', 'redirect', 'typeOf', 'redirect', 'setImmediate']
    onto["$#{k}"] = Batman[k]
  onto

Batman.exportGlobals = () ->
  Batman.exportHelpers(Batman.container)

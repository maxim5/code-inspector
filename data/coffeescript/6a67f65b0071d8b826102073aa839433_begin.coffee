# This library is a flow control library for node.js and CoffeeScript.
# For more information, please see https://github.com/arumons/begin .

# Manage a scope
class Scope
	constructor: (unit) ->
		jumped = false
		_err_msg = "you can call scope transition function only once in a scope"

		_pre_iterator_function = (args) ->
			_arrays = (arg for arg in args when Array.isArray arg)
			fn = args[_arrays.length]
			thisp = args[_arrays.length + 1] ? global
			_arrays: _arrays
			defed: if fn.is_defed then fn else macro(fn).end()
			thisp: thisp
			units: new Units((-> @next()), thisp, true)
			 
		# Jump to the next "catch" scope
		@throw = (args...) ->
			unit.throw.apply unit, args
			unit

		# Jump to outer scope
		@out = (args...) ->
			unit.out.apply unit, args
			unit

		# jump to "_" scope
		@next = (args...) ->
			unit.next.apply unit, args
			unit

		@_ = (block) ->
			self = @
			block.call self
			unit

		# Returning array which consist of value returned true by the block
		# thisp is injected to @self in the block
		@filter = (args...) ->
			{_arrays, defed, thisp, units} = _pre_iterator_function args
			result = []
			arrays.apply(null, _arrays).map((args...) ->
				units._ -> @_ -> defed.apply thisp, args
				units._ (v) -> result.push args.slice(0, -2) if v; @next())
			units._ ->
				@next.apply @, Arrays.zip result
			units.end()
			unit

		# Returning array which consist of value returned by the block
		# thisp is injected to @self in the block
		@map = (args...) ->
			{_arrays, defed, thisp, units} = _pre_iterator_function args
			result = []
			arrays.apply(null, _arrays).map (args...) ->
				units._ -> @_ -> defed.apply thisp, args
				units._ (args...) -> result.push args; @next()
			units._ ->
				@next.apply @, Arrays.zip result
			units.end()
			unit

		# Return true if function return true to all value in the array
		@every = (args...) ->
			{_arrays, defed, thisp, units} = _pre_iterator_function args
			arrays.apply(null, _arrays).map (args...) ->
				units._ -> @_ -> defed.apply thisp, args
				units._ (v) -> unless v then @out false else @next()
			units._ -> @out true
			units.end()
			unit

		# Return true if function return true to any value in the array
		@some = (args...) ->
			{_arrays, defed, thisp, units} = _pre_iterator_function args
			arrays.apply(null, _arrays).map (args...) ->
				units._ -> @_ -> defed.apply thisp, args
				units._ (v) -> if v then @out true else @next()
			units._ -> @out false
			units.end()
			unit

		# Apply a function an accumulator and each value of the array (left-to-right)
		@reduce = (array, block, init, reverse) ->
			global = do -> this
			defed = if block.is_defed then block else macro(block).end()
			
			i = 0
			units = new Units ->
				throw new TypeError() if array.length <= 0
				@next()
			array = array.reverse() if reverse
			if init?
				units._ @next init, array[0], i++, array
				_array = array.slice 1
			else
				i++
				units._ -> @next array[0], array[1], i++, array
				_array = array.slice 2

			_array.forEach (item) ->
				units._ (v1, v2, i, array) -> @_ -> defed.call global, v1, v2, i, array
				units._ (v) -> @next v, item, i++, array
			units._ (v1, v2, i, array) -> @_ -> defed.call global, v1, v2, i, array
			units._ (result) -> @next result
			units.end()
			unit
		
		# Apply a function an accumulator and each value of the array (right-to-left)
		@reduceRight = (array, block, init) ->
			@reduce array, block, init, true

# Manage transitioning to the next scope
class Unit

	# block - a function.
	# receiver - injection to "@self".
	# use_outer_scope  - if it is true, valiable of outer scope is accesible.
	constructor: (@block, receiver = undefined, @use_outer_scope = true) ->
		@scope = Object.create(new Scope(@))
		@scope.self = receiver

	# Save next scope of outer scope.
	# If scope come end and outer scope exist, transition to outer scope.
	end: ->
		continuation = Units.CurrentContinuation
		@next = (args...) ->
			if @use_outer_scope and continuation?
				@_shift @scope, continuation
			return continuation.next.apply continuation, args if continuation?
			
			Units.CurrentContinuation = undefined

		@throw = (args...) ->
			if @use_outer_scope and continuation?
				@_shift @scope, continuation
			return continuation.throw.apply continuation, args if continuation?

			Units.CurrentContinuation = undefined
			process.nextTick -> throw args[0]

		@out = (args...) ->
			if @use_outer_scope and continuation?
				@_shift @scope, continuation
			return continuation.next.apply continuation, args if continuation?
			
			Units.CurrentContinuation = undefined

	# Connect Unit to Unit.
	# Trasition to next scope when @next is called.
	_: (unit) ->
		@next = (args...) ->
			@_next_scope unit, args

		@throw = (args...) ->
			@_skip_scope unit, 'throw', args

		@out = (args...) ->
			@_skip_scope unit, 'out', args

		@next_unit = unit
		unit.previous_unit = @
		return unit

	# Connect Unit to Unit
	# Trasition to next scope when @throw is called.
	catch: (unit) ->
		@next = (args...) ->
			@_skip_scope unit, 'next', args
			
		@throw = (args...) ->
			@_next_scope unit, args

		@out = (args...) ->
			@_skip_scope unit, 'out', args

		@next_unit = unit
		unit.previous_unit = @
		return unit

	# Execute function passed begin or _ or catch.
	invoke: () ->
		return @previous_unit.invoke() if @previous_unit?

		if @use_outer_scope and Units.CurrentContinuation?
			@_shift Units.CurrentContinuation, @scope

		@_next_scope @, []

	_next_scope: (next_unit, args) ->
		if @ isnt next_unit
			next_unit.use_outer_scope = @use_outer_scope
			@_shift @scope, next_unit.scope
		Units.CurrentContinuation = next_unit.scope
		try
			if next_unit isnt next_unit.block.apply next_unit.scope, args
				throw new Error "you must call scope trasition function at end of scope."
			Units.CurrentContinuation = undefined
		catch error
			@_skip_scope next_unit, 'throw', [error]

	_skip_scope: (next_unit, event, args) ->
		next_unit.use_outer_scope = @use_outer_scope
		@_shift @scope, next_unit.scope
		Units.CurrentContinuation = next_unit.scope
		next_unit[event].apply next_unit, args

	_shift: (from, to) ->
		for own p of from
			to[p] = from[p] unless p is "self" and to[p]?

# Utility functions for Array
arrays = (arrays...) ->
	new Arrays arrays

class Arrays
	# if arrays is [[1, 2], [3, 4]],
	# @ziped is [[1, 3], [2, 4]]
	constructor: (@original) ->
		@ziped = Arrays.zip original

	@zip: (arrays) ->
		max = 0
		max = array.length for array in arrays when max < array.length

		(arrays.map((v) -> v[""+i]) for i in [0...max])

	map: (block, thisp) ->
		result = []
		thisp ?= global
		@ziped.map (args, i, _array) ->
			args.push i
			args.push _array
			result.push block.apply(thisp, args)
		result

# Manage Units
class Units
	constructor: (block, context = undefined, use_outer_scope = true) ->
		@head = new Unit block, context, use_outer_scope
		@tail = @head

	# "_" and "catch" can receive Unit, Units, Array and function
	for p in ['_', 'catch']
		@::[p] = do (p) ->
			(block) ->
				if block instanceof Unit
					@tail = @tail[p] block
				else if block instanceof Units
					@tail[p] block.head
					@tail = block.tail
				else
					@tail = @tail[p](new Unit block)
				return @
	
	# Invoke functions from function by passed to "begin"
	end: () ->
		@tail.end()
		@tail.invoke()

# Make freezed Units which is invoked by "()"
# If freezed Units invoked with receiver, receiver is inject to @self.
class Def
	constructor: (block, @use_outer_scope = false) ->
		@factory = -> new Units block

	for p in ['_', 'catch']
		@::[p] = do (p) ->
			(block) ->
				previous_factory = @factory
				@factory = ->
					previous_factory()[p] block
				@

	end: ->
		factory = @factory
		use_outer_scope = @use_outer_scope
		defed = (args...) ->
					begin (->
						@next.apply @, args)
						, @, use_outer_scope
					._(factory())
					.end()
		defed.is_defed = true
		defed

# Receive (block, thisp, use_outer_scope)
begin = (args...) ->
	new Units args[0], args[1], args[2]

# Make freezed Units which can't access outer scope.
def = (block) ->
	new Def block

# Make freezed Units which can access outer scope.
macro = (block) ->
	new Def block, true

exports.begin = begin
exports.def = def
exports.macro = macro

# Component.coffee is a minimal library for when classical inheritence isn't
# enough. Components may be known to you as *traits*, or *mixins*, or something
# else. Multiple inheritance, like in Python, is also somewhat similar.
#
# The idea is to make your *code* conform to the problem space, instead of trying
# to shoehorn the problem into some design pattern. Here's a concrete example.


# ### Classical Inheritance

# You're making a game. The class structure, using classical inheritance is
# shown below (arrows denote a parentâ&#x2020;&#x2019;child relationship). You have objects
# which respond to gravity, like the player. You also have bullets, which
# kill, but don't respond to gravity. Bullet and Player share common code
# related to rendering and position, and everything is amazing.
#
#                         +-----------+    +------------+
#                         | *Physics* |    |  *Player*  |
#     +----------+   +--->|-----------+--->|------------|
#     |  *Base*  |   |    |  Gravity  |    |  Controls  |
#     |----------|   |    +-----------+    |    Walk    |
#     |  Render  +---+                     +------------+
#     | Position |   |    +----------+
#     +----------+   |    | *Bullet* |
#                    +--->|----------|
#                         |   Kill   |
#                         +----------+


# Now you want to add missiles which respond to gravity. You could do this:
#
#                         +-----------+    +------------+
#                         | *Physics* |    |  *Player*  |
#     +----------+   +--->|-----------+--->|------------|
#     |  *Base*  |   |    |  Gravity  |    |  Controls  |
#     |----------|   |    +-----------+    |    Walk    |
#     |  Render  +---+                     +------------+
#     | Position |   |    +----------+
#     +----------+   |    | *Weapon* |      +----------+
#                    +--->|----------|--+-->| *Bullet* |
#                         |   Kill   |  |   +----------+
#                         +----------+  |
#                                       |   +-----------+
#                                       |   | *Missile* |
#                                       +-->|-----------|
#                                           |  Gravity  |
#                                           +-----------+
#
#
# Bullets and missiles can share the code for killing, awesome. But there's
# a glaring issue here though: *code for gravity is duplicated*. You probably
# copy-pasted it, didn't you? Now any changes to gravity have to be done in two
# places simultaneously, *yuck*.


# How about this?:
# 
#     +----------+    +-----------+    +------------+
#     |  *Base*  |    | *Physics* |    |  *Player*  |
#     |----------+--->|-----------+--->|------------|
#     |  Render  |    |  Gravity  |    |  Controls  |
#     | Position |    +-----+-----+    |    Walk    |
#     +----------+          |          +------------+
#                           |
#                           v            +----------+
#                     +----------+   +-->| *Bullet* |
#                     | *Weapon* |   |   +----------+
#                     |----------|+--+
#                     |   Kill   |   |   +-----------+
#                     +----------+   +-->| *Missile* |
#                                        +-----------+
#
# Gravity isn't repeated, so it's good now, right? Except that bullets now have
# code related to gravity. This is workable if that gravity code is never
# called, but that's rather crufty. It's also makes the code harder to read,
# since inspecting the inheritance chain would have you believe that bullets do
# react to gravity.


# ### Components

# Let's see how using components solve this problem. The following diagram
# shows the same code, but using components instead of classical inheritance:
#
#                     +----------+    +-----+-----+
#     +----------+    |  *Base*  |    | *Physics* |
#     | *Bullet* |<---|----------|--->+-----------|
#     +----------+    |  Render  |    |  Gravity  |
#          ^          | Position |    +-----+-----+
#          |          +----------+          |      
#          |                                | 
#          |                +---------------+ 
#          |                |               |
#     +----+-----+          v               v
#     | *Weapon* |    +-----------+   +------------+
#     |----------+--->| *Missile* |   |  *Player*  |
#     |   Kill   |    +-----------+   |------------|
#     +----------+                    |  Controls  |
#                                     |    Walk    |
#                                     +------------+
#
# Physics includes Base functionality, as in classical inheritance. But now
# Missile borrows functionality from both the Physics and Weapon components.
# Bullets can take just the Base and Weapons components, without Physics, and
# Player needs to include only the Physics component.


# Let's see what this looks like in code. JavaScript follows, but in
# *CoffeeScript*, this looks like:
#
#     # This object literal becomes part of the prototype of
#     # the final component, as for Physics/Weapon/Player.
#     # So the usual caveats apply: mainly not putting per-
#     # instance variables into the prototype chain.
#     Base =
#       render: ->
#         console.log "Rendering at (#{@x}, #{@y})!"
#       init: (@x = 0, @y = 0)->
#         console.log "@init() called"
#
#     Physics = component Base,
#       gravity: (v = 1)-> @y += v
#
#     Weapon =
#       kill: (obj)->
#         delete GameObjects[obj.id]
#       
#     Missile = component Physics, Weapon
#     Bullet = component Base, Weapon
#
#     Player = component Physics,
#       walk: -> @x += 2
#       gravity: ->
#         console.log "Demonstrating super"
#         @super(3)
#       update: ->
#         @walk()
#         @gravity()
#         @render()
#     
#     me = new Player(10,10)  # @init() called
#     me.update()
#     me.render()             # Rendering at (12, 13)!


# Or the same thing in *JavaScript*:
#
#     // This object literal becomes part of the prototype of
#     // the final component, as for Physics/Weapon/Player.
#     // So the usual caveats apply: mainly not putting per-
#     // instance variables into the prototype chain.
#     Base = {
#       render: function() {
#         console.log(
#           "Rendering at (" + this.x + ", " + this.y + ")!"
#         );
#       },
#       init: function(x, y) {
#         this.x = x != null ? x : 0;
#         this.y = y != null ? y : 0;
#         return console.log("this.init() called");
#       }
#     };
#
#     Physics = component(Base, {
#       gravity: function(v) {
#         v = (v != null) ? v : 1;
#         this.y += v;
#       }
#     });
#
#     Weapon = {
#       kill: function(obj) {
#         return delete GameObjects[obj.id];
#       }
#     };
#
#     Missile = component(Physics, Weapon);
#     Bullet = component(Base, Weapon);
#
#     Player = component(Physics, {
#       walk: function() {
#         return this.x += 2;
#       },
#       gravity: function() {
#         console.log("Demonstrating super");
#         this.super(3);
#       },
#       update: function() {
#         this.walk();
#         this.gravity();
#         this.render();
#       }
#     });
#
#     me = new Player(10, 10); // this.init() called
#     me.update();
#     me.render();             // Rendering at (12, 13)!

# # Code

# This is the public interface to component.coffee.  
#
# *Parameters*: Any number of object literals or components (constructors)  
# *Returns*: A component. This is just a constructor function with a prototype
#            made up of the given objects/components. Use this constructor
#            function with the `new` keyword as usual. You can set your own
#            constructor with an `init` property, like so:
#
#      MyComponent = component({
#        init: function(){ this.x = 2 }
#      })
#      myInstance = new MyComponent
#      myInstance.x == 2
#
# Note that instances of components have built-in `extend` and `super` methods.
# The documentation for these functions is at `ComponentBase`. But here's an
# example, continuing from the above example:
#
#      myInstance.extend({y: function(){ return 2 }})
#      myInstance.extend({y: function(){ return 3 + this.super() })
#      myInstance.y() == 5
component = (components...)->

  # Create a base object to serve as the returned component's prototype
  comp = new ComponentBase
  comp.extend components...

  # A constructor function is returned as the component, ensuring that
  # object instantiation from the component is fast via `new`.
  # A user-defined `init` property is used as the constructor, if available.
  F = comp.init ? ->
  F.prototype = comp

  # Give F the same extension interface as a `new MyComponent()`
  F.extend = -> ComponentBase::extend.apply(F.prototype, arguments)
  return F

# Instances of `ComponentBase` serve as prototypes for components. This gives
# instances of components access to `@extend` and `@super` methods.
ComponentBase = ->

# `extend` copies properties from the arguments passed in to `this`  
#
# *Parameters*: Any number of object literals or components (constructors)  
# *Returns*: null
ComponentBase::extend = (components...)->
  for c in components
    # Allows extension  using both object literals and other components.  
    # Like this: `x = component(a:1); y = component(x, b:2)`
    c = c.prototype ? c

    # Copy all key/value pairs from given object to `this`. Create a `super`
    # property for functions that will be overwritten, except `extend`/`super`
    for key, val of c
      continue unless c.hasOwnProperty key
      if this[key] and typeof val is 'function' and not /extend|super/.test key
        old = this[key]
        this[key] = val
        this[key].super = old
      else
        this[key] = val
  return null

# Use this by calling `this.super(arg1, arg2)` in a component's function.
# The corresponding function that it overwrote earlier will be called.  
#
# *Parameters*: Arguments to be passed on  
# *Returns*: Whatever the overwritten function returns
ComponentBase::super = ->
  @super.caller.super.apply(this, arguments)

# Export in case CommonJS is being used
if module?.exports?
  module.exports = component

-- 
-- Implementation of a class
-- @func class : responsible of class and object creation
-- returns
--    @param class_object: object class
function class(class_object)
  local events_map = {
    -- Getter and Setters
    __index = '.', __newindex = '.=';
    -- Comparision
    __lt = '<', __le = '<=',  __eq = '==';
    -- Arithmetic
    __add = '+', __sub = '-', __mul = '*', __div = '/', __pow = '^';
    -- Unary negation
    __unm = '-neg';
    __concat = '..';
    __call = '__call';
    __tostring = 'tostring';
    __gc = 'destroy';
  }

  local class_object = class_object or {}
  local instance_protocol = { class = class_object }

  table.foreach(events_map, function(event, method)
    if event ~= '__index' and class_object[method] then
      instance_protocol[event] = class_object[method]
    elseif event == '__index' and class_object[events_map.__index] then
      instance_protocol.___index = class_object[events_map.__index]
    end
  end)

  function instance_protocol:__index(member)
    if type(class_object[member]) == 'function' then
      return function(...)
        return class_object[member](self, ...)
      end
    elseif type(member) == 'string' and type(class_object['.'..member]) == 'function' then
      return class_object['.'..member](self)
    elseif type(instance_protocol.___index) == 'function' then
      return instance_protocol.___index(self, member)
    else
      return self[member]
    end
  end

  function class_object.is_domain_of(instance)
    if getmetatable(instance) then
      return class_object == getmetatable(instance).class
	else
      return false
    end
  end

  function class_object.is_subclass_of(class)
    return class == table
  end

  local constructor = function(self, args)
    args = args or {}
    for key, value in pairs(args) do self[key] = value end
  end

  if type(class_object[1]) == 'function' then
    constructor = table.remove(class_object, 1)
    class_object.initialize = constructor
  elseif type(class_object.initialize) == 'function' then
    constructor = class_object.initialize
  else 
    class_object.initialize = constructor
  end

  local class_protocol = {
    __call = function(self, ...)
      local instance = {}

      setmetatable(instance, instance_protocol)
      constructor(instance, ...)

      return instance
    end
  }

  setmetatable(class_object, class_protocol)
  return class_object
end

-- vim:set ts=4 sw=4 sts=4 et:

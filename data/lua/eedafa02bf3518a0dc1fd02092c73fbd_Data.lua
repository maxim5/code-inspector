Data = class('Data'):include(WrapperIndex)

function Data:initialize(object)
  assert(object, "You're not meant to create Data objects directly.")
  self.object = object
end
"""interface module

An interface is...

An interface attribute is...

An interface directive is...

"""

import re
from collections import OrderedDict
from types import FunctionType


DIRECTIVE_RE = re.compile(r'^_(.*)_([a-z]+)$')


# helpers

def add_to_dict(d, name=None):
    def decorator(obj):
        if name is None:
            name = d[obj.__name__]
        d[name] = obj
        return obj
    return decorator


#################################################

def implements(interface, *names, validate=False):

    #check the names against the interface
    #tie into ABCMeta...
    
    def decorator(cls):
        if validate:
            # validate cls against interface
            ...
        return cls
    return decorator
    

# the types of interface attributes
class InterfaceAttribute:
    def __init__(self, name, docstring):
        self.docstring = docstring
        self.name = name
    def __repr__(self):
        return "{}({}, {})".format(self.
class InterfaceConstant(InterfaceAttribute):
    def __init__(self, name, value, docstring):
        super().__init__(name, docstring)
        self.value = value
class InterfaceComponent(InterfaceAttribute):
    def __init__(self, name, interface, docstring):
        super().__init__(name, docstring)
        self.interface = interface
class InterfaceMethod(InterfaceAttribute): pass
class InterfaceAlias(InterfaceAttribute): pass


# helpers for the interface attributes
def check_func(f):
    if ...: # function actually does something!
        warn("Interface alias body will be discarded")
def IFalias(f):
    """A decorator."""
    check_func(f)
    return InterfaceAlias(None, f.__doc__)


DIRECTIVES = {}

class InterfaceDirective:
    """Commands for an Interface object.

    """

    def __init__(self, name, value):
        self.name = name
        self.value = value

    def handle(self, interface):
        raise NotImplementedError

@add_to_dict(DIRECTIVES, "doc")
class DocDirective(InterfaceDirective):
    def handle(self, interface):
        if self.name not in interface.__all__:
            msg = "Ignoring explicit docstring for undefined attribute: {}"
            warn(msg.format(name))
            return

        attribute = interface.__all__[self.name]
        if attribute.docstring is not None:
            msg = "Overwriting docstring for existing attribute {}"
            warn(msg.format(name))
        else:
            attribute.docstring = self.value
    
@add_to_dict(DIRECTIVES, "inherit")
class InheritanceDirective(InterfaceDirective):
    def handle(self, interface):
        pass


class Interface:
    """The class for interface objects.

    For now the behavior regarding Interface subclasses is undefined.

    Attributes:
      __name__ - the Interface object's name.
      __all__ - the API.
      __directives__ - directives for the interface's behavior.
      __bases__ - any Interfaces from which this one assume attributes.
      {ATTRIBUTES}

    TODO:
      - make sure help() displays all the info for an interface...
    
    """

    def __init__(self, name, attributes=None, directives=(), bases=()):
        self.__name__ = name
        self.__bases__ = bases
        self.__directives__ = directives
        self.__all__ = attributes if attributes is not None else {}
        self._inheritance = {"global": "all", "include": [], "exclude": []}

        for directive in directives:
            directive.handle(self)

        self._inherit()

        for name, value in self.__all__.items():
            setattr(self, name, value)
        self.__doc__ = self.newdoc()

    def __new__(cls, *args, **kwargs):
        raise TypeError("Can't instantiate interfaces")

    def __repr__(self):
        return "<interface '{}'>".format(self.__name__)

    def __dir__(self):
        return tuple(self.__all__)

    def _handle_directive(self, directive):
        """Handle a single directive."""

        # handle explicit docstrings
        if isinstance(directive, DocDirective):
        elif isinstance(directive, InheritanceDirective

    def __inherit(self):

        # pull the inheritance directives
        global_ = "all"
        include = []
        exclude = []
        for name, value in self.directives.items():
            if not name.endswith("_inherit") or name == "__inherit":
                continue

        # handle the global inheritance directive
        global_ = self.directives["__inherit"]
        if global_ is None:
            return
        if global_ == "all":
            
        if global_ == "specific":
        elif global_ not in ("all", "specific"):
            msg = "Unknown global inheritance directive: {}"
            warn(msg.format(attrname, value))

        if 
        for interface in bases:
            self.assume(interface)

        for name, value in self.directives.items():
            if not name.endswith("_inherit") or name == "__inherit":
                continue

            attrname = name.strip("_")[:-7]

            # handle superfluous inheritance
            if attrname in self.__all__:
                msg = ("Ignoring inheritance directive for "
                       "already defined attribute: {}")
                warn(msg.format(attrname))
                if global_ == "all":
                    global_ = "specific"
                continue

            value = self.directives[name]

            # now inherit the attribute
            if value is True:
                if global_ == "all":
                    global_ = "specific"
                for interface in self.__bases__:
                    if attrname in interface.__all__:
                        self.__all__[attrname] = interface.__all__[attrname]
                        break
                else:
                    msg = "No inheritance found for {}"
                    warn(msg.format(attrname))
            else:
                msg = "Unknown inheritance directive for {}: {}"
                warn(msg.format(attrname, value))
        
        if global_ == "all":
            for interface in self.__bases__:
                for name in interface.__all__:
                    if name not in self.__all__:
                        self.__all__[name] = interface.__all__[name]

    def _assume(self, interface):
        """Pull the interface's attributes into self."""

    def _newdoc(self, doc=None):
        """Return a new docstring based on the current one.

        The new docstring will have entries for the various attributes,
        based on the docstrings of each attribute.  If a docstring is
        passed in, it is used instead of the Interface class's __doc__.

        """

        if doc is None:
            doc = type(self).__doc__
        doc = str(doc)
        ...
        return doc
            
   
def interface(cls):
    """A decorator to turn a class into an Interface object.

    Because classes do not have ordered dicts by default, no order is
    preserved here.

    """

    attrs = {}
    directives = {}
    _namespace = dir(cls)

    for name in _namespace:

        # ignore special methods
        if name.startswith("__") and name.endswith("__"):
            continue

        value = getattr(cls, name)

        # handle directives
        match = DIRECTIVE_RE.match(name)
        if match:
            dname = match.group("name")
            dtype = match.group("dtype")
            dcls = DIRECTIVES.get(match.group("dtype"))
            if dcls is None:
                msg = "Ignoring illegal directive type: {}"
                warn(msg.format(name))
            else:
                directives[dname] = dcls(dname, value)
            continue
        if name.startswith("_"):
            msg = "Ignoring illegal attribute name: {}"
            warn(msg.format(name))
            continue

        # handle the different attribute types
        if isinstance(value, InterfaceAttribute):
            pass
        elif name.isupper():
            value = InterfaceConstant(name, value, None)
        elif isinstance(value, str):
            value = InterfaceAttribute(name, value)
        elif isinstance(value, FunctionType):
            check_func(value)
            value = InterfaceMethod(name, value.__doc__)
        elif isinstance(value, InterfaceMeta):
            value = InterfaceComponent(name, value, None)
        else:
            msg = "Unknown iterface attribute type: {}"
            raise TypeError(msg.format(type(value)))
        
        # wrap it up
        if value.name is None:
            value.name = name
        attrs[name] = value
    
    new_interface = Interface(cls.__name__, attrs, directives, cls.__bases__)
    new_interface.__doc__ = new_interface._newdoc(cls.__doc__)
    return new_interface


#################################################

@interface
class ResourceIF:
    """The base Resource Interface."""


@interface
class SomeFeatureIF(ResourceIF):
    ...

@interface
class SomeResourceIF(ResourceIF):
    NAME1 = 5
    
    name1 = SomeInterface
    _name1_doc = "docstring"
    name2 = "docstring"
    _name3_inherit = True

    def method1(...):
        """docstring"""
    
    @IFalias
    def alias1(self):
        """docstring"""


@implements(SomeFeatureIF)
class SomeFeatureA(Feature):
    ...

@implements(SpamResourceIF)
@implements(SomeResourceIF, OtherResourceIF, validate=False)
class SomeResourceA(Resource):
    ...



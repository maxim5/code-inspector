"""Data hiding in Python:
"""

class MyClass:
    def __init__ (self, foo, bar):
        self.foo = foo
        self.bar = bar


class Accessor:
    def __init__ (self, my_class_instance):
        Accessor.foo = property(lambda self: my_class_instance.foo)
        Accessor.bar = property(lambda self: my_class_instance.bar)
        # my_class_instance reference is locked inside of function closures, so
        # it cannot be directly accessed from any Accessor instance:
        # the reference is not directly stored in any class or instance variable.
        # Since it is a reference, it will mirror any changes made to the MyClass
        # instance that it accesses.


my_data = MyClass(10, "113")
accessor = Accessor(my_data)
print(accessor.foo, accessor.bar)

my_data.bar = "231"
my_data.foo = [13, 14, 15]
print(accessor.foo, accessor.bar)

interface A { }
interface B { }
interface C { }
interface A<T> { }
interface B<T> { }

namespace MyNamespace
{
  interface A { }
  interface B { }
  interface C { }

  namespace SubNamespace
  {
    interface A<T> { }
    interface B<T> { }
    interface C<T> { }
  }
}

namespace MyNamespace.SubNamespace
{
  interface A<T, U> { }
  interface B<T, U> { }
  interface C<T, U> { }
}
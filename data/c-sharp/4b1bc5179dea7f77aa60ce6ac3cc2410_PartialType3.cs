interface A { }
interface B { }
interface C { }
interface A { }
interface B { }

namespace MyNamespace
{
  interface A { }
  interface B { }
  interface C { }

  namespace SubNamespace
  {
    interface A { }
    interface B { }
    interface C { }
  }
}

namespace MyNamespace.SubNamespace
{
  interface A { }
  interface B { }
  interface C { }
}
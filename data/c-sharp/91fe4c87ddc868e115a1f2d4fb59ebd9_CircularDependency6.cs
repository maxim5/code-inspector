interface A: A {}

class Master
{
  interface A : A { }
  class Inner
  {
    interface A : A { }
  }
}

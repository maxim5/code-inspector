interface B : D { }
interface C : B { }
interface D : C { }

class Master
{
  interface B : D { }
  interface C : B { }
  interface D : C { }
}
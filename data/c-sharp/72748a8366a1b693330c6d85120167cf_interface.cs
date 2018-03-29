interface IA
{
   void Print(); 
}

class B:IA
{
    public int x;
    void IA.Print(){System.Console.WriteLine(x);}
    public B(int x){this.x=x;}
}

struct C:IA
{
    public int x;
    public void Print(){System.Console.WriteLine(x);}
    public C(int x){this.x=x;}
    public static void Main()
    {
        IA b = new B(10),c = new C(10);
        Modify((B)b,(C)c);
        System.Console.WriteLine(((B)b).x + " " +((C)c).x);
    }
    public static void Modify(B b,C c)
    {
        c.x=b.x=20;

    }


}

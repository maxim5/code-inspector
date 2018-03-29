interface IA
{
    void Fn();
}
interface IB
{
    void Fn();
}
class C:IA,IB
{
     void IA.Fn()
    {
      System.Console.WriteLine("IN IA.fn");
    }
    void IB.Fn()
    {
      System.Console.WriteLine("IN IB.fn");
    }
    public static void Main()
    {
        C c = new C();
        ((IA)c).Fn();
        ((IB)c).Fn();
        //c.Fn();
    }

}

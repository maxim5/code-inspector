interface IList
{
  int Count { get; set; }
}

interface ICounter
{
  void Count(int i);
}

interface IListCounter : IList, ICounter { }


class A
{
  private static IListCounter x;
  //private static int a = x.Count;
  // warning CS0467: Ambiguity between method 'ICounter.Count(int)' and non-method 'IList.Count'. Using method group.
  // error CS0428: Cannot convert method group 'Count' to non-delegate type 'int'. Did you intend to invoke the method?
}


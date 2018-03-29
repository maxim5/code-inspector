interface cB<T> : cD<int> { }
interface cC<U> : cB<int> { }
interface cD<V> : cC<int> { }

class Master
{
  interface cB<T> : cD<int> { }
  interface cC<U> : cB<int> { }
  interface cD<V> : cC<int> { }
}
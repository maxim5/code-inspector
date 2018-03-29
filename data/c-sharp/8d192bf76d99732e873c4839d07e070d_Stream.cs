using System;
using System.Collections;

namespace MetaSharp.Transformation
{
    public class Stream : IStream, IComparable<IStream>
    {
        public readonly static IStream Empty = new Stream();

        private int _index;
        private IEnumerator _items;
        private bool _empty;
        private object _current;
        private IStream _next;
        private IStream _into;

        public bool IsEmpty
        {
            get { return _empty; }
        }

        public long Index
        {
            get { return _index + 1; }
        }

        public object Current
        {
            get
            {
                if (_index == -1)
                    throw new InvalidOperationException();
                return _current;
            }
        }

        public Stream(params object[] items)
        {
            if (items == null)
                throw new ArgumentNullException("items");

            this.Initialize(-1, items.GetEnumerator());
        }

        public Stream(IEnumerable items)
        {
            if (items == null)
                throw new ArgumentNullException("items");

            this.Initialize(-1, items.GetEnumerator());
        }

        private Stream(int index, IEnumerator items)
        {
            this.Initialize(index, items);
        }

        private void Initialize(int index, IEnumerator items)
        {
            _index = index;
            _items = items;
            if (_index > -1)
                _current = items.Current;

            _empty = !items.MoveNext();
        }

        public IStream Next()
        {
            if (_next == null)
            {
                lock (_items)
                {
                    if (_next == null)
                    {
                        _next = new Stream(_index + 1, _items);
                    }
                }
            }

            return _next;
        }

        public IStream Into()
        {
            if (_into == null)
            {
                lock (_items)
                {
                    if (_into == null)
                    {
                        var current = this.Current as IEnumerable;
                        if (current != null)
                            _into = new Stream(current);
                    }
                }
            }

            return _into;
        }

        public static bool operator <(Stream s1, Stream s2)
        {
            return op_Spaceship(s1, s2) < 0;
        }

        public static bool operator <=(Stream s1, Stream s2)
        {
            return op_Spaceship(s1, s2) < 1;
        }

        public static bool operator >(Stream s1, Stream s2)
        {
            return op_Spaceship(s1, s2) > 0;
        }

        public static bool operator >=(Stream s1, Stream s2)
        {
            return op_Spaceship(s1, s2) > -1;
        }

        private static int op_Spaceship(Stream s1, Stream s2)
        {
            return ((IComparable<IStream>)s1).CompareTo(s2);
        }

        int IComparable<IStream>.CompareTo(IStream other)
        {
            var stream = (Stream)other;
            return _index.CompareTo(stream._index);
        }

        int IComparable.CompareTo(object obj)
        {
            return ((IComparable<IStream>)this).CompareTo((IStream)obj);
        }
    }
}

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading;
using MySpace.Common.IO;

namespace MySpace.Common
{
	public delegate T PeriodicFactory<T>(TimeSpan duration);

	/// <summary>
	/// This class is designed to contain many frequently used operations using sorted and unsorted IEnumerable collections
	/// especially where we often allocate memory using List, SortedList, Dictionary, power collection classes etc
	/// Algorithms either do not use memory allocaton (except for few delegate and IEnumerable objects) or use minimum possible memory.
	/// Many of the methods can be found in extension methods in .NET 3.5, but are not available before that
	/// </summary>
	public static class Algorithm
	{
		#region set and order operations

		/// <summary>
		/// Computes intersection of two ordered collections
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">First ordered collection</param>
		/// <param name="b">Second ordered collection</param>
		/// <param name="comparison">A comparison method used to order collections</param>
		/// <returns>Ordered collection of items from intersection of two collections</returns>
		public static IEnumerable<T> Intersect<T>(IEnumerable<T> a, IEnumerable<T> b, Comparer<T> comparison)
		{
			if (a == null || b == null)
				yield break;

			IEnumerator<T> ita = a.GetEnumerator();
			IEnumerator<T> itb = b.GetEnumerator();

			bool goA = ita.MoveNext(), goB = itb.MoveNext();
			while (goA && goB)
			{
				int result = comparison.Compare(ita.Current, itb.Current);
				if (result < 0)
				{
					goA = ita.MoveNext();
				}
				else if (result > 0)
				{
					goB = itb.MoveNext();
				}
				else
				{
					yield return ita.Current;
					goA = ita.MoveNext();
					goB = itb.MoveNext();
				}
			}
		}

		/// <summary>
		/// Computes intersection of two non-descending collections
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">First non-descending collection</param>
		/// <param name="b">Second non-descending collection</param>
		/// <returns>Non-descending collection of items from intersection of two collections</returns>
		public static IEnumerable<T> Intersect<T>(IEnumerable<T> a, IEnumerable<T> b)
			where T : IComparable<T>
		{
			return Intersect(a, b, Comparer<T>.Default);
		}

		/// <summary>
		/// Computes union of two ordered collections
		/// </summary>
		/// <typeparam name="T">type of collection item</typeparam>
		/// <param name="a">First ordered collection</param>
		/// <param name="b">Second ordered collection</param>
		/// <param name="comparison">A comparison method used to order collections</param>
		/// <returns>Ordered collection of items from union of two collections</returns>
		public static IEnumerable<T> Union<T>(IEnumerable<T> a, IEnumerable<T> b, Comparer<T> comparison)
		{
			IEnumerator<T> ita = (a ?? Enumerate<T>()).GetEnumerator();
			IEnumerator<T> itb = (b ?? Enumerate<T>()).GetEnumerator();

			bool goA = ita.MoveNext(), goB = itb.MoveNext();
			while (goA || goB)
			{
				if (goA == false)
				{
					do
					{
						yield return itb.Current;
					} while (itb.MoveNext());
					yield break;
				}
				if (goB == false)
				{
					do
					{
						yield return ita.Current;
					} while (ita.MoveNext());
					yield break;
				}
				int result = comparison.Compare(ita.Current, itb.Current);
				if (result < 0)
				{
					yield return ita.Current;
					goA = ita.MoveNext();
				}
				else if (result > 0)
				{
					yield return itb.Current;
					goB = itb.MoveNext();
				}
				else
				{
					yield return ita.Current;
					goA = ita.MoveNext();
					goB = itb.MoveNext();
				}
			}
		}

		/// <summary>
		/// Computes union of two non-descending collections
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">First non-descending collection</param>
		/// <param name="b">Second non-descending collection</param>
		/// <returns>Non-descending collection of items from union of two collections</returns>
		public static IEnumerable<T> Union<T>(IEnumerable<T> a, IEnumerable<T> b)
			where T : IComparable<T>
		{
			return Union(a, b, Comparer<T>.Default);
		}

		/// <summary>
		/// Computes difference of two ordered collections
		/// </summary>
		/// <typeparam name="T">type of collection item</typeparam>
		/// <param name="a">First ordered collection</param>
		/// <param name="b">Second ordered collection</param>
		/// <param name="comparison">A comparison method used to order collections</param>
		/// <returns>Ordered collection of items from difference of two collections</returns>
		public static IEnumerable<T> Subtract<T>(IEnumerable<T> a, IEnumerable<T> b, Comparer<T> comparison)
		{
			if (a == null)
				yield break;

			IEnumerator<T> ita = a.GetEnumerator();
			IEnumerator<T> itb = (b ?? Enumerate<T>()).GetEnumerator();

			bool goA = ita.MoveNext(), goB = itb.MoveNext();
			while (goA)
			{
				if (goB == false)
				{
					do
					{
						yield return ita.Current;
					} while (ita.MoveNext());
					yield break;
				}
				int result = comparison.Compare(ita.Current, itb.Current);
				if (result < 0)
				{
					yield return ita.Current;
					goA = ita.MoveNext();
				}
				else if (result > 0)
				{
					goB = itb.MoveNext();
				}
				else
				{
					goA = ita.MoveNext();
					goB = itb.MoveNext();
				}
			}
		}

		/// <summary>
		/// Computes difference of two non-descending collections
		/// </summary>
		/// <typeparam name="T">type of collection item</typeparam>
		/// <param name="a">First non-descending collection</param>
		/// <param name="b">Second non-descending collection</param>
		/// <returns>Non-descending collection of items from difference of two collections</returns>
		public static IEnumerable<T> Subtract<T>(IEnumerable<T> a, IEnumerable<T> b)
			where T : IComparable<T>
		{
			return Subtract(a, b, Comparer<T>.Default);
		}

		/// <summary>
		/// True iff first ordered collection is a subset of the second ordered collection
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">First ordered collection</param>
		/// <param name="b">Second ordered collection</param>
		/// <param name="comparison">A comparison method used to order collections</param>
		/// <returns>True iff first collection is a subset of second collection</returns>
		public static bool IsSubsetOf<T>(IEnumerable<T> a, IEnumerable<T> b, Comparer<T> comparison)
		{
			return IsEmpty(Subtract(a, b, comparison));
		}

		/// <summary>
		/// True iff first non-descending collection is a subset of the second non-descending collection
		/// </summary>
		/// <typeparam name="T">type of collection item</typeparam>
		/// <param name="a">First non-descending collection</param>
		/// <param name="b">Second non-descending collection</param>
		/// <returns>True iff first collection is a subset of second collection</returns>
		public static bool IsSubsetOf<T>(IEnumerable<T> a, IEnumerable<T> b)
			where T : IComparable<T>
		{
			return IsSubsetOf(a, b, Comparer<T>.Default);
		}

		/// <summary>
		/// True iff first ordered collection is a proper (not equal) subset of the second ordered collection
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">First ordered collection</param>
		/// <param name="b">Second ordered collection</param>
		/// <param name="comparison">A comparison method used to order collections</param>
		/// <returns>True iff first ordered collection is a proper (not equal) subset of the second collection</returns>
		public static bool IsProperSubsetOf<T>(IEnumerable<T> a, IEnumerable<T> b, Comparer<T> comparison)
		{
			return IsEmpty(Subtract(a, b, comparison)) && false == IsEmpty(Subtract(b, a, comparison));
		}

		/// <summary>
		/// True iff first non-descending collection is a proper (not equal) subset of the second non-descending collection
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">First non-descending collection</param>
		/// <param name="b">Second non-descending collection</param>
		/// <returns>True iff first collection is a proper subset of second collection</returns>
		public static bool IsProperSubsetOf<T>(IEnumerable<T> a, IEnumerable<T> b)
			where T : IComparable<T>
		{
			return IsProperSubsetOf(a, b, Comparer<T>.Default);
		}

		/// <summary>
		/// Intersection of two dictionaries
		/// </summary>
		/// <typeparam name="K">Key type</typeparam>
		/// <typeparam name="T">Value type</typeparam>
		/// <param name="a">First dictionary</param>
		/// <param name="b">Second dictionary</param>
		/// <returns>all key value pairs from the first that are also in the second dictionary</returns>
		public static IEnumerable<KeyValuePair<K, T>> Intersect<K, T>(IDictionary<K, T> a, IDictionary<K, T> b)
		{
			if (a == null || b == null)
				yield break;

			IDictionary<K, T> left = a;
			IDictionary<K, T> right = b;

			ExchangeIf(left.Count > right.Count, ref left, ref right);

			foreach (KeyValuePair<K, T> pair in left)
			{
				T value;
				if (right.TryGetValue(pair.Key, out value))
					yield return pair;
			}
		}

		/// <summary>
		/// Union of two dictionaries
		/// </summary>
		/// <typeparam name="K">Key type</typeparam>
		/// <typeparam name="T">Value type</typeparam>
		/// <param name="a">First dictionary</param>
		/// <param name="b">Second dictionary</param>
		/// <returns>all key value pairs that are in both dictinaries</returns>
		public static IEnumerable<KeyValuePair<K, T>> Union<K, T>(IDictionary<K, T> a, IDictionary<K, T> b)
		{
			if (a == null && b == null)
				yield break;

			IDictionary<K, T> left = a ?? b;
			IDictionary<K, T> right = b ?? a;

			if (false == ReferenceEquals(left, right))
			{
				ExchangeIf(left.Count > right.Count, ref left, ref right);
			}

			foreach (KeyValuePair<K, T> pair in right)
			{
				yield return pair;
			}

			if (false == ReferenceEquals(left, right))
			{
				foreach (KeyValuePair<K, T> pair in left)
				{
					T value;
					if (false == right.TryGetValue(pair.Key, out value))
						yield return pair;
				}
			}
		}

		/// <summary>
		/// Difference between two dictionaries
		/// </summary>
		/// <typeparam name="K">Key type</typeparam>
		/// <typeparam name="T">Value type</typeparam>
		/// <param name="a">Dictionary to subtract from</param>
		/// <param name="b">Dictionary to subtract</param>
		/// <returns>all key value pairs that are in the first but not in the second dictionary</returns>
		public static IEnumerable<KeyValuePair<K, T>> Subtract<K, T>(IDictionary<K, T> a, IDictionary<K, T> b)
		{
			if (a == null)
				yield break;

			if (b == null)
			{
				foreach (KeyValuePair<K, T> pair in a)
					yield return pair;
			}

			foreach (KeyValuePair<K, T> pair in a)
			{
				T value;
				if (false == b.TryGetValue(pair.Key, out value))
					yield return pair;
			}
		}

		/// <summary>
		/// True iff the first dictinary is a subset of second dictionary
		/// </summary>
		/// <typeparam name="K">Key type</typeparam>
		/// <typeparam name="T">Value type</typeparam>
		/// <param name="a">First dictionary</param>
		/// <param name="b">Second dictionary</param>
		/// <returns>True iff the first dictinary is a subset of second dictionary</returns>
		public static bool IsSubsetOf<K, T>(IDictionary<K, T> a, IDictionary<K, T> b)
		{
			return IsEmpty(Subtract(a, b));
		}

		/// <summary>
		/// True iff first dictionary is a proper (not equal) subset of a second dictionary
		/// </summary>
		/// <typeparam name="K">Key type</typeparam>
		/// <typeparam name="T">Value type</typeparam>
		/// <param name="a">First dictionary</param>
		/// <param name="b">Second dictionary</param>
		/// <returns>True iff first dictionary is a proper (not equal) subset of a second dictionary</returns>
		public static bool IsProperSubsetOf<K, T>(IDictionary<K, T> a, IDictionary<K, T> b)
		{
			return IsEmpty(Subtract(a, b)) && false == IsEmpty(Subtract(b, a));
		}

		/// <summary>
		/// True iff collection is ordered by given comparison method X[i] <= X[i+1]
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">Collection to test the order</param>
		/// <param name="comparison">Comparison method defining the order</param>
		/// <returns>True iff collection is in non-descending order given comparison method</returns>
		public static bool IsOrdered<T>(IEnumerable<T> a, Comparer<T> comparison)
		{
			if (a == null)
				return false;

			IEnumerator<T> it = a.GetEnumerator();
			if (false == it.MoveNext())
				return true;

			T value = it.Current;
			while (it.MoveNext())
			{
				if (comparison.Compare(value, it.Current) > 0)
					return false;
				value = it.Current;
			}
			return true;
		}

		/// <summary>
		/// True iff collectin is strictly ordered by given comparison method X[i] < X[i+1]
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">A collection to test order</param>
		/// <param name="comparison">Comparison method defining the order</param>
		/// <returns>True off collection is in strictly asending order given comparison method</returns>
		public static bool IsStrictlyOrdered<T>(IEnumerable<T> a, Comparer<T> comparison)
		{
			return IsOrdered(a, new StrictlyGreaterComparer<T>(comparison));
		}

		/// <summary>
		/// True iff collection is in non-descending order
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">Collection to test order</param>
		/// <returns>True iff collection is in non-descending order of items</returns>
		public static bool IsNonDescending<T>(IEnumerable<T> a)
			where T : IComparable<T>
		{
			return IsOrdered(a, Comparer<T>.Default);
		}

		/// <summary>
		/// True iff collection is in ascending order
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">Collection to test order</param>
		/// <returns>True iff collection is in ascending order of items</returns>
		public static bool IsAscending<T>(IEnumerable<T> a)
			where T : IComparable<T>
		{
			return IsStrictlyOrdered(a, Comparer<T>.Default);
		}

		/// <summary>
		/// Distinct items from original ordered collection.
		/// </summary>
		/// <typeparam name="T">The type of items in the collection.</typeparam>
		/// <param name="collection">An ordered collection.</param>
		/// <param name="comperer">A comparer object used to order collection.</param>
		/// <returns>A distinct set of items in the same order as original collection.</returns>
		public static IEnumerable<T> Distinct<T>(IEnumerable<T> collection, IComparer<T> comperer)
		{
			if (collection == null)
				yield break;

			IEnumerator<T> it = collection.GetEnumerator();
			if (false == it.MoveNext())
				yield break;

			T value = it.Current;
			yield return value;

			while (it.MoveNext())
			{
				if (comperer.Compare(value, it.Current) == 0)
					continue;
				value = it.Current;
				yield return value;
			}
		}

		public static IEnumerable<T> Distinct<T>(IEnumerable<T> a)
			where T : IComparable<T>
		{
			return Distinct(a, Comparer<T>.Default);
		}

		public static IDictionary<T, int> Intersect<T>(IEnumerable<IEnumerable<T>> sets, IEqualityComparer<T> comparer)
		{
			int count = 0;
			Dictionary<T, int> result = new Dictionary<T, int>(comparer);

			if (sets == null)
				return result;

			// subtract all other collections from the first one
			foreach (IEnumerable<T> it in sets)
			{
				if (++count == 1)
				{
					foreach (T key in it)
						result[key] = count;

					if (result.Count == 0)
						break;

					continue;
				}

				int countLeft = 0;
				foreach (T key in it)
				{
					int value;
					if (result.TryGetValue(key, out value))
					{
						if (value + 1 == count)
						{
							result[key] = count;
							++countLeft;
						}
						else
						{
							result.Remove(key);
						}
					}
				}

				if (countLeft == 0)
				{
					result.Clear();
					return result;
				}
			}

			// only values that belong to all sets
			foreach (T key2 in RemoveAll(result, key => result[key] < count)) ;

			return result;
		}

		/// <summary>
		/// Intersect collection of non-descending collections
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="sets">A collection of ordered collections to intersect</param>
		/// <returns>Items that belong to every non-descending collection in the input collection</returns>
		public static IDictionary<T, int> Intersect<T>(IEnumerable<IEnumerable<T>> sets)
		{
			return Intersect(sets, EqualityComparer<T>.Default);
		}

		public static IEnumerable<KeyValuePair<K, V>> Intersect<K, V>(IDictionary<K, V> a, IEnumerable<K> b)
		{
			foreach (K key in b)
			{
				if (a.ContainsKey(key))
					yield return new KeyValuePair<K, V>(key, a[key]);
			}
		}

		public static IDictionary<T, int> Union<T>(IEnumerable<IEnumerable<T>> sets, IEqualityComparer<T> comparer)
		{
			Dictionary<T, int> result = new Dictionary<T, int>(comparer);

			if (sets == null)
				return result;

			// add all collections to the dictionary
			foreach (IEnumerable<T> it in sets)
			{
				foreach (T key in it)
				{
					int value;
					if (false == result.TryGetValue(key, out value))
						value = 0;
					result[key] = ++value;
				}
			}
			return result;
		}

		public static IDictionary<T, int> Union<T>(IEnumerable<IEnumerable<T>> sets)
		{
			return Union(sets, EqualityComparer<T>.Default);
		}

		public static IEnumerable<K> RemoveAll<K, V>(IDictionary<K, V> a, Predicate<K> condition)
		{
			List<K> removed = new List<K>(FindAll(a.Keys, condition));
			foreach (K key in removed)
			{
				if (a.Remove(key))
					yield return key;
			}
		}

		public static IEnumerable<KeyValuePair<K, V>> RemoveAll2<K, V>(IDictionary<K, V> a,
			Predicate<KeyValuePair<K, V>> condition)
		{
			List<KeyValuePair<K, V>> removed = new List<KeyValuePair<K, V>>(FindAll(a, condition));
			foreach (KeyValuePair<K, V> pair in removed)
			{
				if (a.Remove(pair.Key))
					yield return pair;
			}
		}

		/// <summary>
		/// 	<para>Maps the specified value set of type <typeparamref name="TIn"/>
		/// 	to another value set of type <typeparamref name="TOut"/>.</para>
		/// </summary>
		/// <typeparam name="TIn">The type of values in the input set.</typeparam>
		/// <typeparam name="TOut">The type of values in the output set.</typeparam>
		/// <param name="values">The values to map.</param>
		/// <param name="mapper">The the method to map each value.</param>
		/// <returns>
		///	<para>A set of mapped values.</para>
		/// </returns>
		/// <exception cref="ArgumentNullException">
		///	<para><paramref name="values"/> is <see langword="null"/>.</para>
		///	<para>- or -</para>
		///	<para><paramref name="mapper"/> is <see langword="null"/>.</para>
		/// </exception>
		public static IEnumerable<TOut> Map<TIn, TOut>(IEnumerable<TIn> values, Factory<TIn, TOut> mapper)
		{
			if (values == null) throw new ArgumentNullException("values");
			if (mapper == null) throw new ArgumentNullException("mapper");

			foreach (var value in values)
			{
				yield return mapper(value);
			}
		}

		#endregion

		#region comparison

		/// <summary>
		/// True iff collection is empty
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">A collection to test</param>
		/// <returns>True iff input collection is empty</returns>
		public static bool IsEmpty<T>(IEnumerable<T> a)
		{
			if (a == null)
				return true;
			return a.GetEnumerator().MoveNext() == false;
		}

		/// <summary>
		/// Does deep ordered (vector) comparison of two collections with given comparison operator
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">First collection</param>
		/// <param name="b">Second collection</param>
		/// <param name="comparison">A comparison operator</param>
		/// <returns>-1: a is less than b, +1 a is greater than b, 0 if both are equal</returns>
		public static int CompareTo<T>(IEnumerable<T> a, IEnumerable<T> b, Comparer<T> comparison)
		{
			IEnumerator<T> ita = (a ?? Enumerate<T>()).GetEnumerator();
			IEnumerator<T> itb = (b ?? Enumerate<T>()).GetEnumerator();

			bool goA = ita.MoveNext(), goB = itb.MoveNext();
			while (goA && goB)
			{
				int result = comparison.Compare(ita.Current, itb.Current);
				if (result != 0)
					return result;
				goA = ita.MoveNext();
				goB = itb.MoveNext();
			}
			if (goA)
				return 1;
			if (goB)
				return -1;
			return 0;
		}

		/// <summary>
		/// Does deep ordered (vector) comparison of two collections using comparison of type T 
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">First collection</param>
		/// <param name="b">Second collection</param>
		/// <returns>-1: a is less than b, +1 a is greater than b, 0 if both are equal</returns>
		public static int CompareTo<T>(IEnumerable<T> a, IEnumerable<T> b)
			where T : IComparable<T>
		{
			return CompareTo(a, b, Comparer<T>.Default);
		}

		/// <summary>
		/// True iff two collections are strongly equal (vector comparison) using provided comparison operator
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">First collection</param>
		/// <param name="b">Second collection</param>
		/// <param name="comparison">comparison operator</param>
		/// <returns>True iff two collections are strongly equal (vector comparison)</returns>
		public static bool Equals<T>(IEnumerable<T> a, IEnumerable<T> b, Comparer<T> comparison)
		{
			if (ReferenceEquals(a, b))
				return true;

			if (a == null || b == null)
				return false;

			return CompareTo(a, b, comparison) == 0;
		}

		/// <summary>
		/// True iff two collections are strongly equal (vector comparison)
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">First collection</param>
		/// <param name="b">Second collection</param>
		/// <returns>True iff two collections are strongly equal (vector comparison)</returns>
		public static bool Equals<T>(IEnumerable<T> a, IEnumerable<T> b)
			where T : IComparable<T>
		{
			return Equals(a, b, Comparer<T>.Default);
		}

		/// <summary>
		/// Does deep ordered (vector comparison of all {Key, Value} pairs) comparison of two dictionaries
		/// NOTE: Algorithm is optimal for SortedDictionary
		/// NOTE: Avoid using this method if having large unordered dictionaries
		/// </summary>
		/// <typeparam name="K">Key type</typeparam>
		/// <typeparam name="T">Value type</typeparam>
		/// <param name="a">First dictionary</param>
		/// <param name="b">Second dictionary</param>
		/// <returns>-1: a is less than b, +1 a is greater than b, 0 if both are equal</returns>
		public static int CompareTo<K, T>(IDictionary<K, T> a, IDictionary<K, T> b)
			where K : IComparable<K>
			where T : IComparable<T>
		{
			if (ReferenceEquals(a, b))
				return 0;

			if (a == null)
				return -1;

			if (b == null)
				return 1;

			KeyValuePairComparer<K, T> comparer = new KeyValuePairComparer<K, T>();

			IEnumerable<KeyValuePair<K, T>> left;
			if (a is SortedDictionary<K, T>)
			{
				left = a;
			}
			else
			{
				List<KeyValuePair<K, T>> leftList = new List<KeyValuePair<K, T>>(a.Count);
				leftList.AddRange(a);
				leftList.Sort(comparer);
				left = leftList;
			}
			IEnumerable<KeyValuePair<K, T>> right;
			if (b is SortedDictionary<K, T>)
			{
				right = b;
			}
			else
			{
				List<KeyValuePair<K, T>> rightList = new List<KeyValuePair<K, T>>(b.Count);
				rightList.AddRange(b);
				rightList.Sort(comparer);
				right = rightList;
			}

			return CompareTo(left, right, comparer);
		}

		/// <summary>
		/// True iff two dictionaries are strongly equal (vector comparison of all {Key, Value} pairs
		/// NOTE: Algorithm is optimal for SortedDictionary
		/// NOTE: Avoid using this method if having large unordered dictionaries
		/// </summary>
		/// <typeparam name="K">Key type</typeparam>
		/// <typeparam name="T">Value type</typeparam>
		/// <param name="a">First dictionary</param>
		/// <param name="b">Second dictionary</param>
		/// <returns>True iff two dictionaries are strongly equal</returns>
		public static bool Equals<K, T>(IDictionary<K, T> a, IDictionary<K, T> b)
			where K : IComparable<K>
			where T : IComparable<T>
		{
			if (ReferenceEquals(a, b))
				return true;

			if (a == null || b == null)
				return false;

			return CompareTo(a, b) == 0;
		}


		public static bool In<T>(T value, IEnumerable<T> a) where T : IComparable<T>
		{
			if (a == null)
				return false;

			foreach (T value2 in a)
			{
				if (value2.CompareTo(value) == 0)
					return true;
			}
			return false;
		}

		public static bool In<T>(T value, params T[] a) where T : IComparable<T>
		{
			return In(value, a as IEnumerable<T>);
		}

		#endregion

		#region sequence, range and random subset operations

		/// <summary>
		/// Yields an ascending sequence of integers from smallest first to largest last inclusive
		/// </summary>
		/// <param name="first">The first (smallest) item to include</param>
		/// <param name="last">The last (largest) item to include</param>
		/// <returns>Dense sequence of ascending integers from first to last inclusive</returns>
		/// <example>
		/// (1) Create disjoint range collections without allocating memory:
		///		foreach(int index in Union(Sequence(1, 5), Sequence(100, 150)) {}
		///		foreach(int index in Subtract(Sequence(1, 100), Enumerate(1, 19, 56)) {}
		/// (2) Provide argument to the method that accepts IEnumerable (without allocation like list.Range() does)
		///		Logger.LogDebug(ToCSV(Sequence(10, 50), ','))
		/// </example>
		public static IEnumerable<int> Sequence(int first, int last)
		{
			while (first <= last)
				yield return first++;
		}

		/// <summary>
		/// Yields a descending sequence of integers from largest first to smallest last inclusive
		/// </summary>
		/// <param name="first">The first (largest) item to include</param>
		/// <param name="last">The last (smallest) item to include</param>
		/// <returns>Dense sequence of descending integers</returns>
		/// <example>
		///  (1) Correctly iterate things in reverse order - following code is error free and more descriptive
		///		foreach(int index in ReverseSequence(60, 21) {}
		///  (2) Provide argument to the method that accepts IEnumerable (reversing without allocation for example)
		///		IEnumerable[T] x = ConvertAll(ReverseSequence(60, 21), index => list[index])
		/// </example>
		public static IEnumerable<int> ReverseSequence(int first, int last)
		{
			while (first >= last)
				yield return first--;
		}

		/// <summary>
		/// yields an ascending sequence of numbers with cirular (round robin) ordering given sequence start and length
		/// NOTE: if length of sequence exceeds the period - duplicate numbers are returned
		/// </summary>
		/// <param name="period">the number of items in a circle - a period of a sequence</param>
		/// <param name="first">first number in a sequence - any number will be projected to the period</param>
		/// <param name="length">number of items in a sequence</param>
		/// <returns>
		/// a sequence of numbers starting from first and going in circular order
		/// with exactly count items in the sequence and having maximum value of (period - 1)
		/// </returns>
		public static IEnumerable<int> RoundSequence(int period, int first, int length)
		{
			if (period <= 0 || length <= 0)
				yield break;

			first = first % period;
			if (first < 0)
				first += period;

			while (length-- > 0)
			{
				yield return first;
				if (++first == period)
					first = 0;
			}
		}

		public static IEnumerable<int> ReverseRoundSequence(int period, int first, int length)
		{
			if (period <= 0 || length <= 0)
				yield break;

			first = first % period;
			if (first < 0)
				first += period;

			while (length-- > 0)
			{
				yield return first;
				if (--first < 0)
					first += period;
			}
		}

		/// <summary>
		/// yields an ascending sequence of numbers with circular (round robin) ordering with 
		/// given sequence center element and sequence radius (number of elements on both sides of a center)
		/// NOTE: if length of sequence exceeds the period - duplicate numbers are returned
		/// </summary>
		/// <param name="period">the number of items in a circle - a period of a sequence</param>
		/// <param name="center">a center element of a sequence - any number is projected to the period</param>
		/// <param name="radius">number of elements of both sides of a center element</param>
		/// <returns>
		/// a sequence of numbers starting from a number radius items to the left from the center and ending
		/// with number radius items to the right of the center in circular order with exactly 2*radius + 1
		/// numbers in the sequence and having maximum value of (period - 1) 
		/// </returns>
		public static IEnumerable<int> RoundCenteredSequence(int period, int center, int radius)
		{
			if (period <= 0 || radius < 0)
				yield break;

			foreach (int value in RoundSequence(period, (center - radius) % period, radius * 2 + 1))
				yield return value;
		}

		public static IEnumerable<int> ReverseRoundCenteredSequence(int period, int center, int radius)
		{
			if (period <= 0 || radius < 0)
				yield break;

			foreach (int value in ReverseRoundSequence(period, (center + radius) % period, radius * 2 + 1))
				yield return value;
		}

		public static IEnumerable<T> First<T>(IEnumerable<T> fullSet, int count)
		{
			if (fullSet == null || count < 1)
				yield break;

			foreach (T item in fullSet)
			{
				yield return item;
				if (--count < 1)
					yield break;
			}
		}

		/// <summary>
		/// Yields all integers in ascending order from the first range that belong to the second range
		/// </summary>
		/// <param name="first">The first (largest) item of the first range</param>
		/// <param name="last">The last (smallest) item of the first range</param>
		/// <param name="firstFrom">The first (largest) item of the second range</param>
		/// <param name="lastFrom">The second (smallest) item of the first range</param>
		/// <returns>dense sequence of ascending integers from the first range that belong to the second range</returns>
		public static IEnumerable<int> Range(int first, int last, int firstFrom, int lastFrom)
		{
			if (first > lastFrom)
				yield break;

			if (last < firstFrom)
				yield break;

			if (first < firstFrom)
				first = firstFrom;

			if (last > lastFrom)
				last = lastFrom;

			while (first <= last)
				yield return first++;
		}

		/// <summary>
		/// Yields ascending sequence of integers from defined range of a collection
		/// </summary>
		/// <param name="first">The first (largest) item to include</param>
		/// <param name="last">The last (smallest) item to include</param>
		/// <param name="count">Number of items in the collection</param>
		/// <returns>correct range of a collection</returns>
		/// <example>
		/// (1) Does all the checking when taking the range/page from a list - no index out of bounds exceptions
		///		foreach(int index in Range(10, 50, list.Count))
		///		{
		///			MyItem item = list[index];
		///		}
		/// </example>
		public static IEnumerable<int> Range(int first, int last, int count)
		{
			return Range(first, last, 0, count - 1);
		}

		/// <summary>
		/// Creates a random permutation (no repetition) of integers from first to last inclusive
		/// NOTE: may allocate int[last - first + 1] space
		/// </summary>
		/// <param name="generator">A pseudo-random number generator</param>
		/// <param name="first">Smallest integer to consider</param>
		/// <param name="last">Largest integer to consider</param>
		/// <returns>A randomly permuted sequence of integers from given range</returns>
		public static IEnumerable<int> RandomPermutation(Random generator, int first, int last)
		{
			return RandomSubset(generator, first, last, last - first + 1);
		}

		/// <summary>
		/// Creates a random permutation (no repetition) of items in the list
		/// NOTE: may allocate int[list.Count] space
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="generator">A pseudo-random number generator</param>
		/// <param name="list">A list which is randomly permuted</param>
		/// <returns>A random permutation of items in the list</returns>
		/// <example>
		/// Create random permutation of a random sample of top 500 friends
		///		IList<int> fiendList = GetTopFriends(userId, 500);
		///		IList<int> result = new List<int>(500);
		///		result.AddRange(RandomPermutation(new Random(), friendList));		
		/// </example>
		public static IEnumerable<T> RandomPermutation<T>(Random generator, IList<T> list)
		{
			return RandomSubset(generator, list, list.Count);
		}

		/// <summary>
		/// Creates a random permutation of a random subset of integers from first to last inclusive
		/// NOTE: optimal for samples sizes close to range size
		/// NOTE: close to optimal when sample sizes are much smaller than range size
		/// NOTE: may allocate O(int[subsetSize]) space
		/// </summary>
		/// <param name="generator">A pseudo-random number generator</param>
		/// <param name="first">Smallest integer to consider</param>
		/// <param name="last">Largest integer to consider</param>
		/// <param name="subsetSize">Number of items in the subset</param>
		/// <returns>A random permutation of a random subset of integer range</returns>
		public static IEnumerable<int> RandomSubset(Random generator, int first, int last, int subsetSize)
		{
			int size = last - first + 1;
			return size < subsetSize + subsetSize || size < 100
						?
							RandomSubsetUsingArray(generator, first, last, subsetSize)
						:
							RandomSubsetUsingDictionary(generator, first, last, subsetSize);
		}

		/// <summary>
		/// Creates a random permutation of a random subset of items in aa given list
		/// NOTE: may allocate O(int[subsetSize]) space
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="generator">A pseudo-random number generator</param>
		/// <param name="list">A list to compute random subset</param>
		/// <param name="subsetSize">Number of items in the subset</param>
		/// <returns>A random permutation of a random subset of items in given list</returns>
		public static IEnumerable<T> RandomSubset<T>(Random generator, IList<T> list, int subsetSize)
		{
			if (list == null)
				yield break;

			foreach (int index in RandomSubset(generator, 0, list.Count - 1, subsetSize))
			{
				yield return list[index];
			}
		}

		/// <summary>
		/// Creates a random permutation of a random subset of all items stored in a list of item lists
		/// NOTE: this algorithm works the best with small (<100) number of long lists
		/// NOTE: originally created for Music JV project
		/// NOTE: may allocate at most O(int[subsetSize]) space
		/// </summary>
		/// <param name="generator">A pseudo-random number generator</param>
		/// <param name="listCounts">A list of counts in child collections</param>
		/// <param name="subsetSize">Number of items in the subset</param>
		/// <returns>
		/// A random permutation of a random subset of items in all lists as a KeyValuePairs with
		/// Key as parent index and Value as a child collection index
		/// </returns>
		public static IEnumerable<KeyValuePair<int, int>> RandomComplexSubset(
			Random generator, int[] listCounts, int subsetSize)
		{
			//
			// this algorithm works the best with small number of long lists
			// when large number of lists is invloved it becomes sub-optimal
			// since it scans the list of lists to map the element
			//
			// the often used alternative is to allocate list that contains all lists before generating 
			// random sample which creates ballon effect when small sample is requested from large population
			//
			if (listCounts.Length == 0)
				yield break;

			int count = 0;
			foreach (int listCount in listCounts)
				count += listCount;

			int listIndex = 0, firstItemIndex = 0;
			foreach (int index in RandomSubset(generator, 0, count - 1, subsetSize))
			{
				if (listCounts.Length == 1)
				{
					yield return new KeyValuePair<int, int>(0, index);
					continue;
				}
				FindListIndexUsingBookmark(listCounts, index, ref listIndex, ref firstItemIndex);
				yield return new KeyValuePair<int, int>(listIndex, index - firstItemIndex);
			}
		}

		#endregion

		#region enumerate, find and convert operations

		private class AnonymousDisposable : IDisposable
		{
			private readonly Action _disposeAction;
			private bool _disposed;

			public AnonymousDisposable(Action disposeAction)
			{
				_disposeAction = disposeAction;
			}

			#region IDisposable Members

			public void Dispose()
			{
				if (!_disposed)
				{
					_disposeAction();
					_disposed = true;
				}
			}

			#endregion
		}

		/// <summary>
		/// Creates an anonymous <see cref="IDisposable"/> implementation from <paramref name="disposer"/>.
		/// </summary>
		/// <param name="disposer">The dispose action.</param>
		/// <returns>An anonymous <see cref="IDisposable"/> implementation from <paramref name="disposer"/>.</returns>
		/// <exception cref="ArgumentNullException">
		///		<para><paramref name="disposer"/> is <see langword="null"/>.</para>
		/// </exception>
		public static IDisposable CreateDisposable(Action disposer)
		{
			ArgumentAssert.IsNotNull(disposer, "disposer");

			return new AnonymousDisposable(disposer);
		}

		private class AnonymousComparer<T> : IComparer<T>
		{
			private readonly Comparison<T> _comparison;

			public AnonymousComparer(Comparison<T> comparison)
			{
				_comparison = comparison;
			}

			public int Compare(T x, T y)
			{
				return _comparison(x, y);
			}
		}

		/// <summary>
		/// Creates an <see cref="IComparer{T}"/> implementation from the specified method.
		/// </summary>
		/// <typeparam name="T">The type to compare.</typeparam>
		/// <param name="comparison">The comparison.</param>
		/// <returns>An <see cref="IComparer{T}"/> implementation from the specified method.</returns>
		public static IComparer<T> CreateComparer<T>(Comparison<T> comparison)
		{
			if (comparison == null) throw new ArgumentNullException("comparison");

			return new AnonymousComparer<T>(comparison);
		}

		private class AnonymousEqualityComparer<T> : IEqualityComparer<T>
		{
			private readonly Func<T, T, bool> _equalityComparison;
			private readonly Func<T, int> _hashProvider;

			public AnonymousEqualityComparer(
				Func<T, T, bool> equalityComparison,
				Func<T, int> hashProvider)
			{
				_equalityComparison = equalityComparison;
				_hashProvider = hashProvider ?? EqualityComparer<T>.Default.GetHashCode;
			}

			public bool Equals(T x, T y)
			{
				return _equalityComparison(x, y);
			}

			public int GetHashCode(T obj)
			{
				return _hashProvider(obj);
			}
		}

		/// <summary>
		/// Creates an <see cref="IEqualityComparer{T}"/> implementation from the specified methods.
		/// </summary>
		/// <typeparam name="T">The type to compare.</typeparam>
		/// <param name="equalityComparison">The equality comparison.</param>
		/// <param name="hashProvider">The hash provider; or <see langword="null"/> to use the default hash provider.</param>
		/// <returns>An <see cref="IEqualityComparer{T}"/> implementation from the specified methods.</returns>
		public static IEqualityComparer<T> CreateEqualityComparer<T>(
			Func<T, T, bool> equalityComparison,
			Func<T, int> hashProvider)
		{
			if (equalityComparison == null) throw new ArgumentNullException("equalityComparison");

			return new AnonymousEqualityComparer<T>(equalityComparison, hashProvider);
		}

		/// <summary>
		/// Allow for selecting an object from an IEnumerable by specifying an index
		/// </summary>
		/// <typeparam name="T">object Type</typeparam>
		/// <param name="enumeration">A collection of items</param>
		/// <param name="index">index location of object to return</param>
		/// <returns></returns>
		public static T At<T>(IEnumerable<T> enumeration, int index)
		{
			if (enumeration == null)
				throw new ArgumentNullException("enumeration");
			var i = 0;
			foreach (T e in enumeration)
			{
				if (i == index)
					return e;
				i++;
			}
			throw new IndexOutOfRangeException(
				 String.Format("Index {0} does not exist in the enumeration", index));
		}



		/// <summary>
		/// Checks if item defined by predicate exists, iterates until and if the first item is met
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">A collection of items to check existence</param>
		/// <param name="predicate">A predicate to compute for all items</param>
		/// <returns>True iff any item defined by predicate exists</returns>
		public static bool Exists<T>(IEnumerable<T> a, Predicate<T> predicate)
		{
			return false == IsEmpty(FindAll(a, predicate));
		}

		/// <summary>
		/// Enumerates all provided arguments if any
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="args">list of items to put in a collection</param>
		/// <returns>A collection of supplied arguments or empty collection if none</returns>
		/// <example>
		/// Very useful instrument to avoid allocating memory and creating collections with few known elements
		/// Saves a lot of coding for unit tests as well
		/// 
		/// Instead of:
		///		List<int> yetAnotherList = new List<int>();
		///		yetAnotherList.Add(1);
		///		yetAnotherList.Add(100);
		///		foreach(int item in yetAnotherList)
		///		{
		///			...
		///		}
		/// 
		/// Write:
		/// 
		///		foreach(int item in Enumerate(1, 100)
		///		{
		///			...
		///		}
		/// 
		/// </example>
		/// 
		public static IEnumerable<T> Enumerate<T>(params T[] args)
		{
			if (args == null || args.Length == 0)
				yield break;

			foreach (T it in args)
				yield return it;
		}

		/// <summary>
		/// Concatenates a collection of collections into a single collection maintaining the order of collections as well as items in them
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="collections">a collection of collections to concatenate</param>
		/// <returns>A collections of all items starting from all items in first collection, second etc.</returns>
		public static IEnumerable<T> Concatenate<T>(IEnumerable<IEnumerable<T>> collections)
		{
			if (collections == null)
				yield break;

			foreach (IEnumerable<T> collection in collections)
			{
				if (collection == null)
					continue;

				foreach (T value in collection)
					yield return value;
			}
		}

		public static IEnumerable<T> Concatenate<T>(params IEnumerable<T>[] args)
		{
			return Concatenate(Enumerate(args));
		}

		public static void Copy<T>(IEnumerable<T> a, T[] args)
		{
			if (args == null || args.Length == 0)
				return;

			if (a == null)
				a = Enumerate<T>();

			IEnumerator<T> it = a.GetEnumerator();
			for (int i = 0; i < args.Length; ++i)
			{
				args[i] = it.MoveNext() == false ? default(T) : it.Current;
			}
		}

		/// <summary>
		/// Enumerates all and only items from given collection that satisfy criteria
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">A collection to filter</param>
		/// <param name="predicate">A condition to evaluate for each item</param>
		/// <returns>All and only items in the same order as original collection that satisy condition</returns>
		public static IEnumerable<T> FindAll<T>(IEnumerable<T> a, Predicate<T> predicate)
		{
			foreach (T value in a)
			{
				if (predicate(value))
					yield return value;
			}
		}

		/// <summary>
		/// Enumerates all and returns the object with the maximum value of the integer property specified by the MaxFunction delegate.
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">A collection to find the max for a property</param>
		/// <param name="MaxFunction">A function returning an int value given an object</param>
		/// <returns>The item in the collection with the max value returned by MaxFunction</returns>
		public static T MaxItem<T>(IEnumerable<T> a, MaxFunction<T> func)
		{
			T maxValue = default(T);
			if (a == null) return maxValue;
			if (func == null) return maxValue;
			foreach (T value in a)
			{
				if (maxValue == null)
				{
					maxValue = value;
					continue;
				}
				if (func(value) > func(maxValue))
					maxValue = value;
			}
			return maxValue;
		}

		public delegate int MaxFunction<T>(T source);

		public static IDictionary<TKey, IEnumerable<TValue>> GroupByDistinctKey<TKey, TValue>(
			IEnumerable<TValue> a, Converter<TValue, TKey> converter)
			where TKey : IComparable<TKey>
		{
			Dictionary<TKey, IEnumerable<TValue>> distinct = new Dictionary<TKey, IEnumerable<TValue>>();
			if (a == null)
				return distinct;
			foreach (TValue value in a)
			{
				TKey key = converter(value);
				if (distinct.ContainsKey(key))
					continue;
				distinct.Add(key, FindAll(a, v => converter(v).CompareTo(key) == 0));
			}
			return distinct;
		}

		/// <summary>
		/// Applies action for each item in a collection
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">Collection supplying action items</param>
		/// <param name="action">An action to perform on each item</param>
		public static void ForEach<T>(IEnumerable<T> a, Action<T> action)
		{
			foreach (T value in a)
				action(value);
		}

		/// <summary>
		/// Converts all items of input collection according to conversion rule from one type to another
		/// </summary>
		/// <typeparam name="TInput">Input item type</typeparam>
		/// <typeparam name="TOutput">Output item type</typeparam>
		/// <param name="a">A collection of items to convert</param>
		/// <param name="converter">A method to convert items from one type to another</param>
		/// <returns>All items from input collection converted to output type in the same order</returns>
		public static IEnumerable<TOutput> ConvertAll<TInput, TOutput>(IEnumerable<TInput> a,
			Converter<TInput, TOutput> converter)
		{
			foreach (TInput value in a)
				yield return converter(value);
		}

		/// <summary>
		/// Convert collection of items to a string where items are separeted by given character
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="values">A values to be converted into string</param>
		/// <param name="separator">A character used to separate items in output string</param>
		/// <returns>A string with all items are converted to strings and separated by separator character</returns>
		public static string ToCSV<T>(IEnumerable<T> values, char separator)
		{
			if (values == null)
				return string.Empty;

			StringBuilder sb = new StringBuilder();
			IEnumerator<T> it = values.GetEnumerator();
			if (it.MoveNext())
			{
				sb.Append(it.Current);
				while (it.MoveNext())
				{
					sb.Append(separator).Append(it.Current);
				}
			}
			return sb.ToString();
		}

		public static string ToHex(IEnumerable<byte> values)
		{
			if (values == null)
				return string.Empty;
			return values.ToHex();
		}

		/// <summary>
		/// Gets the given stream as a string represented by 3 character long bytes delimited by spaces
		/// (e.g. 245 002 021 254)
		/// </summary>
		/// <param name="stream">The stream to read from, if not a position 0 will seek to position 0; can be null.</param>
		/// <param name="maxCount">The maximum number of bytes to read from the stream.</param>
		/// <param name="indicateTruncate">If <see langword="true"/> the string contain text indicating that the data is tructated and how much more data was available.</param>
		/// <returns>A string representation of the bytes in the stream.</returns>
		public static string ToByteString(Stream stream, int maxCount, bool indicateTruncate)
		{
			if (stream == null) return string.Empty;

			if (stream.CanSeek || stream.Position == 0)
			{
				if (stream.Position != 0)
				{
					stream.Seek(0, SeekOrigin.Begin);
				}

				byte[] buffer = SafeMemoryAllocator.CreateArray<byte>(maxCount);

				int count = maxCount;
				int offset = 0;
				do
				{
					int read = stream.Read(buffer, offset, count);
					if (read == 0)
					{
						break;
					}
					offset += read;
					count -= read;
				}
				while (count > 0);

				int totalRead = offset;

				//big enough for 3 chars per byte and 1 char space per byte
				StringBuilder sb = new StringBuilder(totalRead * 4);

				for (int i = 0; i < totalRead; i++)
				{
					if (i > 0) sb.Append(" ");
					//the manual formatting is twice as fast as AppendFormat("{0:000}");
					//0.50 seconds manual formatting on 2^20 items vs 1.04 for AppendFormat
					string str = buffer[i].ToString();
					if (str.Length == 1) sb.Append("00");
					else if (str.Length == 2) sb.Append("0");
					sb.Append(str);
				}

				if (indicateTruncate)
				{
					if (stream.Length > maxCount)
					{
						sb.AppendFormat(" Data truncated, {0} more bytes were available", stream.Length - maxCount);
					}
				}

				return sb.ToString();
			}
			else
			{
				return "Stream can't seek";
			}
		}

		/// <summary>
		/// Converts a string containing separated by separator character values into a collections of those items
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="values">String of separated values</param>
		/// <param name="separator">Separator character</param>
		/// <param name="converter">A method to convert a string into an item</param>
		/// <returns>Collection of items separated by given character in input string</returns>
		public static IEnumerable<T> FromCSV<T>(string values, char separator, Converter<string, T> converter)
		{
			string[] stringValues = values.Split(separator);
			foreach (string candidate in stringValues)
				yield return converter(candidate);
		}

		#endregion

		#region miscellaneous operations

		/// <summary>
		/// Joins <paramref name="keys"/> and <paramref name="values"/> into a set of <see cref="KeyValuePair{TKey,TValue}"/> instances.
		/// </summary>
		/// <typeparam name="TKey">The type of elements contained by the <paramref name="keys"/> collection.</typeparam>
		/// <typeparam name="TValue">The type of elements contained by the <paramref name="values"/> collection.</typeparam>
		/// <param name="keys">The set of keys.</param>
		/// <param name="values">The set of values.</param>
		/// <returns>
		///		<para>A set of <see cref="KeyValuePair{TKey,TValue}"/> where they key property
		///		comes from the elements in <paramref name="keys"/> and the value property
		///		comes from the elements in <paramref name="values"/>.</para>
		/// </returns>
		/// <exception cref="ArgumentNullException">
		///		<para><paramref name="keys"/> is <see langword="null"/>.</para>
		///		<para>- or -</para>
		///		<para><paramref name="values"/> is <see langword="null"/>.</para>
		/// </exception>
		/// <exception cref="ArgumentException">
		///		<para><paramref name="keys"/> has more elements than <paramref name="values"/>.</para>
		///		<para>- or -</para>
		///		<para><paramref name="values"/> has more elements than <paramref name="keys"/>.</para>
		/// </exception>
		public static IEnumerable<KeyValuePair<TKey, TValue>> JoinPairs<TKey, TValue>(
			IEnumerable<TKey> keys,
			IEnumerable<TValue> values)
		{
			using (var keyEnumerator = keys.GetEnumerator())
			using (var valueEnumerator = values.GetEnumerator())
			{
				var keyMoved = keyEnumerator.MoveNext();
				var valueMoved = valueEnumerator.MoveNext();

				if (keyMoved && valueMoved)
				{
					yield return new KeyValuePair<TKey, TValue>(keyEnumerator.Current, valueEnumerator.Current);
				}
				else if (keyMoved)
				{
					throw new ArgumentException("The 'keys' collection has more items than the 'values' collection");
				}
				else if (valueMoved)
				{
					throw new ArgumentException("The 'values' collection has more items than the 'keys' collection");
				}
			}
		}

		/// <summary>
		/// Checks if collection contains single element and returns first element
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="a">A collection to be tested</param>
		/// <param name="value">Value of a first element regardless if collection is a singleton, default(T) if it is empty or null</param>
		/// <returns>True iff collection contains a single element</returns>
		public static bool IsSingleton<T>(IEnumerable<T> a, out T value)
		{
			return Membership(a, out value) != CollectionMembership.Multiple;
		}

		public enum CollectionMembership
		{
			Empty,
			Single,
			Multiple
		};

		public static CollectionMembership Membership<T>(IEnumerable<T> a, out T value)
		{
			value = default(T);
			if (a == null)
				return CollectionMembership.Empty;
			IEnumerator<T> it = a.GetEnumerator();
			if (false == it.MoveNext())
				return CollectionMembership.Empty;
			value = it.Current;
			return it.MoveNext() ? CollectionMembership.Multiple : CollectionMembership.Single;

		}

		/// <summary>
		/// Exchange two values if condition is satisfied
		/// </summary>
		/// <typeparam name="T">Type of a value to exchange</typeparam>
		/// <param name="condition">Condition when to exchange values</param>
		/// <param name="a">First value</param>
		/// <param name="b">Second value</param>
		public static void ExchangeIf<T>(bool condition, ref T a, ref T b)
		{
			if (condition)
			{
				T value = b;
				b = a;
				a = value;
			}
		}

		/// <summary>
		/// Computes hash code that is reasonably good for most applications
		/// NOTE: although it is somewhat more expensive to compute than traditional "simple" hashes it
		/// gives 2 orders of magniture lower collision rates which should payback as long as number of items to hash is non-trivial
		/// WARNING: a single character code change may cause dramatic increase in % of collisions 
		/// </summary>
		/// <typeparam name="T">Item type</typeparam>
		/// <param name="values">Collection of items to hash</param>
		/// <returns>hash code of a collection</returns>
		public static int GetHashCode<T>(IEnumerable<T> values)
		{
			if (values == null)
				return -1;

			IEnumerator<T> it = values.GetEnumerator();
			if (false == it.MoveNext())
				return 0;

			int h = it.Current.GetHashCode();
			int hash = H2S(h, h);

			while (it.MoveNext())
			{
				hash = H2S(hash, it.Current.GetHashCode());
			}
			return H2S(hash, h);
		}

		public static int GetConsistentIndex(IEnumerable<int> keys, int first, int last)
		{
			if (keys == null)
				return -1;

			if (first > last)
				return -1;

			if (first == last)
				return first;

			int hash = GetHashCode(keys);
			double s = hash > 0 ? (double)hash / int.MaxValue : hash < 0 ? (double)hash / int.MinValue : 0.0;
			double value = Math.Floor(s * (last - first + 1));
			return first + (int)value;
		}

		private class StrictlyGreaterComparer<T> : Comparer<T>
		{
			public StrictlyGreaterComparer(IComparer<T> comparer)
			{
				Comparer = comparer;
			}

			#region IComparer<T> Members

			public override int Compare(T x, T y)
			{
				return Comparer.Compare(x, y) + 1;
			}

			public IComparer<T> Comparer { get; set; }

			#endregion
		}

		public class KeyValuePairComparer<K, V> : Comparer<KeyValuePair<K, V>>
			where K : IComparable<K>
			where V : IComparable<V>
		{
			#region IComparer<KeyValuePair<K,V>> Members

			public override int Compare(KeyValuePair<K, V> x, KeyValuePair<K, V> y)
			{
				int result = x.Key.CompareTo(y.Key);
				return result != 0 ? result : x.Value.CompareTo(y.Value);
			}

			#endregion
		}

		public class CustomComparer<T> : Comparer<T>
		{
			public CustomComparer(Comparison<T> comp)
			{
				Comparison = comp;
			}

			public Comparison<T> Comparison { get; set; }

			public override int Compare(T x, T y)
			{
				return Comparison(x, y);
			}
		}

		public delegate T Factory<T>();

		/// <summary>
		///	<para>A lazily initialized indexer. You provide a factory method that will be called to
		///	construct values for keys that have not yet been encountered.
		///	This is a thread-safe pattern.</para>
		/// </summary>
		/// <typeparam name="TKey">The type indexer argument.</typeparam>
		/// <typeparam name="TValue">The type values stored in the indexer.</typeparam>
		/// <param name="valueFactory">
		///	<para>A factory method for creating values given a key. This method will
		///	only be called once for each key.</para>
		/// </param>
		/// <returns>
		///	<para>A lazily initialized <typeparamref name="TValue"/> given a <typeparamref name="TKey"/>.</para>
		/// </returns>
		/// <exception cref="ArgumentNullException">
		///	<para><paramref name="valueFactory"/> is <see langword="null"/>.</para>
		/// </exception>
		public static Factory<TKey, TValue> LazyIndexer<TKey, TValue>(Factory<TKey, TValue> valueFactory)
		{
			return LazyIndexer<TKey, TValue>(valueFactory, null);
		}

		/// <summary>
		///	<para>A lazily initialized indexer. You provide a factory method that will be called to 
		///	construct values for keys that have not yet been encountered. You also provide
		///	an <see cref="IEqualityComparer{TKey}"/> implementation that will be used to compare keys.
		///	This is a thread-safe pattern.</para>
		/// </summary>
		/// <typeparam name="TKey">The type indexer argument.</typeparam>
		/// <typeparam name="TValue">The type values stored in the indexer.</typeparam>
		/// <param name="valueFactory">
		///	<para>A factory method for creating values given a key. This method will
		///	only be called once for each key.</para>
		/// </param>
		/// <param name="comparer">
		///	<para>The <see cref="T:System.Collections.Generic.IEqualityComparer`1" /> implementation to use when comparing keys,
		///	or <see langword="null"/> to use the default <see cref="T:System.Collections.Generic.EqualityComparer`1" /> for the type of the key.</para>
		/// </param>
		/// <returns>
		///	<para>A lazily initialized <typeparamref name="TValue"/> given a <typeparamref name="TKey"/>.</para>
		/// </returns>
		/// <exception cref="ArgumentNullException">
		///	<para><paramref name="valueFactory"/> is <see langword="null"/>.</para>
		/// </exception>
		public static Factory<TKey, TValue> LazyIndexer<TKey, TValue>(Factory<TKey, TValue> valueFactory, IEqualityComparer<TKey> comparer)
		{
			if (valueFactory == null) throw new ArgumentNullException("valueFactory");

			var syncRoot = new object();
			var dictionary = new Dictionary<TKey, TValue>(comparer);

			return key =>
			{
				TValue result;
				if (dictionary.TryGetValue(key, out result)) return result;
				lock (syncRoot)
				{
					if (dictionary.TryGetValue(key, out result)) return result;

					var newValue = valueFactory(key);
					var newDictionary = new Dictionary<TKey, TValue>(dictionary.Count + 1, comparer);
					foreach (var pair in dictionary)
					{
						newDictionary.Add(pair.Key, pair.Value);
					}
					newDictionary.Add(key, newValue);
					Thread.MemoryBarrier();
					dictionary = newDictionary;
					return newValue;
				}
			};
		}

		/// <summary>
		///	<para>A readable lazily initialized indexer. You provide a factory method that will be called to 
		///	construct values for keys that have not yet been encountered. You also provide
		///	an <see cref="IEqualityComparer{TKey}"/> implementation that will be used to compare keys.
		///	This is a thread-safe pattern.</para>
		/// </summary>
		/// <typeparam name="TKey">The type indexer argument.</typeparam>
		/// <typeparam name="TValue">The type values stored in the indexer.</typeparam>
		/// <param name="valueFactory">
		///	<para>A factory method for creating values given a key. This method will
		///	only be called once for each key.</para>
		/// </param>
		/// <param name="comparer">
		///	<para>The <see cref="T:System.Collections.Generic.IEqualityComparer`1" /> implementation to use when comparing keys,
		///	or <see langword="null"/> to use the default <see cref="T:System.Collections.Generic.EqualityComparer`1" /> for the type of the key.</para>
		/// </param>
		/// <returns>
		///	<para>A lazily initialized <typeparamref name="TValue"/> given a <typeparamref name="TKey"/>.</para>
		/// </returns>
		/// <exception cref="ArgumentNullException">
		///	<para><paramref name="valueFactory"/> is <see langword="null"/>.</para>
		/// </exception>
		public static ReadableLazyIndexer<TKey, TValue> LazyIndexerReadable<TKey, TValue>(Factory<TKey, TValue> valueFactory, IEqualityComparer<TKey> comparer)
		{
			return new ReadableLazyIndexer<TKey, TValue>(valueFactory, comparer);
		}

		public static Factory<T> OnceProperty<T>(Factory<T> factory) where T : class
		{
			T value = default(T);
			return () => value ?? (value = factory());
		}

		public static PeriodicFactory<T> PeriodicProperty<T>(Factory<T> factory) where T : class
		{
			Stopwatch watch = new Stopwatch();
			watch.Start();

			T value = factory();
			return duration =>
						{
							if (watch.Elapsed < duration)
								return value;

							watch.Reset();
							watch.Start();
							return value = factory();
						};
		}

		/// <summary>
		///	<para>Creates a method that, if called repeatedly, will run no more than once every
		///	<paramref name="minInterval"/> amount of time.</para>
		/// </summary>
		/// <param name="action">
		///	<para>The action to execute.</para>
		/// </param>
		/// <param name="minInterval">
		///	<para>The minimum time that must elapse before the next method can run.</para>
		/// </param>
		/// <returns>
		/// A method that will only run if it hasn't already run within <paramref name="minInterval"/>.
		/// </returns>
		public static ParameterlessDelegate FrequencyBoundMethod(ParameterlessDelegate action, TimeSpan minInterval)
		{
			long lastRan = 0;

			return () =>
			{
				var now = DateTime.UtcNow.Ticks;
				var limit = Interlocked.Read(ref lastRan) + minInterval.Ticks;
				if(now >= limit)
				{
					var oldTicks = Interlocked.Exchange(ref lastRan, now);
					if (oldTicks < limit)
					{
						action();
					}
				}
			};
		}

		/// <summary>
		///	<para>Creates a method that, if called repeatedly, will run no more than once every
		///	<paramref name="minInterval"/> amount of time and will provide the number of times the method
		///	was suppressed between calls.</para>
		/// </summary>
		/// <param name="action">
		///	<para>The action to execute. The value passed to the method is the number of times this method was suppressed within
		///	the given <paramref name="minInterval"/>.</para>
		/// </param>
		/// <param name="minInterval">
		///	<para>The minimum time that must elapse before the next method can run.</para>
		/// </param>
		/// <returns>
		/// A method that will only run if it hasn't already run within <paramref name="minInterval"/>.
		/// </returns>
		public static ParameterlessDelegate FrequencyBoundMethod(Action<long> action, TimeSpan minInterval)
		{
			long lastRan = 0;
			long callsSinceLastRan = 0;
			return () =>
			{
				var now = DateTime.UtcNow.Ticks;
				var limit = Interlocked.Read(ref lastRan) + minInterval.Ticks;
				if (now >= limit)
				{
					var oldTicks = Interlocked.Exchange(ref lastRan, now);
					if (oldTicks < limit)
					{
						long calls = Interlocked.Exchange(ref callsSinceLastRan, 0);
						action(calls);
						return;
					}
				}
				Interlocked.Increment(ref callsSinceLastRan);
			};
		}

		#endregion

		#region cross product

		public delegate Z BinaryConverter<X, Y, Z>(X x, Y y);

		public static IEnumerable<TValue> CrossProduct<TLeft, TRight, TValue>(
			IEnumerable<TLeft> a, IEnumerable<TRight> b, BinaryConverter<TLeft, TRight, TValue> converter)
		{
			foreach (TLeft left in a)
			{
				foreach (TRight right in b)
					yield return converter(left, right);
			}
		}

		public static IEnumerable<TValue> CrossProduct<TLeft, TRight, TValue>(
			TLeft left, IEnumerable<TRight> b, BinaryConverter<TLeft, TRight, TValue> converter)
		{
			foreach (TRight right in b)
				yield return converter(left, right);
		}

		public static IEnumerable<TValue> CrossProduct<TLeft, TRight, TValue>(
			IEnumerable<TLeft> a, TRight right, BinaryConverter<TLeft, TRight, TValue> converter)
		{
			foreach (TLeft left in a)
				yield return converter(left, right);
		}

		#endregion

		#region implementation

		private static int H2S(int hash, int value)
		{
			unchecked
			{
				return ((hash << 5) ^ (hash >> 27)) ^ ~value + ((value << 27) - (value >> 5)) ^ hash;
			}
		}

		private static IEnumerable<int> RandomSubsetUsingArray(Random generator, int first, int last, int subsetSize)
		{
			if (first > last || subsetSize <= 0)
				yield break;

			if (first == last)
			{
				yield return first;
				yield break;
			}

			int size = last - first + 1;

			//
			// this is "classical" implementation of random permutation algorithm
			// it is the fastest possible version but it requires that size memory is allocated
			// hence it works the best when subsetSize < or ~= last - first + 1
			// 
			int[] result = new int[size];
			for (int t = first; t <= last; ++t)
				result[t - first] = t;

			foreach (int t in ReverseSequence(last, first))
			{
				int k = generator.Next(first, t + 1);
				yield return result[k - first];

				if (last - t + 1 >= subsetSize)
					break;

				result[k - first] = result[t - first];
			}
		}

		private static IEnumerable<int> RandomSubsetUsingDictionary(Random generator, int first, int last, int subsetSize)
		{
			if (first > last || subsetSize <= 0)
				yield break;

			if (first == last)
				yield return first;

			//
			// algorithm is based on the fact that we need at most subsetSize substitutions
			// each substitution is composed of a slot index and value that we put in there
			// this algorithm is the best when last - first + 1 >> subsetSize
			//
			Dictionary<int, int> result = new Dictionary<int, int>(subsetSize);
			foreach (int t in ReverseSequence(last, first))
			{
				int k = generator.Next(first, t + 1);

				int value;
				if (false == result.TryGetValue(k, out value))
					value = k;
				yield return value;

				if (last - t + 1 >= subsetSize)
					break;

				if (false == result.TryGetValue(t, out value))
					value = t;
				result[k] = value;
			}
		}

		private static void FindListIndexUsingBookmark(int[] listCounts, int itemIndex,
			ref int listIndex, ref int firstItemIndex)
		{
			//
			// average number of scans for perfectly uniform random sequence of {itemIndex} is [(lists.Count + 3)/4]
			// for example for lists.Count = 10 we have 3 scans
			//
			if (firstItemIndex > itemIndex)
				listIndex = firstItemIndex = 0;

			while (firstItemIndex + listCounts[listIndex] <= itemIndex)
				firstItemIndex += listCounts[listIndex++];
		}

		#endregion

		#region hash

		public const uint DefaultMurmer64HashSeed = 564328714;

		public unsafe static ulong GetMurmur64AHash(byte[] value, uint seed = DefaultMurmer64HashSeed)
		{
			ArgumentAssert.IsNotNull(value, "value");

			const ulong m = 0xc6a4a7935bd1e995;
			const int r = 47;
			var byteLength = value.Length;

			ulong h = seed ^ ((ulong)byteLength * m);

			fixed (byte* bytes = value)
			{
				ulong* data = (ulong*)bytes;
				ulong* end = data + (byteLength / 8);

				while (data != end)
				{
					ulong k = *data++;

					k *= m;
					k ^= k >> r;
					k *= m;

					h ^= k;
					h *= m;
				}

				byte* data2 = (byte*)data;

				for (int i = byteLength & 7 - 1; i >= 0; --i)
				{
					h ^= (ulong)data2[i] << (8 * i);
				}
				h *= m;

				h ^= h >> r;
				h *= m;
				h ^= h >> r;
			}
			return h;
		}

		#endregion
	}

	public struct APair<TLeft, TRight> : IComparable<APair<TLeft, TRight>>, IEquatable<APair<TLeft, TRight>>
		where TLeft : IComparable<TLeft>
		where TRight : IComparable<TRight>
	{
		public TLeft Left;
		public TRight Right;

		public APair(TLeft left, TRight right)
		{
			Left = left;
			Right = right;
		}

		#region IComparable<APair<TLeft, TRight>> Members

		public int CompareTo(APair<TLeft, TRight> other)
		{
			int result = Left.CompareTo(other.Left);
			return result != 0 ? result : Right.CompareTo(other.Right);
		}

		#endregion

		#region IEquatable<APair<TLeft, TRight>> Members

		public bool Equals(APair<TLeft, TRight> other)
		{
			return CompareTo(other) == 0;
		}

		#endregion

		#region Object members

		public override string ToString()
		{
			return string.Format("({0}, {1})", Left, Right);
		}

		public override bool Equals(object obj)
		{
			return CompareTo((APair<TLeft, TRight>)obj) == 0;
		}

		public override int GetHashCode()
		{
			return (Left.GetHashCode() << 5) ^ Right.GetHashCode();
		}

		#endregion
	}
}

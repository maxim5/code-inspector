using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;

namespace classifier
{
	/// <summary>
	/// Created a Hash Set class (with count) that:
	/// 1. Automaticaly created a new entry with a new occurence of a key
	/// 2. Increments an existing entry if the key already exists
	/// </summary>
    internal class Set<T> : ICollection<T>
    {
        private Dictionary<T, int> d;
        public Set() { d = new Dictionary<T, int>(); }
        public Set(int capacity) { d = new Dictionary<T, int>(capacity); }
        public Set(IEnumerable<T> col) : this() { this.AddRange(col); }
		
		/// <summary>
		/// Allows the Hash Set to be accessed as a Dictionary.
		/// </summary>
        internal IDictionary<T, int> D { get { return d; } }
        public int Count { get { return d.Count; } }
        public int this[T key] { get { return d[key]; } }

        private int totalWordCount = 0;
		/// <summary>
		/// This value is kept as a complete summation of all words
		/// defined within this set.
		/// </summary>
        public int TotalWordCount 
        { 
            get { return totalWordCount; }
            set { totalWordCount = value; }
        }
		
		/// <summary>
		/// Adds an item to the Hash Set.
		/// If the item does not exist, create a new entry.
		/// If the item exists, increment the current count.
		/// </summary>
        public virtual void Add(T item) 
        {
            if (!d.ContainsKey(item)) { d.Add(item, 1); }
            else { d[item]++; }
        }
		
		/// <summary>
		/// Adds a collection of items using the defined Set Add method.
		/// </summary>
        public void AddRange(IEnumerable<T> col) { foreach (T item in col) { this.Add(item); } }
        public bool Contains(T item) { return d.ContainsKey(item); }
		
		/// <summary>
		/// An item is completely removed from the Hash Set, regardless of count.
		/// </summary>
        public bool Remove(T item) { return d.Remove(item); }
        public void Clear() { d.Clear(); }

        public void CopyTo(T[] array, int arrayIndex) { d.Keys.CopyTo(array, arrayIndex); }
        public bool IsReadOnly { get { throw new NotImplementedException(); } }
        public IEnumerator<T> GetEnumerator() { return d.Keys.GetEnumerator(); ; }
        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() { return d.Keys.GetEnumerator(); }
		
		/// <summary>
		/// Converts the Hash Set to an array of the keys for the Hash.
		/// </summary>
        public T[] ToArray()
        {
            List<T> result = new List<T>(d.Keys);
            return result.ToArray();
        }
    }
	
	/// <summary>
	/// Main class for the Naive Bayes classifier. 
	/// </summary>
    internal class Algorithm
    {
        private const double BASE = 2.0d;
        private const string REGEX_WORDS = @"([a-zA-Z]+)";
        private static readonly Regex re = new Regex(REGEX_WORDS, RegexOptions.Compiled);
		
		/// <summary>
		/// The vocabulary is a complete "dictionary" collection of all words that have been seen.
		/// </summary>
        internal Set<string> vocabulary = new Set<string>();
        internal Dictionary<string, Set<string>> docs= new Dictionary<string, Set<string>>();
        internal Dictionary<string, double> prior = new Dictionary<string, double>();
        internal Dictionary<string, Dictionary<string, double>> likelihood = new Dictionary<string, Dictionary<string, double>>();
        internal Dictionary<string, string> classification = new Dictionary<string, string>();
		
		/// <summary>
		/// This is the training step of the Naive Bayes Classifier.
		/// The training data is provided as a collection of documents.
		/// The documents are parsed and the words are associated with the specific label for the document.
		/// The parsed words are also added to the overall vocabulary of this classifier.
		/// </summary>
		public void LearnNaiveBayesText(Dictionary<string, string> examples, string[] labels) 
        {
			Dictionary<string, int> match = new Dictionary<string, int>(labels.Length);
			
			// Initialize the learner's training variables
            foreach (string label in labels) 
            {
                match.Add(label, 0);
                docs.Add(label, new Set<string>());
                likelihood.Add(label, new Dictionary<string, double>());
            }

            // have the examples split, got vocab, |doc_j|, and text for each v{j}
            double exampleCount = (double)examples.Count;

            ICollection<string> words;
			foreach(KeyValuePair<string, string> entry in examples) 
            {
				// First, grab all of the words in this document.
                words = GetWords(entry.Value);
				// Add the word-bag to the vocabulary (which identifies the count)
                vocabulary.AddRange(words);
				// Find the right label for the given document
                foreach (string key in labels)
                {
					// because the key here is a filename that contains the label, we do a string contains check
                    if (entry.Key.Contains(key)) 
                    { 
						// increment the match for this label
                        match[key]++;
						// add all of the identified words for this particular label
                        docs[key].AddRange(words);
						// update the total word count
                        docs[key].TotalWordCount += words.Count;
                    }
                    //if (j++ == 0) File.WriteAllLines(key + ".output.txt", Convert<string, int>(docs[key].D));
                }
			}

            // remove words as a process of feature selection.
            Remove(0, labels);

            double pr;
            foreach (string key in labels)
            {
                // generate the priors, which is simply a count of documents for a particular label, over all documents
                prior[key] = Math.Abs(Math.Log(match[key] / exampleCount, BASE));
                foreach (string word in docs[key])
                {
					// generate the log likelihood probability for each word (given a label)
                    pr = CalcLikelihoodWithQ(docs[key][word], docs[key].TotalWordCount, word);
                    likelihood[key].Add(word, pr);
                }
            }

            //Console.WriteLine(vocabulary.Count);
            //File.WriteAllLines("politics.txt", ToStringArray<string, int>(docs["politics"].D));
            //File.WriteAllLines("medical.txt", ToStringArray<string, int>(docs["medical"].D));
            //File.WriteAllLines("priors.txt", ToStringArray<string, double>(prior));
            //File.WriteAllLines("vocab.txt", ToStringArray<string, int>(vocabulary.D));
        }
		
		/// <summary>
		/// This is a process of feature selection, where a p count of words are removed from the vocabulary.
		/// The vocabulary as well as the associated label-specific counters are updated to reflect this change.
		/// </summary>
        private void Remove(int p, string[] labels)
        {
            List<KeyValuePair<string, int>> list = new List<KeyValuePair<string, int>>(vocabulary.D);
            list.Sort(delegate(KeyValuePair<string, int> e1, KeyValuePair<string, int> e2)
                { return e1.Value.CompareTo(e2.Value); });
            list.Reverse();
            for (int i = 0; i < p; i++)
            {
                vocabulary.Remove(list[i].Key);
                foreach (string key in labels)
                {
                    docs[key].Remove(list[i].Key);
                }
            }
        }
		
		/// <summary>
		/// This is the classification step of the algorithm.
		/// 
		/// </summary>
        public void Classify(Dictionary<string, string> tests, string[] labels)
        {
            Dictionary<string, double> heap;
            ICollection<string> positions;
            List<double> sorter;

            foreach (KeyValuePair<string, string> doc in tests)
            {
				// here, we only collect the words that already exist in our vocabulary
                positions = GetWords(doc.Value, true);
                heap = new Dictionary<string, double>();
                foreach (string key in labels)
                {
					/*
					 * each iteration performs a likelihood calculation of all words in a document
					 * with respect to some label. This is then stored on a conceptual "heap"
					 * which will be checked to identify the most likelihood labeling.
					 */
                    double v = prior[key];
                    foreach (string ai in positions) 
                        v += GetLogLikelihood(key, ai);
                    // add into heap
                    heap.Add(key, v);
                }
                // TODO: find a better sort
                sorter = new List<double>(heap.Values);
                sorter.Sort();
                foreach (KeyValuePair<string, double> entry in heap)
                {
                    if (entry.Value == sorter[0])
                        classification.Add(doc.Key, entry.Key);
                }
                //File.WriteAllLines("positions.txt", ToStringArray<string, int>(positions.D));
            }
            //File.WriteAllLines("classification.txt", ToStringArray<string, string>(classification));
        }
		
		/// <summary>
		/// Gets the log likelihood.
		/// </summary>
        private double GetLogLikelihood(string key, string ai)
        {
            // get my n, count duplicate words multiple times
            double result;
            if (likelihood[key].TryGetValue(ai, out result))
                return result;

            // should only be here if I don't have the word in my docs
            result = CalcLikelihoodWithQ(0, docs[key].TotalWordCount, ai);
            likelihood[key][ai] = result;
            return result;
        }
		
		/// <summary>
		/// Performs a simple calculation of the likelihood.
		/// This variation issues some offset to the count of words to help smooth out the cases
		/// 	where a word may be encountered for the first time for a particular label.
		/// </summary>
        private double CalcLikelihood(int nk, int n)
        {
            //return Math.Abs(Math.Log(((double)(nk + 1d) / (n + vocabulary.Count)), BASE));
            int m = 10;
            double result = Math.Abs(Math.Log(
                                (nk + ((double)m / vocabulary.Count))
                                / (n + m)
                            , BASE));
            return result;
        }
		
		/// <summary>
		/// Here, a non-uniform calculation for the likelihood is used.
		/// The difference here is that a word that shows up more frequently in any category
		/// 	will be given more weight than a random word (for a particular occurence in a document).
		/// </summary>
        private double CalcLikelihoodWithQ(int nk, int n, string w)
        {
            int m = vocabulary.Count;
            //double q = 1d/vocabulary.Count;
            double q = 0d;
            int num = 0, denom = 0;

            foreach (Set<string> doc in docs.Values)
            {
                num += doc.Contains(w) ? doc[w] : 0;
                denom += doc.TotalWordCount;
            }
            q = ((double)(num + 1) / (denom + vocabulary.Count));
            double result = Math.Abs(Math.Log(
                                ((double)n / (n + m)) * ((double)nk / n) 
                                + ((double)m / (n + m)) * q
                            , BASE));
            return result;
        }
		
		/// <summary>
		/// Gets the words from the text without checking to see if the word currently exists in the vocabulary.
		/// </summary>
        private ICollection<string> GetWords(string text) { return GetWords(text, false); }
		
		/// <summary>
		/// Gets the word from the text.
		/// Uses a customizable regex to figure out how to identify a "word"
		/// Additional filters can also be added in through additional overloads as needed.
		/// (e.g. a check to see if the split words exist in the current vocabulary)
		/// </summary>
        private ICollection<string> GetWords(string text, bool check)
        {
            string filter = text.ToLower();
            List<string> words = new List<string>();
            foreach(Match m in re.Matches(filter)) 
            {
                // additional filters
                if (m.Value.Length > 2 
                    &&(!check || (check && vocabulary.Contains(m.Value))))
                {
                    words.Add(m.Value);
                }
            }
            return words;
        }
		
		/// <summary>
		/// Outputs the results to a string array.
		/// </summary>
        private static string[] ToStringArray<K, V>(IDictionary<K, V> dict)
        {
            string[] result = new string[dict.Count];
            int i = 0;
            foreach (KeyValuePair<K, V> entry in dict)
                result[i++] = String.Format("{0}:{1}", entry.Key, entry.Value);
            return result;
        }
    }
}


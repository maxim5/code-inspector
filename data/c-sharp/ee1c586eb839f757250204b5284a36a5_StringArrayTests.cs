using System;
using NUnit.Framework;

namespace DNAEval
{
    [TestFixture]
    public class StringArrayTests
    {
        private static readonly string[] TEXT = new []{
           "In computer science, the Boyer–Moore string search algorithm is a particularly efficient string searching algorithm, and it has been the standard benchmark for the practical string search literature.[1] It was developed by Bob Boyer and J Strother Mo",
           "ore in 1977. The algorithm preprocesses the target string (key) that is being se",
           "arched for, but not the string being searched in (unlike some algorithms that preprocess the string to be searched and can then amortize the expense of the preprocessing by searching repeatedly). The execution time of the Boyer-Moore algorithm, while still linear in the size of the string being searched, can have a significantly lower constant factor than many ot", 
           "her search algorithms: it doesn't need to check every character of the stri",
           "ng to be searched, but rather skips over some of them. Generally the algorithm gets faster as the key being searched for becomes longer. Its effic",
           "iency derives from the fact that with each unsuccessful attempt to find a match between the search string and the text it is searching, it uses the information gained from that attempt to rule out as many positions of the text as po",
           "ssible where the string cannot match.",
        "In computer science, the Boyer–Moore string search algorithm is a particularly efficient string searching algorithm, and it has been the standard benchmark for the practical string search literature.[1] It was developed by Bob Boyer and J Strother Mo",
           "ore in 1977. The algorithm preprocesses the target string (key) that is being se",
           "arched for, but not the string being searched in (unlike some algorithms that preprocess the string to be searched and can then amortize the expense of the preprocessing by searching repeatedly). The execution time of the Boyer-Moore algorithm, while still linear in the size of the string being searched, can have a significantly lower constant factor than many ot", 
           "her search algorithms: it doesn't need to check every character of the stri",
           "ng to be searched, but rather skips over some of them. Generally the algorithm gets faster as the key being searched for becomes longer. Its effic",
           "iency derives from the fact that with each unsuccessful attempt to find a match between the search string and the text it is searching, it uses the information gained from that attempt to rule out as many positions of the text as po",
           "ssible where the string cannot match.",
        "In computer science, the Boyer–Moore string search algorithm is a particularly efficient string searching algorithm, and it has been the standard benchmark for the practical string search literature.[1] It was developed by Bob Boyer and J Strother Mo",
           "ore in 1977. The algorithm preprocesses the target string (key) that is being se",
           "arched for, but not the string being searched in (unlike some algorithms that preprocess the string to be searched and can then amortize the expense of the preprocessing by searching repeatedly). The execution time of the Boyer-Moore algorithm, while still linear in the size of the string being searched, can have a significantly lower constant factor than many ot", 
           "her search algorithms: it doesn't need to check every character of the stri",
           "ng to be searched, but rather skips over some of them. Generally the algorithm gets faster as the key being searched for becomes longer. Its effic",
           "iency derives from the fact that with each unsuccessful attempt to find a match between the search string and the text it is searching, it uses the information gained from that attempt to rule out as many positions of the text as po",
           "ssible where the string cannot match."};
        private static readonly Random _rnd = new Random();

        private StringArray _sa;
        private string _normalString;

        [SetUp]
        public void SetUp()
        {
            _sa = StringArray.FromString(TEXT[TEXT.Length - 1]);
            for (int i = TEXT.Length - 2; i >= 0; i--)
                _sa.Prepend(TEXT[i]);

            _normalString = string.Join("", TEXT);
        }

        [Test]
        public void ToStringTest()
        {
            Assert.That(_sa.ToString(), Is.EqualTo(_normalString));
        }

        [Test]
        public void RandomSubstringTests()
        {
            for (int i = 0; i < 10000; i++)
            {
                var start = _rnd.Next(_normalString.Length - 1);
                var end = _rnd.Next(start, _normalString.Length - 1); ;
                
                Assert.That(_sa.Substring(start, end).ToString(), Is.EqualTo(_normalString.Substring(start, end - start)), 
                    string.Format("{0}-{1}", start, end));
            }
        }

        [Test]
        public void RandomIndexOfTests()
        {
            var i = 0;
            for (i = 0; i < 100000; i++)
            {
                var s = randomSubstring();
                var start = _rnd.Next(_normalString.Length - 1);

                Assert.That(_sa.IndexOf(s, start), Is.EqualTo(_normalString.IndexOf(s, start, StringComparison.Ordinal)),
                            string.Format(i + ": start {1}, str \"{0}\"", s, start) );
            }
        }

        [Test]
        public void IndexOfTests_0()
        {
            var s = "ing searched for becomes longer. Its efficiency derives from the fact that with each unsuccessful attempt to find a match between the search string and the text it is searching, it uses the information gained from that attempt to rule out as many positions of the text as possible where the string cannot match.In computer science, the Boyer–Moore string search algorithm is a particularly efficient string searching algorithm, and it has been the standard benchmark for the practical string search literature.[1] It was developed by Bob Boyer and J Strother Moore in 1977. The algorithm preprocesses the target string (key) that is being searched for, but not the string being searched in (unlike some algorithms that preprocess the string to be searched and can then amortize the expense of the preprocessing by searching repeatedly). The execution time of the Boyer-Moore algorithm, while still linear in the size of the string being searched, can have a significantly lower constant factor than many other search algorithms: it doesn't need to check every character of the string to be searched, but rather skips over some of them. Generally the algorithm gets faster as the key being searched for becomes longer. Its efficiency derives from the fact that with each unsuccessful attempt to find a match between the search string and the text it is searching, it uses the information gained from that attempt to rule out as man";
            var start = 877;

            Assert.That(_sa.IndexOf(s, start), Is.EqualTo(_normalString.IndexOf(s, start, StringComparison.Ordinal)));
        }

        [Test]
        public void IndexOfTests_1()
        {
            var s = "as possible where the string cannot match.In computer science, the Boyer–Moore string search algorithm is a particularly efficient string searching algorithm, and it has been the standard benchmark for the practical string search literature.[1] It was developed by Bob Boyer and J Strother Moore in 1977. The algorithm preprocesses the target string (key) that is being searched for, bu";
            var start = 2111;

            Assert.That(_sa.IndexOf(s, start), Is.EqualTo(_normalString.IndexOf(s, start, StringComparison.Ordinal)));
        }

        [Test]
        public void IndexOfTests_2()
        {
            var s = "ther search algorithms: it doesn't need to check every character of the string to be searched, but rather skips over some of them. Generally the algorithm gets faster as the key being searched for becomes longer. Its efficiency derives from the fact that with each unsuccessful attempt to find a match between the search string and the text it is searching, it uses the information gained from that attempt to rule out as many positions of the text as possible where the string cannot match.In computer science, the Boyer–Moore string search algorithm is a particularly efficient string searching algorithm, and it has been the standard benchmark for the practical string search literature.[1] It was developed by Bob Boyer and J Strother Moore in 1977. Th";
            var start = 456;

            Assert.That(_sa.IndexOf(s, start), Is.EqualTo(_normalString.IndexOf(s, start, StringComparison.Ordinal)));
        }

        [Test]
        public void IndexOfTests_3()
        {
            var s = " string can";
            var start = 2534;

            Assert.That(_sa.IndexOf(s, start), Is.EqualTo(_normalString.IndexOf(s, start, StringComparison.Ordinal)));
        }

        [Test]
        public void IndexOfTests_4()
        {
            var s = "ciency derives from the fact that with each unsuccessful attempt to find a match between the search string and the text it";
            var start = 2068;

            Assert.That(_sa.IndexOf(s, start), Is.EqualTo(_normalString.IndexOf(s, start, StringComparison.Ordinal)));
        }

        [Test]
        public void IndexOfTests_5()
        {
            var s = "ring search literature.[1] It was developed by Bob Boyer and J Strother Moore in 1977. The algorithm preprocesses the target string (key) that is being searched for, but not the string being searched in (unlike some algorithms that preprocess the string to be searched and can then amortize the expense of the preprocessing by searching repeatedly). The execution time of the Boyer-Moore algorithm, while still linear in the size of the string being searched, can have a significantly lower constant factor than many other search algorithms: it doesn't need to check every character of the string to be searched, but rather skips over some of them. Generally the algorithm gets faster as the key being searched for becomes longer. Its efficiency derives from the fact that with each unsuccessful attempt to find a match between the search string and the text it is searching, it uses the information gained from that attempt to rule out as many positions of the text as possible where the string cannot match.In computer science, the Boyer–Moore string search algorithm is a particularly efficient string searching algorithm, and it has been the standard benchmark for the practical string search literature.[1] It was developed by Bob Boyer and J Strother Moore in 1977. The algorithm preprocesses the target string (key) that is bei";
            var start = 176;

            Assert.That(_sa.IndexOf(s, start), Is.EqualTo(_normalString.IndexOf(s, start, StringComparison.Ordinal)));
        }

        [Test]
        public void IndexOfTests_6()
        {
            var s = "y being searched for becomes longer. Its efficiency derives from the fact that with each unsuccessful attempt to find a match between the search string and the text it is searching, it uses the information gained from that attempt to rule out as many positions of the text as possible where the string cannot match.In computer science, the Boyer–Moore string search algorithm is a particularly efficient string searching algorithm, and it has been the standard benchmark for the practical string search literature.[1] It was developed by Bob Boyer and J Strother Moore in 1977. The algorithm preprocesses the target string (key) that is being searched for, but not the string being searched in (unlike some algorithms that preprocess the string to be searched and can then amortize the expense of the preprocessing by sear";
            var start = 373;

            Assert.That(_sa.IndexOf(s, start), Is.EqualTo(_normalString.IndexOf(s, start, StringComparison.Ordinal)));
        }

        [Test]
        public void IndexOfTests_7()
        {
            var s = " skips over some of them. Generally the algorithm gets faster as the key being searched";
            var start = 1233;

            Assert.That(_sa.IndexOf(s, start), Is.EqualTo(_normalString.IndexOf(s, start, StringComparison.Ordinal)));
        }

        private string randomSubstring()
        {
            var start = _rnd.Next(_normalString.Length - 1);
            var end = _rnd.Next(start, _normalString.Length - 1);

            return _normalString.Substring(start, end - start);
        }
    }
}
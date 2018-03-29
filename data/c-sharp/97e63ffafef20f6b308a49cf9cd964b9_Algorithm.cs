using System;
using System.Diagnostics;

namespace ProjectEuler.Solutions
{
    public class Algorithm<T> : IAlgorithm
    {
        public T Result;
        public T Dummy;
        Func<T> Solution;

        public Algorithm(Func<T> solution)
        {
            Solution = solution;
        }

        public string GetResult
        {
            get { return Result.ToString(); }
        }
        public string GetDummy
        {
            get { return Dummy.ToString(); }
        }

        public Stopwatch Run(Stopwatch watch, int warmupRounds, int benchmarkRounds)
        {
            //dummy variable trying to ensure nothing is optimized away
            //long dummy = long.MaxValue;

            //JIT/Warmup
            for (int i = 0; i < warmupRounds; i++)
            {
                Result = Solution();
                //dummy ^= result;
            }

            //Try to prevent GC run during execution
            GC.Collect();       //QUESTION: Why run collection twice? Is it because of generation GC?
            GC.Collect();

            watch.Reset();
            watch.Start();
            for (int i = 0; i < benchmarkRounds; i++)
            {
                Result = Solution();
                //dummy ^= result;
            }
            watch.Stop();
            return watch;
        }
    }
}

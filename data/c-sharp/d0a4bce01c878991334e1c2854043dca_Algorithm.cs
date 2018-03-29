namespace Genetic {
    using System;
    using System.Diagnostics;
    using System.Threading;
    
    public abstract class Algorithm<T>
            where T : struct {

        public static readonly Random Random = new Random();

        public Algorithm(int size) {
            if (size <= 0) {
                throw new ArgumentOutOfRangeException("size");
            }
            _population = new Chromosome<T>[size];
            for (int i = 0; i < size; i++) {
                Chromosome<T> c = new Chromosome<T>();
                _population[i] = c;
            }
        }

        Chromosome<T>[] _population;
        public Chromosome<T>[] Population {
            get {
                return _population;
            }
        }

        double _elitism = 0.01;
        /// <summary>
        /// Controls the elitism rate. Elite chromosomes are copied over to the next generation as is.
        /// </summary>
        public double Elitism {
            get {
                return _elitism;
            }
            set {
                _elitism = value;
            }
        }

        double _mutation = 0.25;
        public double Mutation {
            get {
                return _mutation;
            }
            set {
                _mutation = value;
            }
        }

        double _threshold = 0;
        public double Threshold {
            get {
                return _threshold;
            }
            set {
                _threshold = value;
            }
        }

        public abstract void Mutate(ref Chromosome<T> chromosome);
        public abstract double GetFitness(ref Chromosome<T> chromosome);
        public abstract bool Epoch(int epoch);

        /// <summary>
        /// Basic single-point crossover implementation.
        /// </summary>
        public Chromosome<T> Reproduce(Chromosome<T> mother, Chromosome<T> father) {
            return Reproduce(mother, father, -1);
        }

        /// <summary>
        /// Basic single-point crossover implementation.
        /// </summary>
        public virtual Chromosome<T> Reproduce(Chromosome<T> mother, Chromosome<T> father, int location) {
            if (mother == null) {
                throw new ArgumentNullException("mother");
            }
            if (father == null) {
                throw new ArgumentNullException("father");
            }

            /*  Sort parnets in the ascending order... */
            if (mother.Genes.Length > father.Genes.Length) {

                mother = Interlocked.Exchange<Chromosome<T>>(
                    ref father,
                    mother);
            }

            if (location < 0) {

                location = Random.Next(0, mother.Genes.Length);
            }

            Debug.Assert(location >= 0 && location < mother.Genes.Length, "location >= 0 && location < mother.Genes.Length");
            Debug.Assert(location >= 0 && location < father.Genes.Length, "location >= 0 && location < father.Genes.Length");

            T[] buff = new T[father.Genes.Length];

            Array.Copy(mother.Genes, 0, buff, 0, location);
            Array.Copy(father.Genes, location, buff, location, buff.Length - location);

            return new Chromosome<T>(buff);
        }

        protected virtual int Compare(Chromosome<T> left, Chromosome<T> right) {
            if (left.Fitness < right.Fitness) {
                return -1;
            }
            else if (left.Fitness > right.Fitness) {
                return 1;

            } else {
                
                return 0;
            }
        }
        
        /// <summary>
        /// Converges to solution for the specified number of epochs or until a threshold is reached.
        /// </summary>
        public void Converge(int epochs) {
            for (int epoch = 0; epoch < epochs; epoch++) {

                for (int j = 0; j < _population.Length; j++) {
                    
                    _population[j].Fitness 
                        = GetFitness(ref _population[j]);
                }

                /* move the fittest genes to the very top */

                Array.Sort(_population, Compare);

                if (!Epoch(epoch)) {
                    break;
                }

                /* check if we reached the solution  */

                if (_population.Length <= 0
                                || _population[0].Fitness <= Threshold) {

                    break;
                }

                /* the most fit genes are carried over to the next generation as is */

                int elite = (int)(_population.Length * Elitism);

                for (int i = elite; i < _population.Length; i++) {

                    /* randomly select parents from the pool for crossover */

                    _population[i] = Reproduce(
                        _population[Random.Next(_population.Length / 2)],
                        _population[Random.Next(_population.Length / 2)]);

                    /* based on the mutation rate, introduce some anomalies */

                    if (Random.NextDouble() < _mutation) {
                        Mutate(ref _population[i]);
                    }

                }

            }
        }
    }
}

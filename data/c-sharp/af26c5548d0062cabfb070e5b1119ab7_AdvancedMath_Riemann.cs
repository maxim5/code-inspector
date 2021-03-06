using System;

namespace Meta.Numerics.Functions {

    public partial class AdvancedMath {

        /// <summary>
        /// Compute the Riemann zeta function.
        /// </summary>
        /// <param name="s">The argument.</param>
        /// <returns>The value &#x3B6;(s).</returns>
        /// <remarks>
        /// <para>The Riemann &#x3B6; function can be defined as the sum of the <paramref name="s"/>th inverse power of the natural numbers.</para>
        /// <img src="../images/ZetaSeries.png" />
        /// </remarks>
        /// <seealso href="http://en.wikipedia.org/wiki/Riemann_zeta_function"/>
        public static double RiemannZeta (double s) {
            if (s < 0.0) {
                // for negative numbers, use the reflection formula
                double t = 1.0 - s;
                double z = 2.0 * Math.Pow(Global.TwoPI, -t) * Math.Cos(Global.HalfPI * t) * AdvancedMath.Gamma(t) * RiemannZeta(t);
                return (z);
            } else {
                if (Math.Abs(s - 1.0) < 0.25) {
                    // near the sigularity, use the Stjielts expansion
                    return (RiemannZeta_Series(s - 1.0));
                } else {
                    // call Dirichlet function, which converges faster
                    return (DirichletEta(s) / (1.0 - Math.Pow(2.0, 1.0 - s)));
                }
            }
        }

        /// <summary>
        /// Computes the Dirichlet eta function.
        /// </summary>
        /// <param name="s">The argument, which must be non-negative.</param>
        /// <returns>The value of &#x3B7;(s).</returns>
        /// <remarks>
        /// <para>The Dirichlet eta function is the sum of the <paramref name="s"/>th inverse power of the natural numbers,
        /// with alternating signs.</para>
        /// <img src="../images/DirichletEtaSeries.png" />
        /// <para>Because these are just the terms of the Riemann zeta function (<see cref="RiemannZeta"/>) with
        /// alternating signs, it is also called the alternating zeta function.</para>
        /// <para>It can be related to the Riemann &#x3B6; function.</para>
        /// </remarks>
        /// <exception cref="ArgumentOutOfRangeException"><paramref name="s"/> is negative.</exception>
        /// <seealso cref="RiemannZeta"/>
        /// <seealso href="http://en.wikipedia.org/wiki/Dirichlet_eta_function"/>
        public static double DirichletEta (double s) {
            if (s < 0.0) throw new ArgumentOutOfRangeException("s");
            return (DirichletEta_Sequence(s));
        }

        // Borwein's amazing method for computing eta is detailed at http://numbers.computation.free.fr/Constants/Miscellaneous/zetaevaluations.html

        // an amazing, fast, fixed-length sequence approximation to eta that is good to 1/8^(DirichletEta_Coefficients.Length) for all s > 0
        private static double DirichletEta_Sequence (double s) {
            double sum1 = 0.0;
            bool sign = true;
            for (int k = 0; k < DirichletEta_Coefficients.Length; k++) {
                double term = Math.Pow(k + 1, -s);
                if (!sign) term = -term;
                sum1 += term;
                sign = !sign;
            }
            double sum2 = 0.0;
            for (int k = 0; k < DirichletEta_Coefficients.Length; k++) {
                double term = DirichletEta_Coefficients[k] * Math.Pow(k + DirichletEta_Coefficients.Length + 1, -s);
                if (!sign) term = -term;
                sum2 += term;
                sign = !sign;
            }
            return (sum1 + sum2 / Math.Pow(2.0, DirichletEta_Coefficients.Length));
        }

        private static readonly double[] DirichletEta_Coefficients = ReimannCoefficients(15);

		private static double[] ReimannCoefficients (int n) {
			double[] e = new double[n];
			double sum = 0.0;
			for (int k = n; k>0; k--) {
				sum += (double) AdvancedIntegerMath.BinomialCoefficient(n, k);
				e[k-1] = sum;
			}
			return(e);
		}

        // The Laurent expansion of zeta near s = 1 is
        //   \zeta(s) = 1/(s-1) + \sum_{k=0}^{\infty} \gamma_k (s-1)^k / k!
        // where \gamma_k are Stieltjes constants
        // Note that the argument of this function is x = s - 1, not s

        private static double RiemannZeta_Series (double x) {
            double dz = 1.0;
            double z = 1.0 / x + StieltjesConstants[0];
            for (int i = 1; i < StieltjesConstants.Length; i++) {
                double z_old = z;
                dz = - dz * x / i;
                z += StieltjesConstants[i] * dz;
                if (z == z_old) return (z);
            }
            throw new NonconvergenceException();
        }

        // Here are the first 16 Stieltjes constants, from http://pi.lacim.uqam.ca/piDATA/stieltjesgamma.txt
        // Since the last term in the Laurent expansion of zeta goes like \gamma_n (s-1)^n / n!, this should
        // be enough to allow us to use the expansion up to (s-1) ~ 1

        private static readonly double[] StieltjesConstants = new double[] {
            0.577215664901532860607,
           -0.072815845483676724861,
           -0.009690363192872318484,
            0.002053834420303345866,
            0.002325370065467300058,
            0.000793323817301062702,
           -0.000238769345430199610,
           -0.000527289567057751046,
           -0.000352123353803039510,
           -0.000034394774418088048,
            0.000205332814909064795,
            0.000270184439543903527,
            0.000167272912105140193,
           -0.000027463806603760159,
           -0.000209209262059299946,
           -0.000283468655320241447
        };

    }

    public static partial class AdvancedComplexMath {

#if FUTURE
        public static Complex Riemann_Euler (Complex z) {
            Complex f = 1.0;
            for (int k = 0; k < primes.Length; k++) {
                Complex f_old = f;
                Complex fk = 1.0 - ComplexMath.Pow(primes[k], -z);
                f = f * fk;
                if (f == f_old) return (1.0 / f);
            }
            throw new NonconvergenceException();
        }

        private static readonly int[] primes = new int[] {
            2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53,
            59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131,
            137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223
        };
#endif

    }

}

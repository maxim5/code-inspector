(ns genetic.algorithm.algorithm
  (:use genetic.algorithm.types))

(defn horner
  "Calculates the value of a polynomial in point x given its coefficients."
  [coefficients x]
  (double
    (reduce
      (fn [accumulator coefficient] (+ (* accumulator x) coefficient))
      coefficients)))

(defn root-mean-square-error
  "Calculates the root mean square error from polynomial coefficients and
  given ideal values.
  @param coefficients - array of real values.
  @param ideal-vals - Array map where (key, val) is (x, y)."
  [ideal-vals coefficients]
  (let [calculated (map #(horner coefficients (:x %)) ideal-vals) ; calculated values for the given coeffs
        ideal (map #(:y %) ideal-vals) ; Fix the order ? ? ? ? ?
        diffs (map #(- %1 %2) calculated ideal)] ; differences between ideal and calculated
    (->> diffs
      (map #(* % %)) ; calc squares
      (reduce +) ; sum
      (Math/sqrt) ; square root
      )))

(defn generate-population
  "Generates a population. Takes a random sequence of numbers from the range
  (-PI/2, PI/2) and applies the tanges function to the value."
  [population-size indiv-size]
  (let [PI-half (/ Math/PI 2)
        random-count (* population-size indiv-size)
        rands (take random-count (repeatedly #(rand Math/PI)))]
    (->> rands
      (map #(Math/tan (Math/sqrt %)))
      (partition indiv-size)
      (map #(reverse %)) ; so the order is more human readable
      )))

(defn mutate
  "Mutates the coefficients in a way that they are totally different from the rest of the population.
  Squares every coefficient and reverses their order."
  [coefficients]
  (map #(double (if (zero? %) % (/ 1 (* % %)))) coefficients))

(defn crossover
  "Crossover function giving four children, which are built by taking the
   max value of corresponding coefficients and the same for the minimal value.
   Other two are randomly taken from values between the min and max for the given coefficients."
  [first-parent second-parent]
  (let [maxes (map #(max %1 %2) first-parent second-parent)
        mins (map #(min %1 %2) first-parent second-parent)
        in-between-fn ; function for taking random values from a range
        (fn [coeff-min coeff-max]
          (let [distance (Math/abs (- coeff-max coeff-min))
                rand-inside (rand distance)]
            [(+ coeff-min rand-inside) (- coeff-max rand-inside)]))
        in-betweens (map #(in-between-fn %1 %2) mins maxes)
        third (map #(first %) in-betweens)
        fourth (map #(last %) in-betweens)]
    [mins maxes third fourth]))

(defn distinct-randoms
  "Generates a list of distinct random integers."
  [amount max]
  (let [dim (min amount max)] ; chose min so the function will not search infinetly.
    ; Example 11 distinct elements from a range from 0 to 10 excluded.
    (take dim (distinct (repeatedly #(rand-int max))))))

(defn mutate-population
  "Mutates the population given as argument."
  [mutation-count population]
  (let [randoms (distinct-randoms mutation-count (count population))]
    (concat population (map #(mutate (nth population %)) randoms))))

(defn next-population-fn
  "Creates a function for generating a new population, given a function parts."
  [mutations-count ideal] ; amount of mutations that should take place in this iteration
  (fn [individuals]
    (let [pop-size (count individuals)
          pairs (partition 2 (shuffle individuals))
          children (apply concat (map #(apply crossover %) pairs))
          mutated (mutate-population mutations-count children)
          children-sorted (sort-by (partial root-mean-square-error ideal) mutated)
          next-gen (take pop-size children-sorted)]
      next-gen)))
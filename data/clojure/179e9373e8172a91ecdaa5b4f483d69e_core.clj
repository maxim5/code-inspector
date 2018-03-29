(ns didine.core)
(use '[clojure.string :only (split split-lines)] 'clojure.math.numeric-tower)

; CNF is stored in following format
;
; {:length 4
; :form [[1 -3 4] [-1 2 -3] [3 4 2]]
; :weights [1 2 3 4]}
;
; That would be form
;
; n = 4
; F = (x1 + x3^ + x4) * (x1^ + x2 + x3^) * (x3 + x4 + x2)
; W = [1 2 3 4]
;
; This is the form from testform.cnf, excluding weights which are generated.
;
; Pure genomes are stored as vector of vectors.
; Population after fitness computation is stored as ({:fitness 6 :genome [] :is-solution true})

(defn str-to-int
  "Converts string to number"
  [s]
  (Integer. s))

(defn random-weights
  "Generates random weights"
  [n]
  (take n (repeatedly #(rand-int 1000))))

; this is terribly ugly code. sorry about that
(defn load-dimacs-form
  "Loads DIMACS format CNF for sat from a file with random weights."
  [filename]
  (let [
  file (split-lines (slurp filename))
  ; find row starting with p, split it by spaces and take third value
  genome-length (str-to-int
          ((first (map #(split % #"\s") (filter (partial re-find #"^p") file))) 2))

  ; find rows starting with - or number, those are clausules. 0s are dropped
  clausules (map (comp (partial remove zero?) (partial map str-to-int) #(split % #"\s"))
          (filter (partial re-find #"^[-123456789]") file))

  ; we need to generate weights, DIMACS format doesn't have them
  weights (random-weights genome-length)
  ]
  {:length genome-length :form clausules :weights weights}))

(defn flip
  "Flip 0 and 1"
  [x]
  (if (= 1 x) 0 1))

(defn lkp
  "Looks up the 0/1 value of single literal in the terrible DIMACS form in 0-based vector"
  [v x]
  (if (pos? x) (v (dec x)) (flip (v (dec (abs x))))))

(defn compute-weights-sum
  "Computes weights"
  [weights genome]
  (apply + (map + weights genome)))

(defn clausules-satisfied
  "How many clausles of form are satisfied?"
  [form genome]
  (count (filter #{1} (map #(reduce max (map (partial lkp genome) %)) form))))

(defn fitness
  "Computers fitness of one genome"
  [problem genome]
  (let [
    form (:form problem)
    clausules-ratio (/ (clausules-satisfied form genome) (count form))
    is-solution (= clausules-ratio 1)
    fitness1 (* (compute-weights-sum (:weights problem) genome) clausules-ratio)
    fitness2 (if is-solution (* fitness1 1.5) fitness1)
    ]
  {:fitness (long fitness2) :genome genome :is-solution is-solution}))

(defn mutate
  "Mutates genome"
  [genome]
  (let [random-number (rand-int (count genome))]
        (assoc genome random-number (flip (genome random-number)))))

(defn one-point-cross
  "Cross two genomes at random point"
  [first-genome second-genome]
  (let [
    splitpoint (inc (rand-int (count first-genome)))
    firstsplit (split-at splitpoint first-genome)
    secondsplit (split-at splitpoint second-genome)
    newfirst (concat (firstsplit 0) (secondsplit 1))
    newsecond (concat (secondsplit 0) (firstsplit 1))]
  (list (vec newfirst) (vec newsecond))))

(defn two-point-cross
  "Genetic two-point crossover"
  [first-genome second-genome]
  (let [
    [a b] (sort (repeatedly 2 #(rand-int (count first-genome))))
    v1 (subvec first-genome 0 a)
    v2 (subvec first-genome a b)
    v3 (subvec first-genome b)
    u1 (subvec second-genome 0 a)
    u2 (subvec second-genome a b)
    u3 (subvec second-genome b)
    ]
  (list (vec (concat v1 u2 v3)) (vec (concat u1 v2 u3)))))

(defn tournament
  "Does one deterministic tournament"
  [tournament-size population] ; expectin population as [{:genome [...] :fitness 7}]
  (first (sort-by :fitness > (repeatedly tournament-size #(rand-nth population)))))

(defn tourney-and-cross
  [population tournament-size]
  (let [
    tournament-of-size (partial tournament tournament-size)
    lucky1 (:genome (tournament-of-size population))
    lucky2 (:genome (tournament-of-size population))
    ]
  (two-point-cross lucky1 lucky2)))

; this isn't very pretty. but # can't be nested and partial won't work for 1 argument
(defn generate-population
  "Generates population"
  [population-size genome-length]
  (vec (repeatedly population-size #(vec (repeatedly genome-length (partial rand-int 2))))))

; this feels almost imperative, but I still think it's the most readable way of doing it
(defn select-population
 "Builds new population from old using crossover, mutation and renew."
 [problem old-population cross-probability mutation-probability renew-probability tournament-size]
 (let [
  to-cross (round (* (count old-population) cross-probability))
  to-mutate (round (* (count old-population) mutation-probability))
  to-renew (round (* (count old-population) renew-probability))
  rest (- (count old-population) to-cross to-mutate to-renew)

  crossed-population (concat (repeatedly (/ to-cross 2) #(tourney-and-cross old-population tournament-size)))
  untangled-crossed-pop (vec (concat (map #(nth % 0) crossed-population) (map #(nth % 1) crossed-population)))
  mutated-population (repeatedly to-mutate #(mutate (:genome (rand-nth old-population))))
  new-population (generate-population to-renew (:length problem))
  rest-population (repeatedly rest #(:genome (rand-nth old-population)))
  ]
  (vec (concat untangled-crossed-pop mutated-population new-population rest-population))))

(defn avg-fitness
  "Returns average fintess for a population"
  [population]
  (/ (apply + (map :fitness population)) (count population)))

(defn best-of-population
  "Returns best individual in population"
  [population]
  (apply max-key :fitness population))

(defn genetic-algorithm
  "Does genetic algorithm"
  [problem population-size cross-prob mutation-prob renew-prob tournament-size max-generations]
  (let [
    population (generate-population population-size (:length problem))
    ]
    (println "generation, avg-fitness, best-of-gen-fitness")
    (loop [population population
           best-of-run {:fitness 0 :is-solution false}
           gens-with-same-best 0
           gen 0]
      (let [
        population-with-fitness (map (partial fitness problem) population)
        best-of-gen (best-of-population population-with-fitness)
        new-best-of-run (best-of-population [best-of-run best-of-gen])
        new-pop (select-population problem population-with-fitness cross-prob
                 mutation-prob renew-prob tournament-size)
        new-gens-with-same-best (if (= (:fitness new-best-of-run) (:fitness best-of-run)) (inc gens-with-same-best) 0)
        new-gen (inc gen)
        ]
        (println (str gen ", " (round (avg-fitness population-with-fitness)) ", "
          (round (best-of-gen :fitness)) ", " (:is-solution best-of-run)
          ", " gens-with-same-best ", " (count population)))
        (if (or
              (and (:is-solution best-of-run) (> gens-with-same-best max-generations))
              (> gen 500))
          {:best-of-run new-best-of-run :generations gen}
          (recur new-pop new-best-of-run new-gens-with-same-best new-gen))))))

(defn -main
  [& args]
  (genetic-algorithm (load-dimacs-form "data/uf20-01.cnf") 200 0.6 0.2 0.1 10 50))

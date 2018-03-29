(ns main
  (:gen-class)
  (:use [info.betareduction.dojo.holger-salesman
         :only(search
               ga1-algorithm
               random-search-algorithm
               generate-regular-polygon
               make-euclidian-problem
               sample-problem)]
        [clojure.contrib.command-line
         :only (with-command-line print-help)]))

(defn -main [& args]
  (let [usage (fn [text]
                (prn text)
                (prn "Call with --help for more help")
                (System/exit 1))]
    (with-command-line args
      "Execute some algorithm."
      [[algorithm "Which algorithm to use 'ga', 'random'" "ga"]
       [num-loops "Number of loops/generations to run" "100"]
       [problem-size "Number of edges to use, 0 for other sample problem" "10"]
       rest-args]
      (if (not (empty? rest-args))
        (usage "No free arguments allowed"))
      (let [num-loops (try (Integer/parseInt num-loops)
                           (catch Exception e
                             (usage "num-loops argument must be a positive integer")))
            problem-size (try (Integer/parseInt problem-size)
                              (catch Exception e
                                (usage "problem-size argument must a a positive integer")))
            problem (cond (= problem-size 0)
                          sample-problem
                          (neg? problem-size)
                          (usage "problem-size argument must be 0 or a positive integer")
                          :else
                          (make-euclidian-problem (generate-regular-polygon problem-size)))
            algorithm (or ({ "random" random-search-algorithm,
                             "ga" ga1-algorithm }
                           algorithm)
                          (usage (str "Unknown algorithm '" algorithm "' - use 'ga' or 'random'")))]
        (if (not (pos? num-loops))
          (usage "num-loops argument must be strictly positive"))
        (let [result (search problem algorithm num-loops)]
          (prn "Result of search: " result))))))

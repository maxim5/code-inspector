(ns info.betareduction.dojo.holger-salesman
  
  (:use [clojure.contrib.seq-utils :only (shuffle)]))



;;
;; This file contains my dabblings into search algorithms (mainly GA)
;; w.r.t the travelling salesman problem.
;;
;; We describe a problem in a generalised way as a map with two keys:
;; - :targets -- a seq of all targets that need to be visited
;; - :distance -- a 2-ary function taking a origin and target and
;;   returning a numeric distance
;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The original problem

(def
 #^{
    :doc "This is the original problem description. I added a final entry to allow circular routes."
    }
 sample-problem-costs {
                       '(a b) 2,
                       '(b a) 3,
                       '(c b) 4,
                       '(b c) 4,
                       '(c a) 5,
                       '(a c) 2,
                       '(d c) 9,
                       '(c d) 7,
                       '(d a) 10
                       })
(def
 #^{
    :doc "sample-problem-costs wrapped in the generalised problem description"
    }
 sample-problem
 {
  :distance #(or (sample-problem-costs (list %1 %2)) 1000),
  :targets (set (map first (keys sample-problem-costs)))
  })



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A more useful way of posing a TSP: as a set of point in the eucledian plane

(defn-
  #^{ :doc "Calculate the eucledian distance between the two points passed" }
  eucledian-distance [[x1, y1] [x2, y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn
  #^{ :doc "Given a seq of points, construct a generalised problem description from it." }
  make-euclidian-problem [points]
     (let [num-points (count points)
           targets (for [idx (range num-points)] (symbol (format "p%d" (inc idx))))
           distances (zipmap (for [s1 targets s2 targets] (list s1 s2))
                             (for [p1 points p2 points] (eucledian-distance p1 p2)))]
       {
        :distance #(distances (list %1 %2))
        :targets targets
        :coordinates (zipmap targets points)
        :optimal-score (* num-points (distances '(p1 p2)))
        }))

(defn
  #^{ :doc "Generate a seq of points arranged as the corners of a regular polygon." }
  generate-regular-polygon
  ([n-corners]
     ;; without a scale, align points along the unit circle
     (generate-regular-polygon n-corners 1.0))
  ([n-corners scale]
     (assert (> n-corners 2))
     (for [i (range n-corners)]
       (let [angle (/ (* i 2.0 Math/PI) n-corners)]
         [(* scale (Math/cos angle))
          (* scale (Math/sin angle))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scoring

(defn
  #^{
     :doc "Scores a given route"
     :test #(assert (= 23 (score '(a b c d) sample-problem)))
     }
  score [route problem]
  (let [n-elements (count (:targets problem))]
    (assert (= (count route) n-elements))
    (reduce +
            (map (:distance problem)
                 route
                 (next (cycle route))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Algorithms
;;
;; We have a generalised representation of search algorithms

(def
 #^{ :doc "Random search -- just shuffle the input and hope for an improvement." }
 random-search-algorithm
 {
  :initial-state (fn [start-route problem] nil),
  :next-state (fn [route last-state] (list (shuffle route) nil))
  })



(defn-
  #^{ :doc "Given a seq, swap elements at position idx1 and idx2." }
  rotate-elements [a-list idx1 idx2]
  (assert (< idx1 idx2))
  (assert (< idx2 (count a-list)))
  (assert (<= 0 idx1))
  (let [[start rest1] (split-at idx1 a-list)
        element1 (first rest1)
        [middle rest2] (split-at (dec (- idx2 idx1)) (next rest1))
        element2 (first rest2)
        end (next rest2)]
    (concat start [element2] middle [element1] end)))



(defn-
  #^{ :doc "Do a single mutation - swapping two random elements." }
  single-mutation [route]
  (let [n-elements (count route)
        first-element (rand-int n-elements)
        second-element-relative (rand-int (dec n-elements))
        second-element (if (< second-element-relative first-element)
                         second-element-relative
                         (inc second-element-relative))]
    (rotate-elements route
                     (min first-element second-element)
                     (max first-element second-element))))


(defn mutate
  #^{ :doc "Do some random number of mutations on the input" }
  ([route]
     (mutate route 20))
  ([route prob-limit]
     (loop [route route]
       (let [new-route (single-mutation route)]
         (if (< (rand-int 100) prob-limit)
           (recur new-route)
           new-route)))))



(def
  #^{ :doc "Generalised description of our the first GA algoritm.
We keep our own route as a state and mutate that accepting
a better mutation with some probability or a worse one
with the opposite probability." } 
 ga1-algorithm
     {
      :initial-state (fn [start-route problem] (list start-route
                                                     (score start-route problem)
                                                     problem))
      :next-state (fn [route old-state]
                    (let [[old-route old-score problem] old-state
                          new-route (mutate old-route)
                          new-score (score new-route problem)
                          new-score-better (> new-score old-score)
                          pick-better (< (rand-int 100) 95)]
                      (if (or (and new-score-better pick-better)
                              (and (not new-score-better) (not pick-better)))
                        (list new-route (list new-route new-score problem))
                        (list old-route (list old-route old-score problem)))))
      })


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Searching

(defn
  #^{
     :doc "General greedy search"
     }
  search
  ([problem algorithm num-loops]
     (search (shuffle (:targets problem)) problem algorithm num-loops))
  ([start-route problem algorithm num-loops]
     (loop [i 0,
            route start-route,
            state ((:initial-state algorithm) start-route problem),
            best-score (score start-route problem)]
       (let [next-try ((:next-state algorithm) route state)
             next-score (score (first next-try) problem)]
         (cond
           (= i num-loops)
           (list route best-score)
           (< next-score best-score)
           (recur (inc i) (first next-try) (second next-try) next-score)
           :else
           (recur (inc i) route (second next-try) best-score))))))





(ns euler.test.data
  (:use [euler.data] :reload)
  (:use [clojure.test]))
  
(deftest natural-numbers-test
  (is (=
    (natural-numbers 9)  [1 2 3 4 5 6 7 8 9])
    "A list of natural numbers"))

(deftest next-fib-test
  (is (= (next-fib [1 2])  [2 3])))
  
(deftest all-fibs-test
  (is (=
    (take 5 (all-fibs [1 2]))  [1 2 3 5 8])))
    
(deftest fibs-up-to-test
  (is (=
    (fibs-up-to 90 [1 2])  [1 2 3 5 8 13 21 34 55 89])
    "starting seq from 1 2")
  (is (=
    (fibs-up-to 90 [0 1])  [0 1 1 2 3 5 8 13 21 34 55 89])
    "starting seq of 0 1"))
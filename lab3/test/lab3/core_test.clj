(ns lab3.core-test
  (:require [clojure.test :refer :all]
            [lab3.1 :refer :all]
            [lab3.2 :refer :all]))

(deftest parallel-filter-test
  (is (= '(0 2 4 6 8) (parallel-filter even? (range 10) 4))))

(deftest parallel-filter-lazy-test
  (is (= '(0 2 4 6 8) (take 5 (lazy-parallel-filter even? (range) 5 2)))))
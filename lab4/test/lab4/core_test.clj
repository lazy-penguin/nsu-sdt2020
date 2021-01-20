(ns lab4.core-test
  (:require [clojure.test :refer :all]
            [lab4.core :refer :all]))

(deftest test-dnf
  (let [x (variable :x)
        y (variable :y)
        z (variable :z)]
    (testing
      (is (= (dnf (and-f (or-f (not-f x) (not-f y)) (or-f x (or-f (not-f y) (not-f z)))))
             (or-f (and-f (not-f x) (or-f x (or-f (not-f y) (not-f z)))) (and-f (not-f y) (or-f x (or-f (not-f y) (not-f z))))))))))
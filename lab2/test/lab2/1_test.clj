(ns lab2.1_test
  (:require [clojure.test :refer :all]
            [lab2.1 :refer :all]))


(deftest integrate-test-1[]
 (testing
   (is (= (Math/round (integrate #(* % %) 12)) 576))
   (is (= (Math/round (integrate #(Math/sqrt %) 100)) 667))
   (is (= (Math/round (integrate #(Math/sin  %) (Math/PI))) 2))
   (is (= (Math/round (integrate #(/ 7 (+ (* % %) 1)) 5)) 10))
   (is (= (Math/round (integrate #(/ 1 (+ 1 %)) 20)) 3))
   (is (= (Math/round (integrate #(* (Math/cos %) (Math/sin %)) 3.14)) 0))))
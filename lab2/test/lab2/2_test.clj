(ns lab2.2_test
  (:require [clojure.test :refer :all]
            [lab2.2 :refer :all]))

(deftest integrate-test-2 []
  (testing
   (is (= (Math/round ((integrate-lazy #(* % %)) 12)) 576))
   (is (= (Math/round ((integrate-lazy #(Math/sqrt %)) 100)) 667))
   (is (= (Math/round ((integrate-lazy #(Math/sin  %)) (Math/PI))) 2))
   (is (= (Math/round ((integrate-lazy #(/ 7 (+ (* % %) 1))) 5)) 10))
   (is (= (Math/round ((integrate-lazy #(/ 1 (+ 1 %))) 20)) 3))
   (is (= (Math/round ((integrate-lazy #(* (Math/cos %) (Math/sin %))) 3.14)) 0))))
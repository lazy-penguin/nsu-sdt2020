(ns lab2.2)

(defn get-partial-sum
  ([f dt] (get-partial-sum f 0 dt 0))
  ([f t dt S]
    (lazy-seq (cons S (get-partial-sum f (+ t dt) dt (+ S (f t)))))))

(defn integrate-lazy [f]
  (let [dt 0.001]
   #(* (nth (get-partial-sum f dt) (/ % dt)) dt)))
 
(letfn [(f [x] (* x x))]
  (time ((integrate-lazy f) 1000))
  (time ((integrate-lazy f) 990))
  (time ((integrate-lazy f) 1200))
  (time ((integrate-lazy f) 1000)))

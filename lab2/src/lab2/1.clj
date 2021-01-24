(ns lab2.1)

(defn trapezoidal-rule[f x S dt]
  (* dt (+ S (/ (+ (f 0) (f x)) 2))))

(defn integrate [f x]
   (loop [i 0 t 0 dt 0.001 S 0]
     (if (>= i (/ x dt))
       (trapezoidal-rule f x S dt)
       (recur (inc i) (+ t dt) dt (+ S (f t))))))

(def integrate-mem (memoize integrate))

(defn main [f]
  #(integrate-mem f %))

(letfn [(f [x] (* x x))]
  (time ((main f) 99))
  (time ((main f) 100)))
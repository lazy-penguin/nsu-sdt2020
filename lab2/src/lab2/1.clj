(ns lab2.1)

(defn integrate-in-dot [f t]
  (f t))

(def integrate-in-dot-mem (memoize integrate-in-dot))

(defn integrate [f x]
   (loop [i 0 t 0 dt 0.001 S 0]
     (if (>= i (/ x dt))
       (* S dt)
       (recur (inc i) (+ t dt) dt (+ S (integrate-in-dot-mem f t))))))

(defn main [f]
  #(integrate f %))

(letfn [(f [x] (* x x))]
  (time ((main f) 100))
  (time ((main f) 99))
  (time ((main f) 120)))
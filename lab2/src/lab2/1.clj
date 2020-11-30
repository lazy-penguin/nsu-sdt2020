(ns lab2)

(defn integrate-in-dot [f t]
  (f t))

(def integrate-in-dot-mem (memoize integrate-in-dot))

(defn integrate [f x]
   (loop [i 0 t 0 dt 0.01 S 0]
     (if (>= i (/ x dt))
       (* S dt)
       (recur (inc i) (+ t dt) dt (+ S (integrate-in-dot-mem f t))))))

(defn func [x]
  (* x x))

(defn main [f]
  #(integrate f %))

(time ((main func) 1001))
(time ((main func) 1000))
(time ((main func) 5000))
(ns lab2)

(defn integrate
  ([f dt] (integrate f 0 dt 0))
  ([f t dt S]
    (lazy-seq (cons S (integrate f (+ t dt) dt (+ S (f t)))))))

(defn func [x]
  (* x x))

(defn main [f]
  (let [dt 0.01]
   #(* (nth (integrate f dt) (/ % dt)) dt)))
 
(time ((main func) 1001))
(time ((main func) 1000))
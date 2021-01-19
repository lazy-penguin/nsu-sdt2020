(ns lab3.1)

;; size - block size
(defn split [size coll]
  (when-let [s (seq coll)]
    (cons (take size s) (split size (drop size s)))))

;;n - block count
(defn parallel-filter [pred coll n]
 (let [size (Math/ceil (/ (count coll) n))
       splited (split size coll)]
  (->>
   (range 0 n)
   (map #(future (doall (filter pred (nth splited %)))))
   (doall)
   (mapcat deref))))

(defn predicate [x]
   (Thread/sleep 100)
   (even? x))

(defn main []
  (println "Simple filter")
  (time (doall (filter predicate (range 10))))
  (println "Parallel filter with 1 block")
  (time (parallel-filter predicate (range 10) 1))
  (println "Parallel filter with 4 blocks")
  (time (parallel-filter predicate (range 10) 4)))
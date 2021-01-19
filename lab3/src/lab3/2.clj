(ns lab3.2)

;; size - block size
(defn split [size coll]
  (lazy-seq (when-let [s (seq coll)]
              (cons (take size s) (split size (drop size s))))))

(defn execute-task [pred coll]
  (->> coll
       (map #(future (doall (filter pred %))))
       (doall)
       (map deref)
       (lazy-seq)))

(defn lazy-parallel-filter [pred coll n task]
  ;; (let [size (Math/ceil (/ (count coll) n))]
    (->> coll
         (split n)
         (split task)
         (map #(execute-task pred %))
         (mapcat identity)
         (mapcat identity)))

(defn predicate [x]
  (Thread/sleep 100)
  (even? x))

(defn main []
  (time (take 10 (lazy-parallel-filter predicate (range) 5 1)))
  (time (take 10 (lazy-parallel-filter predicate (range) 5 2)))
  (time (take 10 (lazy-parallel-filter predicate (range) 5 4))))
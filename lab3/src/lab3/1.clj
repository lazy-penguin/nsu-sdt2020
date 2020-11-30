(ns lab3)

;; size - block size
(defn split [coll size]
   (lazy-seq
    (let [subseq (take size coll)]
      (cons subseq (split (drop size coll) size)))))

;;n - block count
(defn parallel-filter [pred coll n]
 (let [size (Math/ceil (/ (count coll) n))]
  (->>
   (range 0 n)
   (map #(future (filter pred (nth (split coll size) %))))
   (doall)
   (map deref)
   (apply concat))))

(take 2 (parallel-filter even? (range 100) 4))

; (defn parallel-lazy-filter [pred coll n]
;  (let [size (Math/ceil (/ (count coll) n))]
;   (lazy-seq 
;    (let [subseq ()]
;      (cons subseq (#(future (filter pred (nth (split coll size) %)))))))))

; (take 2 (parallel-lazy-filter even? (range 100) 4))
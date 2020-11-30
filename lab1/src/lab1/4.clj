(ns lab1)

(defn add-element [permutation elems]
  (map #(cons % permutation) (filter #(not (= (first permutation) %)) elems)))

(defn permutate [elems permutations]
  (mapcat #(add-element % elems) permutations))

(defn main-iterate [k & elems]
  (if (and (> k 0) (<= k (count elems)))
    (let [permutation (add-element '() elems)
          k (dec k)]
      (nth (iterate (partial permutate elems) permutation) k))
    "Incorrect input"))

(main-iterate 3 1 2 3)

(defn permutate-recur [elems k]
   (loop [permutations (add-element '() elems) k (dec k)]
   (if (== k 0)
     permutations
     (recur (mapcat #(add-element % elems) permutations) (dec k)))))

(defn main [k & elems]
  (if (and (> k 0) (<= k (count elems)))
    (permutate-recur elems k)
    "Incorrect input"))

(main 1 1 2 3 4)
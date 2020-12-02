(ns lab1)

(defn add-element [permutation elems]
   (loop [permutations '() elems elems]
     (let [first-elem (first elems)]
      (if (empty? elems)
       permutations
       (recur (if (= (first permutation) first-elem)
                permutations
                (cons (cons first-elem permutation) permutations)) (rest elems))))))

(defn for-each-permutation [permutations elems]
   (loop [result '() permutations permutations]
     (if (empty? permutations)
       result
       (recur (concat result (add-element (first permutations) elems)) (rest permutations)))))

(defn permutate [elems k]
   (loop [permutations (add-element '() elems) k (dec k)]
   (if (== k 0)
     permutations
     (recur (for-each-permutation permutations elems) (dec k)))))

(defn main [k & elems]
  (if (and (> k 0) (<= k (count elems)))
    (permutate elems k)
    "Incorrect input"))

(main 2 1 2 3 4)
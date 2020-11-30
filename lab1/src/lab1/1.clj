(ns lab1)

(defn add-element
  ([permutation elems] (add-element permutation '() elems))
  ([permutation permutations elems]
   (let [first-elem (first elems)]
     (if (empty? elems)
       permutations
       (add-element permutation
                    (if (= (first permutation) first-elem)
                      permutations
                      (cons (cons first-elem permutation) permutations)) (rest elems))))))

(defn for-each-permutation
  ([permutations elems] (for-each-permutation permutations '() elems))
  ([permutations result elems]
   (if (empty? permutations)
     result
     (for-each-permutation (rest permutations) (concat result (add-element (first permutations) elems)) elems))))

(defn permutate
  ([elems k] (permutate elems (add-element '() elems) (dec k)))
  ([elems permutations k]
   (if (= k 0)
     permutations
     (permutate elems (for-each-permutation permutations elems) (dec k)))))

(defn main [k & elems]
  (if (and (> k 0) (<= k (count elems)))
    (permutate elems k)
    "Incorrect input"))

(main 2 1 2 3 4)
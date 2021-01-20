(ns lab4.core)

; Variable
(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  (= (first expr) ::var))

(defn variable-name [var]
  (second var))

(defn same-variables? [var1 var2]
  {:pre [(and (variable? var1) (variable? var2))]}
  (= (variable-name var1) (variable-name var2)))

;; Constant
(defn const [value]
  {:pre [(boolean? value)]}
  (list ::const value))

(defn constant? [expr]
  (= (first expr) ::const))

(defn constant-value [const]
  (second const))

; Negation
(defn not-f [expr]
  (cons ::not (list expr)))

(defn not? [expr]
  (= ::not (first expr)))

; Conjunction
(defn and-f [expr & rest]
  (cons ::* (cons expr rest)))

(defn and? [expr]
  (= ::* (first expr)))

; Disjunction
(defn or-f [expr & rest]
  (cons ::+ (cons expr rest)))

(defn or? [expr]
  (= ::+ (first expr)))

; Implication
(defn impl [left right]
  (cons ::-> (list left right)))

(defn impl? [expr]
  (= ::-> (first expr)))

(defn args [expr]
  (rest expr))

(defn is-var-or-const? [expr]
  ((some-fn constant? variable?) expr))

(defn get-operation-type [expr]
  (when ((some-fn and? or? not?) expr)
    (cond (and? expr) and-f (or? expr) or-f (not? expr) not-f)))

(defn calculate-by-rules [expr rules & vars]
  ((some (fn [[_ rule]]
           (if ((first rule) expr)
             (second rule)
             false))
         (map-indexed list rules)) expr vars))

;; 1) To basic operations (and, or, not)
(declare rules)

(defn to-basic [expr]
  (calculate-by-rules expr rules))

(def rules
  (list
   [(fn [expr] (is-var-or-const? expr))
    (fn [expr _] expr)]

   [(fn [expr] (not? expr))
    (fn [expr _]
      (let [[arg] (args expr)]
        (not-f (to-basic arg))))]

   [(fn [expr] (and? expr))
    (fn [expr _]
      (let [[arg1 arg2] (args expr)]
        (and-f (to-basic arg1) (to-basic arg2))))]

   [(fn [expr] (or? expr))
    (fn [expr _]
      (let [[arg1 arg2] (args expr)]
        (or-f (to-basic arg1) (to-basic arg2))))]

   [(fn [expr] (impl? expr))
    (fn [expr _]
      (let [[arg1 arg2] (args expr)]
        (or-f (not-f (to-basic arg1)) (to-basic arg2))))]))

; 2) Replace negation
(declare replace-not)

(def not-rules
  (list
   [(fn [expr] (is-var-or-const? expr))
    (fn [expr [to-not]] (if to-not
                          (not-f expr)
                          expr))]

   [(fn [expr] (and? expr))
    (fn [expr [to-not]] (let [[arg1 arg2] (args expr)
                              negate (if to-not or-f and-f)]
                          (negate (replace-not arg1 to-not)
                                  (replace-not arg2 to-not))))]

   [(fn [expr] (or? expr))
    (fn [expr [to-not]] (let [[arg1 arg2] (args expr)
                              negate (if to-not and-f or-f)]
                          (negate (replace-not arg1 to-not)
                                  (replace-not arg2 to-not))))]

   [(fn [expr] (not? expr))
    (fn [expr [to-not]] (let [[arg] (args expr)]
                          (replace-not arg (not to-not))))]))

(defn replace-not
  ([expr] (replace-not expr false))
  ([expr to-not]
   (calculate-by-rules expr not-rules to-not)))

; 3) Distribution rules
(declare distribution)

(def distribution-rules
  (list
   [(fn [expr]
      (or (is-var-or-const? expr)
          (and (not? expr)
               (if-let [[arg] (args expr)]
                 (is-var-or-const? arg)
                 false))))
    (fn [expr _] expr)]

    ; (x v y) ^ z = (x ^ z) v (y ^ z)
   [(fn [expr]
      (and (and? expr) (or? (first (args expr)))))

    (fn [expr _]
      (let [[x-or-f-y z] (args expr)
            [x y] (args x-or-f-y)]
        (or-f (and-f (distribution x)
                     (distribution z))
              (and-f (distribution y)
                     (distribution z)))))]

    ;x ^ (y v z) = (x ^ y) v (x ^ z)
   [(fn [expr]
      (and (and? expr) (or? (second (args expr)))))

    (fn [expr _]
      (let [[x y-or-f-z] (args expr)
            [y z] (args y-or-f-z)]
        (or-f (and-f (distribution x)
                     (distribution y))
              (and-f (distribution x)
                     (distribution z)))))]

   [(fn [expr] (or? expr))
    (fn [expr _]
      (let [[arg1 arg2] (args expr)]
        (or-f (distribution arg1) (distribution arg2))))]

   [(fn [expr] (and? expr))
    (fn [expr _]
      (let [[arg1 arg2] (args expr)]
        (and-f (distribution arg1) (distribution arg2))))]))

(defn distribution [expr]
  (calculate-by-rules expr distribution-rules))

(declare variables-to-values)

(def sign-rules
  (list
   [(fn [expr] (constant? expr))
    (fn [expr _] expr)]

   [(fn [expr] (variable? expr))
    (fn [expr [var val]]
      (if (same-variables? expr var)
        (const val)
        expr))]

   [(fn [expr] (and? expr))
    (fn [expr [var val]]
      (let [[arg1 arg2] (args expr)]
        (and-f (variables-to-values arg1 var val) (variables-to-values arg2 var val))))]

   [(fn [expr] (or? expr))
    (fn [expr [var val]]
      (let [[arg1 arg2] (args expr)]
        (or-f (variables-to-values arg1 var val) (variables-to-values arg2 var val))))]

   [(fn [expr] (not? expr))
    (fn [expr [var val]]
      (let [[arg] (args expr)]
        (not-f (variables-to-values arg var val))))]))

(defn variables-to-values
  [expr var val]
  (calculate-by-rules expr sign-rules var val))

(defn signify-var [expr var val]
  {:pre [(variable? var)
         (boolean? val)]}
  (variables-to-values expr var val))

; 4) Simplify
(declare simplify)

(defn equal-not [x y]
  (and (not? x) (let [[arg] (args x)] (= arg y))))

(def simplify-rules
  (list
  ; arg1 ^ 1 = arg1
   [(fn [expr]
      (and (and? expr)
           (let [[arg1 arg2] (args expr)]
             (or (= arg1 (const true)) (= arg2 (const true))))))
    (fn [expr _]
      (let [[arg1 arg2] (args expr)]
        (simplify (if (= arg1 (const true)) arg2 arg1))))]

    ; arg1 v 1 = 1
   [(fn [expr]
      (and (or? expr)
           (let [[arg1 arg2] (args expr)]
             (or (= arg1 (const true)) (= arg2 (const true))))))
    (fn [_ _] (const true))]

    ; arg1 ^ 0 = 0
   [(fn [expr]
      (and (and? expr)
           (let [[arg1 arg2] (args expr)]
             (or (= arg1 (const false)) (= arg2 (const false))))))
    (fn [_ _] (const false))]

     ; arg1 v 0 = arg1
   [(fn [expr]
      (and (or? expr)
           (let [[arg1 arg2] (args expr)]
             (or (= arg1 (const false)) (= arg2 (const false))))))
    (fn [expr _]
      (let [[arg1 arg2] (args expr)]
        (simplify (if (= arg1 (const false))
                    arg2
                    arg1))))]

    ; arg1 ^ arg1 = arg1
   [(fn [expr]
      (and (and? expr)
           (let [[arg1 arg2] (args expr)]
             (= arg1 arg2))))
    (fn [expr _]
      (simplify (first (args expr))))]

    ; arg1 v arg1 = arg1
   [(fn [expr]
      (and (or? expr)
           (let [[arg1 arg2] (args expr)]
             (= arg1 arg2))))
    (fn [expr _]
      (simplify (first (args expr))))]

    ; arg1 ^ !arg1 / !arg1 ^ arg1 = 0
   [(fn [expr]
      (and (and? expr)
           (let [[arg1 arg2] (args expr)]
             (or (equal-not arg1 arg2) (equal-not arg2 arg1)))))
    (fn [_ _] (const false))]

    ; arg1 v !arg1 / !arg1 v arg1 = 1
   [(fn [expr]
      (and (or? expr)
           (let [[arg1 arg2] (args expr)]
             (or (equal-not arg1 arg2) (equal-not arg2 arg1)))))
    (fn [_ _] (const true))]

   [(fn [expr]
      ((some-fn and? or? not?) expr))

    (fn [expr _]
      (let [type (get-operation-type expr)
            args (args expr)]
        (apply type (map simplify args))))]

   [(fn [expr] (is-var-or-const? expr))
    (fn [expr _] expr)]))

(defn simplify [expr]
  (calculate-by-rules expr simplify-rules))




(defn dnf [expr]
  (->> expr
       (to-basic)
       (replace-not)
       (distribution)
       (simplify)))
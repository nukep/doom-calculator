(ns doomcalc.logic
  (:require [doomcalc.tree :as t :refer [mkvar]]))


(defn make-and [a b r]
  (a (r 0)
     (b (r 0) (r 1))))


(defn make-nand [a b r]
  (a (r 1)
     (b (r 1) (r 0))))


(defn make-xor [a b r]
  (a (b (r 0) (r 1))
     (b (r 1) (r 0))))


(defn make-half-adder [a b s cout]
  [(make-and a b cout)
   (make-xor a b s)])


(defn make-full-adder [cin a b s cout]
  [;; Sum bit
   (cin (a (b (s 0) (s 1))
           (b (s 1) (s 0)))
        (a (b (s 1) (s 0))
           (b (s 0) (s 1))))

   ;; Carry bit
   (cin (a (cout 0)
           (b (cout 0) (cout 1)))
        (a (b (cout 0) (cout 1))
           (cout 1)))])


(defn make-n-bit-adder [cin a-vars b-vars s-vars cout]
  (let [a-vars (reverse a-vars)
        b-vars (reverse b-vars)
        s-vars (reverse s-vars)
        bits (count a-vars)
        carry-vars (map (fn [_] (mkvar)) (range 1 bits))]

    [(if (nil? cin)
       (make-half-adder (nth a-vars 0) (nth b-vars 0) (nth s-vars 0) (nth carry-vars 0 cout))
       (make-full-adder cin (nth a-vars 0) (nth b-vars 0) (nth s-vars 0) (nth carry-vars 0 cout)))
     (vec (for [i (range 1 bits)]
            (make-full-adder (nth carry-vars (dec i)) (nth a-vars i) (nth b-vars i) (nth s-vars i) (nth carry-vars i cout))))]))


(defn make-bcd-adder [cin
                      [a3 a2 a1 a0]
                      [b3 b2 b1 b0]
                      [s3 s2 s1 s0]
                      cout]
  (let [z3 (mkvar)
        z2 (mkvar)
        z1 (mkvar)

        zc (mkvar)
        cc1 (mkvar)
        cc2 (mkvar)]
    [(make-n-bit-adder cin
                       [a3 a2 a1 a0]
                       [b3 b2 b1 b0]
                       [z3 z2 z1 s0]
                       zc)

     ;; zc + z3*z2 + z3*z1
     ;; i.e. zc + z3*(z2+z1)
     (zc (z3 (cout 0)
             (z2 (z1 (cout 0)
                     (cout 1))
                 (cout 1)))
         (cout 1))

     ;; implement a 3-bit adder manually
     ;; i.e. if cout=1, then (z3..z1)+3 -> s3..s1
     (make-half-adder     cout z1 s1 cc1)
     (make-full-adder cc1 cout z2 s2 cc2)
     (make-xor cc2 z3 s3)]))


(defn- remove-nth [v n]
  (into (subvec v 0 n) (subvec v (inc n))))

(defn- split-truth-table-at-n [table n]
  [(into {}
         (comp
          (filter (fn [[k _]] (= (nth k n) 0)))
          (map (fn [[k v]] [(remove-nth k n) v])))
         table)
   (into {}
         (comp
          (filter (fn [[k _]] (= (nth k n) 1)))
          (map (fn [[k v]] [(remove-nth k n) v])))
         table)])

(defn- solve-truth-table-recur [vars out-var table]
  (if (empty? vars)
    :UNDEFINED

    (if (= (count vars) 1)
      (let [current-var (nth vars 0)
            l (get table [0] :UNDEFINED)
            r (get table [1] :UNDEFINED)
            l (cond (= l :UNDEFINED) l
                    (= l 0) (out-var 0)
                    :else (out-var 1))
            r (cond (= r :UNDEFINED) r
                    (= r 0) (out-var 0)
                    :else (out-var 1))]
        (current-var l r))

      (let [trees (for [i (range (count vars))]
                    (let [current-var (nth vars i)
                          remaining-vars (remove-nth vars i)
                          [tt-0 tt-1] (split-truth-table-at-n table i)]
                      (current-var (solve-truth-table-recur remaining-vars out-var tt-0)
                                   (solve-truth-table-recur remaining-vars out-var tt-1))))]
        (t/minimize-trees trees)))))

(defn solve-truth-table
  "Returns a single tree.
   Note that this algorithm is expected to emit lots of unused subtrees."
  [vars out-var table]
  (solve-truth-table-recur vars out-var table))


(defn make-digit-input [[da01 da23 da45 da67 da89] [a3 a2 a1 a0]]
  [(da01 (a3 0) (a3 0))
   (da01 (a2 0) (a2 0))
   (da01 (a1 0) (a1 0))
   (da01 (a0 0) (a0 1))

   (da23 (a3 0) (a3 0))
   (da23 (a2 0) (a2 0))
   (da23 (a1 1) (a1 1))
   (da23 (a0 0) (a0 1))

   (da45 (a3 0) (a3 0))
   (da45 (a2 1) (a2 1))
   (da45 (a1 0) (a1 0))
   (da45 (a0 0) (a0 1))

   (da67 (a3 0) (a3 0))
   (da67 (a2 1) (a2 1))
   (da67 (a1 1) (a1 1))
   (da67 (a0 0) (a0 1))

   (da89 (a3 1) (a3 1))
   (da89 (a2 0) (a2 0))
   (da89 (a1 0) (a1 0))
   (da89 (a0 0) (a0 1))])


(comment

  (contains? ["a" "b"] "a")

  (-> (make-bcd-adder (mkvar :cin)
                      [(mkvar :a3) (mkvar :a2) (mkvar :a1) (mkvar :a0)]
                      [(mkvar :b3) (mkvar :b2) (mkvar :b1) (mkvar :b0)]
                      [(mkvar :s3) (mkvar :s2) (mkvar :s1) (mkvar :s0)]
                      (mkvar :cout))
      (t/simplify-trees)
      (t/debug-dot))
 ;; 
  )
(ns doomcalc.tree
  (:require [clojure.set :as set]))

;; we define trees generically.
;;
;; tree implementations can not be vectors
;; (we use vectors to represent collections of separate trees)

;; each tree is associated with 2 wadtags.
;; each tree _leaf_ is associated 1 wadtag (these are outputs).

(defonce *counter* (atom 0))
(defonce *adjacency* (atom {}))
(defn mkint [] (swap! *counter* inc))

(defn out? [tree] (= :out (first (get @*adjacency* tree))))
(defn decision? [tree] (= :decision (first (get @*adjacency* tree))))
(defn tree-varfn [tree] (nth (get @*adjacency* tree) 1))
(defn tree-varval [tree] (nth (get @*adjacency* tree) 2))
(defn tree-outval [tree] (when (out? tree) (nth (get @*adjacency* tree) 3)))
(defn tree-left [tree] (when (decision? tree) (nth (get @*adjacency* tree) 3)))
(defn tree-right [tree] (when (decision? tree) (nth (get @*adjacency* tree) 4)))

(defn traverse [tree f]
  (cond
    (out? tree)
    (f tree (tree-varfn tree) (tree-varval tree) (tree-outval tree))

    (decision? tree)
    (do
      (f tree (tree-varfn tree) (tree-varval tree) (tree-left tree) (tree-right tree))
      (traverse (tree-left tree) f)
      (traverse (tree-right tree) f))))

(defn flatten-vectors [l]
  (cond
    (vector? l) (vec (apply concat (map flatten-vectors l)))
    (nil? l)    []
    :else       [l]))

(defn traverse-trees-uniquely [trees f]
  (doseq [tree (flatten-vectors trees)]
    (traverse tree (memoize f))))


(defn inc-nil [v] (inc (or v 0)))

(defn rewrite-tree
  "Returns a tree.
   Trees are rewritten top-down (from roots to leaves)"
  [tree f]

  (let [newtree (f tree)
        varfn (tree-varfn newtree)]
    (cond
      (out? newtree)
      (varfn (tree-outval newtree))

      (decision? newtree)
      (varfn (rewrite-tree (tree-left newtree) f)
             (rewrite-tree (tree-right newtree) f)))))

(defn actual-root-nodes [trees]
  (let [trees (flatten-vectors trees)
        roots (atom (into #{} trees))]
    (traverse-trees-uniquely trees
                             (fn
                               ([_ _ _ _])
                               ([_ _ _ l r]
                                ;; remove any nodes that have parents
                                (swap! roots disj l)
                                (swap! roots disj r))))
    (vec @roots)))

(defn rewrite-trees
  "Returns a vector of trees.
   Trees are rewritten top-down (from roots to leaves)"
  [trees f]
  ;; rewrite every root node.
  ;; note: it's possible to rewrite trees in such a way that former root nodes have parents.
  ;; we correct for this edge case with (actual-root-nodes).
  (actual-root-nodes (mapv #(rewrite-tree % f) (flatten-vectors trees))))


(defn simplify-trees
  "Does simplification of some nodes. Some optimizations can only be done once the whole tree is known.
     Optimization 1) if any variables only appear once and are root nodes, then outputs setting the variable are rerouted.
       1. count variable occurrences
       2. for all with a count of 1, find the ones that are also root nodes and have been used as outputs
          (tracking outputs is important because if nothing writes to the variable, it's probably set from outside the tree)
       3. keep them in a list and go to them during traversal"
  [trees]
  (let [trees (flatten-vectors trees)
        varfn-counts (atom {})
        varfn-as-outputs (atom #{})
        _ (traverse-trees-uniquely trees
                                   (fn
                                     ([t varfn varval v] (swap! varfn-as-outputs conj varfn))
                                     ([t varfn varval l r] (swap! varfn-counts update varfn inc-nil))))

        varfns-of-roots (into #{} (comp (filter decision?) (map tree-varfn)) trees)
        root-varfn->leftright (into {}
                                    (comp (filter decision?)
                                          (map (fn [t] [(tree-varfn t) [(tree-left t) (tree-right t)]])))
                                    trees)
        varfns-once (into #{} (comp (filter (fn [[_ v]] (= v 1))) (map first)) @varfn-counts)
        varfns-to-simplify (set/intersection varfns-of-roots varfns-once @varfn-as-outputs)

        actual (fn [tree]
                 (cond
                   (out? tree)
                   (if (contains? varfns-to-simplify (tree-varfn tree))
                     ;; This output goes directly somewhere else.
                     (let [leftright (root-varfn->leftright (tree-varfn tree))
                           v (tree-outval tree)
                           choose (if (= v 0) (first leftright) (second leftright))]
                       choose)

                     tree)

                   (decision? tree)
                   tree))

        ;; remove root nodes that are simplified away
        newtrees (into [] (remove (fn [t] (contains? varfns-to-simplify (tree-varfn t)))) trees)]
    (rewrite-trees newtrees actual)))

(defn visit-dot-creator []
  (let [counter (atom 100)
        h (memoize (fn [_] (swap! counter inc)))]
    (fn
      ([out-tree varfn varval v]
       (println (str (h out-tree) "[label=\"" (if varval varval (h varfn)) "." v "\",shape=rectangle]")))
      ([decision-tree varfn varval l r]
       (let [num (h decision-tree)]
         (println (str num "[label=\"" (if varval varval (h varfn)) "\"]"))
         (println (str num " -> " (h l) " [label=0,style=dashed]"))
         (println (str num " -> " (h r) " [label=1]")))))))

(defn debug-dot
  "Prints a DOT representation of the trees. Useful for debugging!"
  [trees]
  (traverse-trees-uniquely trees (visit-dot-creator)))

(defn emit-variable-output [varfn varval outval]
  (let [num (mkint)]
    (swap! *adjacency* assoc num [:out varfn varval outval])
    num))

(defn emit-decision [varfn varval left right]
  (let [num (mkint)]
    (swap! *adjacency* assoc num [:decision varfn varval left right])
    num))

(defn mkvar
  ([] (mkvar nil))

  ([varval & {:keys [optimize-same-left-right?]
              :or {optimize-same-left-right? true}}]
   (let [intern-decisions (atom {})
         intern-out-0 (atom nil)
         intern-out-1 (atom nil)]
     ;; We're treating a function as an opaque "Variable" type that we can call.
     ;; It would be valid to extract this into a deftype, but this is easy.
     (letfn [(varfn
               ;; 1-arity: Variable output
               ([outval]
                (let [a (if (= outval 0) intern-out-0 intern-out-1)
                      num @a
                      new? (nil? num)
                      num (if new? (emit-variable-output varfn varval outval) num)
                      _ (reset! a num)]
                  num))

               ;; 2-arity: Decision
               ([left right]
                (cond
                  (= left :UNDEFINED) right
                  (= right :UNDEFINED) left
                  (and optimize-same-left-right? (= left right)) left
                  :else
                  (let [num  (get @intern-decisions [left right])
                        new? (nil? num)
                        num (if new? (emit-decision varfn varval left right) num)
                        _ (swap! intern-decisions assoc [left right] num)]
                    num))))]
       varfn))))

(defn count-unique-tree-values [tree]
  (let [uniq-values (atom #{})]
    (traverse tree (fn ([_ _ _ _]) ([v _ _ _ _] (swap! uniq-values conj v))))
    (count @uniq-values)))

(defn minimize-trees [trees]
  ;; count all the unique values in the tree
  ;; return the tree with the smallest number of unique values
  (second
   (reduce
    (fn [[c1 t1] [c2 t2]]
      (if (<= c1 c2) [c1 t1] [c2 t2]))

    (for [tree trees]
      [(count-unique-tree-values tree) tree]))))




(comment
  (let [a (mkvar [1 2])
        b (mkvar [3 4])
        r (mkvar [5 6])]
    (make-level-parts (make-nand a b r)
                      100
                      [r]))

  (let [a (mkvar "a")
        b (mkvar "b")
        r (mkvar "r")]
    (make-and a a r))

  (make-half-adder (mkvar "a") (mkvar "b") (mkvar "s") (mkvar "cout"))


  ;; not all the input variables are intended to be realized, so we have to disable some optimizations.
  (make-digit-input (mapv (fn [s] (mkvar s :optimize-same-left-right? true)) ["da01" "da23" "da45" "da67" "da89"])
                    (mapv mkvar ["a3" "a2" "a1" "a0"]))

  (make-bcd-adder nil
                  [(mkvar "b3") (mkvar "b2") (mkvar "b1") (mkvar "b0")]
                  [(mkvar "d3") (mkvar "d2") (mkvar "d1") (mkvar "d0")]
                  [(mkvar "s3") (mkvar "s2") (mkvar "s1") (mkvar "s0")]
                  (mkvar "cout0"))
  )
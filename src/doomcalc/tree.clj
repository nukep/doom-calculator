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
  (let [ff (memoize f)]
    (doseq [tree (flatten-vectors trees)]
      (traverse tree ff))))


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
       3. keep them in a list and go to them during traversal
     Optimization 2) if any variable output is a root tree, it's a constant. rewrite trees that use the variable.
   "
  [trees]
  (let [trees (flatten-vectors trees)

        ;; all root trees that are outputs are constants.
        var-constants (into {}
                            (comp (filter out?)
                                  (map (fn [t] [(tree-varfn t) (tree-outval t)])))
                            trees)

        varfn-counts (atom {})
        varfn-as-outputs (atom #{})
        _ (traverse-trees-uniquely trees
                                   (fn
                                     ([t varfn varval v])
                                     ([t varfn varval l r]
                                      (swap! varfn-counts update varfn inc-nil)
                                      ;; only consider outputs with parents
                                      (when (out? l) (swap! varfn-as-outputs conj (tree-varfn l)))
                                      (when (out? r) (swap! varfn-as-outputs conj (tree-varfn r))))))

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
                   ;; if the tree uses a variable that's a constant, choose its left or right child
                   (let [constant (get var-constants (tree-varfn tree))]
                     (cond
                       (= constant 0) (tree-left tree)
                       (= constant 1) (tree-right tree)
                       :else tree))))

        ;; remove root nodes that are simplified away
        newtrees (into [] (remove (fn [t] (contains? varfns-to-simplify (tree-varfn t)))) trees)]
    (rewrite-trees newtrees actual)))

(defn transpose-adjlist
  "Transposes the adjaency list.
  An adjacency list is a map of sets. e.g. {1: #{:a :b :c}, 2...}.
  a is an m*n list. Returns an n*m list."
  [a]
  (let [m-items (set (keys a))
        n-items (reduce set/union (vals a))]
    (into {}
          (remove (comp empty? second))
          (for [j n-items]
            [j (into #{}
                     (mapcat (fn [i] (when (contains? (get a i) j)
                                       [i])))
                     m-items)]))))

(defn *-adjlists
  "Like (0,1)-matrix multiplication, but for adjacency lists.
  An adjacency list is a map of sets. e.g. {1: #{:a :b :c}, 2...}.
  Domain is keys, codomain is elements of the set. Domain x Codomain.

  a is an m*n list, b is an n*o list. Returns an m*o list."
  [a b]
  (let [m-items (keys a)
        o-items (reduce set/union (vals b))

        dot (fn [x y] (some? (seq (set/intersection x y))))
        row (fn [adj i] (get adj i #{}))
        col (fn [adj j] (into #{}
                              (comp (filter (fn [[_ v]] (contains? v j)))
                                    (map first))
                              adj))]
    (into {}
          (remove (comp empty? second))
          (for [i m-items]
            [i (into #{}
                     (mapcat (fn [j] (when (dot (row a i) (col b j))
                                       [j])))
                     o-items)]))))

(defn *-adjlists-transposed-second
  "An optimization of (*-adjlists a b), where b is transposed."
  [a b]
  (let [m-items (keys a)
        o-items (keys b)

        dot (fn [x y] (some? (seq (set/intersection x y))))
        row (fn [adj i] (get adj i #{}))]
    (into {}
          (remove (comp empty? second))
          (for [i m-items]
            [i (into #{}
                     (mapcat (fn [j] (when (dot (row a i) (row b j))
                                       [j])))
                     o-items)]))))

(defn +-adjlists
  [a b]
  (into {}
        (for [i (set/union (set (keys a)) (set (keys b)))]
          [i (set/union (get a i #{}) (get b i #{}))])))

(defn tree->input-output-vars [tree]
  (let [invars (atom #{})
        outvars (atom #{})]
    (traverse tree
              (fn
                ([t varfn varval v] (swap! outvars conj varfn))
                ([t varfn varval l r] (swap! invars conj varfn))))
    [@invars @outvars]))

(defn prune-unreachable-trees
  "Keep the trees that eventually output to the provided variables, and remove the rest.
   Note that this operates on entire trees, and doesn't remove/rewrite subtrees.

   We solve this as follows:
   
   Let I be an adjacency matrix of trees to input variables.
   Let J be an adjacency matrix of output variables to trees.

   K = I * J
   Let K be an adjacency matrix of trees to trees (from output trees to input trees).
   
   L = 1 + K + K*K + K*K*K + ...
   Let L be an adjacency matrix of trees to trees (from output trees to all eventually reachable input trees).
   
   R = reachablevars*J * L
   Let R be a row vector of trees that are reachable from the variables.
   reachablevars is a row vector.
   
   Simplified:
   R = (reachablevars*J) * (1 + K + K*K + K*K*K + ...)
   n = reachablevars*J
   R = n + nK + nKK + nKKK + ...
   "
  [trees reachablevars]
  (let [tio (map (fn [t] [t (tree->input-output-vars t)])
                 (flatten-vectors trees))
        I (into {}
                (map (fn [[k v]] [k (first v)]))
                tio)
        Jt (into {}
                 (map (fn [[k v]] [k (second v)]))
                 tio)
        J (transpose-adjlist Jt)

        K (*-adjlists-transposed-second I Jt)
        n (*-adjlists {:result (set reachablevars)} J)
        R (loop [term n
                 sum term]
            (let [term (*-adjlists term K)
                  newsum (+-adjlists sum term)]
              ;; the sum will eventually converge. end the loop when it does.
              (if (= sum newsum)
                sum
                (recur term newsum))))]
    (vec (:result R))))

(comment
  (let [a (mkvar :a), b (mkvar :b), c (mkvar :c), d (mkvar :d), e (mkvar :e)
        r (mkvar :r), s (mkvar :s), t (mkvar :t), y (mkvar :y), z (mkvar :z)

        trees
        [(a (b (r 1)
               (c (s 0)
                  (s 1)))
            (r 1))
         (r (t 1)
            (a (t 0)
               (t 1)))
         (y (z (c 1)
               (c 0))
            (d 1))
         (d (e 0)
            (e 1))]]
    (identity
     (prune-unreachable-trees trees [s t]))))

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


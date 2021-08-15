(ns doomcalc.core
  (:require [doomcalc.digits :as digits]
            [doomcalc.write-pwad :refer [spit-pwad]]
            [doomcalc.wad-builder :as w]))

(defrecord Varbit [var bit])
(defn varbit [var bit] (Varbit. var bit))
(defn varbit? [v] (instance? Varbit v))
;; dnode = "decision node"
(defn dnode? [v] (not (varbit? v)))

(defn compose
  "Combine all provided graphs into one graph. Each node is renamed. Graph is not simplified."
  [& graphs]
  (let [out (atom {})
        node-counter (atom 0)
        rename-node-impl (memoize (fn [_] (let [id (swap! node-counter inc)]
                                            (keyword (str "n" id)))))
        rename-node (fn [graph-num s] (cond (varbit? s) s
                                            (dnode? s)  (rename-node-impl (str graph-num s))))

        add (fn [node v] (swap! out assoc node v))]

    (doseq [[i graph] (zipmap (range) graphs)]
      (doseq [[node [var [on0 on1]]] graph]
        (if (varbit? node)
          (add node nil)
          (add (rename-node i node)
               [var [(rename-node i on0) (rename-node i on1)]]))))
    @out))

(defn nodes-with-parents [graph]
  (into #{}
        (mapcat (fn [[_node [var [on0 on1]]]] (when var [on0 on1])))
        graph))
(defn root-nodes [graph]
  (let [node-has-parent? (nodes-with-parents graph)]
    (into #{} (remove node-has-parent?) (keys graph))))

(defn wrap-gen-wadtag [v]
  (cond
    ;; use 0-arity function provided
    (fn? v) v

    ;; create autoincrement function, using v as starting number.
    (number? v) (let [counter (atom v)]
                  (fn [] (first (swap-vals! counter inc))))

    :else (throw (ex-info "Bad type" {}))))

;; Convert the graph into a list of Tele2 and Droom parts.
;; Each dnode is a Tele2.
;; Each varbit that's not an input or output is a Droom.
;;
;; If an output varbit is also used in a node, a Droom is created.
;; The external output will be a Tele2 where I0 and I1 are that var.
(defn materialize-graph [graph in-varbit-wadtags out-varbit-wadtags gen-wadtag]
  (let [gen-wadtag (wrap-gen-wadtag gen-wadtag)
        out (atom [])
        add (fn [type v] (swap! out conj [type v]))

        vars-used-by-nodes (into #{} (map (fn [[_node [var _]]] var)) graph)

        intermediate-varbit-wadtags (atom #{})
        output-varbit->intermediate-wadtag (atom {})
        interm-varbit->wadtag* (memoize (fn [_] (gen-wadtag)))

        node->wadtag (memoize (fn [_] (gen-wadtag)))
        varbit->wadtag (fn [x]
                         (if-let [ot (get out-varbit-wadtags x)]
                           (cond
                             ;; varbit is an output, but also used by a node in the graph.
                             (contains? vars-used-by-nodes (:var x))
                             (let [t (interm-varbit->wadtag* x)]
                               (swap! intermediate-varbit-wadtags conj t)
                               (swap! output-varbit->intermediate-wadtag assoc x t)
                               t)

                             ;; varbit is an output, and not used by a node in the graph.
                             :else
                             ot)

                           (if-let [ot (get in-varbit-wadtags x)]
                             ;; varbit is an input.
                             ot

                             (let [t (interm-varbit->wadtag* x)]
                               (swap! intermediate-varbit-wadtags conj t)
                               t))))
        node-or-varbit->wadtag (fn [x] (cond
                                         (varbit? x) (varbit->wadtag x)
                                         (dnode? x)  (node->wadtag x)))

        node-has-parent? (nodes-with-parents graph)]

    (doseq [[node [var [on0 on1]]] graph]
      (when (dnode? node)
        (add :tele2 {:X  (node->wadtag node)
                     :I0 (varbit->wadtag (varbit var 0))
                     :I1 (varbit->wadtag (varbit var 1))
                     :o0 (node-or-varbit->wadtag on0)
                     :o1 (node-or-varbit->wadtag on1)
                     :item (if (node-has-parent? node) :teleporter :monster)})))

    (doseq [wadtag @intermediate-varbit-wadtags]
      (add :droom {:Y wadtag}))
    (doseq [[varbit intermediate-wadtag] @output-varbit->intermediate-wadtag]
      (let [final-wadtag (get out-varbit-wadtags varbit)]
        (add :tele2 {:X  0
                     :I0 intermediate-wadtag
                     :I1 intermediate-wadtag
                     :o0 final-wadtag
                     :o1 final-wadtag
                     :item :monster})))

    @out))

;; "AND" decision graph
;;
;; In English:
;;  Start at :node0.
;;
;;  :node0
;;    If variable "a" is 0, then turn on the "r0" varbit (variable "r", bit 0).
;;    If variable "a" is 1, then go to :node1.
;;
;;  :node1
;;    If variable "b" is 0, then turn on the "r0" varbit (variable "r", bit 0).
;;    If variable "b" is 1, then turn on the "r1" varbit (variable "r", bit 1).
;;
;; Variables are different from nodes, because a variable can be used in many nodes in a graph.
;;
;;      (a) :node0
;;     /0 1\
;;    /    (b) :node1
;; [r0]   /0 1\
;;     [r0]   [r1]
(defn make-and [a b r]
  {:node0 [a [(varbit r 0) :node1]]
   :node1 [b [(varbit r 0) (varbit r 1)]]
   (varbit r 0) nil
   (varbit r 1) nil})

(defn make-nand [a b r]
  {:node0 [a [(varbit r 1) :node1]]
   :node1 [b [(varbit r 1) (varbit r 0)]]
   (varbit r 0) nil
   (varbit r 1) nil})

(defn make-or [a b r]
  {:node0 [a [:node1 (varbit r 1)]]
   :node1 [b [(varbit r 0) (varbit r 1)]]
   (varbit r 0) nil
   (varbit r 1) nil})

(defn make-xor [a b r]
  {:node0 [a [:node1 :node2]]
   :node1 [b [(varbit r 0) (varbit r 1)]]
   :node2 [b [(varbit r 1) (varbit r 0)]]
   (varbit r 0) nil
   (varbit r 1) nil})

(defn make-half-adder [a b s cout]
  (compose (make-and a b cout)
           (make-xor a b s)))

(defn make-full-adder [cin a b s cout]
  ;; This one takes up less space than stringing together logic gates.
  {;; S (sum)
   :Snode0 [cin [:Snode1 :Snode2]]
   :Snode1 [a   [:Snode3 :Snode4]]
   :Snode2 [a   [:Snode4 :Snode3]]
   :Snode3 [b   [(varbit s 0) (varbit s 1)]]
   :Snode4 [b   [(varbit s 1) (varbit s 0)]]

   ;; C (carry out)
   :Cnode0 [cin [:Cnode1 :Cnode2]]
   :Cnode1 [a   [(varbit cout 0) :Cnode3]]
   :Cnode2 [a   [:Cnode3 (varbit cout 1)]]
   :Cnode3 [b   [(varbit cout 0) (varbit cout 1)]]

   (varbit s 0) nil
   (varbit s 1) nil
   (varbit cout 0) nil
   (varbit cout 1) nil})

(def ^:dynamic *genvar-counter* (atom 0))

(defn genvar []
  (let [[id _] (swap-vals! *genvar-counter* inc)]
    (keyword (str "tmp" id))))

(defmacro with-new-genvar [& body]
  `(binding [*genvar-counter* (atom 0)]
     ~@body))


(defn test-graph [graph variable-values]
  (let [cursors (atom (root-nodes graph))
        variable-values (atom variable-values)]
    (loop [prev-cursors @cursors]
      (doseq [cursor @cursors]
        (let [[var-at-cursor choices] (get graph cursor)]
          (when (contains? @variable-values var-at-cursor)
            (let [goto-node-or-varbit (get choices (get @variable-values var-at-cursor))]
              (cond
                ;; Node: Change cursor to new node
                (dnode? goto-node-or-varbit)
                (do
                  (swap! cursors (fn [x] (-> (disj x cursor)
                                             (conj goto-node-or-varbit)))))

                ;; Varbit: Set variable, remove cursor
                (varbit? goto-node-or-varbit)
                (let [varbit goto-node-or-varbit]
                  (swap! variable-values assoc (:var varbit) (:bit varbit))
                  (swap! cursors disj cursor)))))))

      (if (empty? @cursors)
        @variable-values
        (if (= prev-cursors @cursors)
          (throw (ex-info "Graph never finishes" {}))
          (recur @cursors))))))

(defn minimize
  "Return the item with the lowest score"
  [score-fn coll]
  (second (reduce (fn [[prev-score prev-item] item]
                    (let [new-score (score-fn item)]
                      (if (or (nil? prev-score)
                              (< new-score prev-score))
                        [new-score item]
                        [prev-score prev-item])))
                  nil
                  coll)))

(defn keys-with-duplicate-values [m pred]
  (sequence (comp (map (fn [[_ v]] (map first v)))
                  (filter (fn [x] (> (count x) 1))))
            (group-by second
                      (into {}
                            (filter (fn [[k _v]] (pred k)))
                            m))))

(comment
  
  (keys-with-duplicate-values {1 "a", 2 "a", 3 "b", 4 "c", 5 "c"})
  ;; => ((1 2) (4 5))

  )


(defn graph-count-parents
  "Returns map of node -> number of parents"
  [graph]
  (reduce (fn [counts [node [_var [on0 on1]]]]
            (let [counts (update counts node #(or % 0))]
              (if (dnode? node)
                (let [counts (update counts on0 #(inc (or % 0)))
                      counts (update counts on1 #(inc (or % 0)))]
                  counts)
                counts)))
          {}
          graph))

(defn graph-node-children [graph node]
  (let [v (get graph node)]
    (if v
      (let [[_var [on0 on1]] v]
        [on0 on1])
      [])))

(defn remove-subgraphs-if-root [graph nodes]
  (loop [graph graph
         nodes (vec nodes)
         node->parent-count (graph-count-parents graph)]
    (if (empty? nodes)
      graph
      (let [[node & rest-nodes] nodes
            rest-nodes (vec rest-nodes)]
        (if (= (node->parent-count node) 0)
          ;; is a root node
          (recur (dissoc graph node)
                 (into rest-nodes (graph-node-children graph node))
                 (reduce (fn [c n] (update c n dec))
                         node->parent-count
                         (graph-node-children graph node)))

          ;; is not
          (recur graph rest-nodes node->parent-count))))))

;; Simplification algorithms typically remove nodes and replace edges.
;; But sometimes the replacement edges point to nodes that also get removed,
;; so we have to adjust the replace-with lookup to recurse through all the replacements.
(defn adjust-replace-with [replace-with]
  (into {}
        (map (fn find-true-replacement [from]
               (loop [x from]
                 (if (contains? replace-with x)
                   (recur (get replace-with x))
                   [from x]))))
        (keys replace-with)))

;; The simplification algorithms use a remove-and-replace technique.
;; This helper encapsulates the busy work of rerouting the graph.
(defn remove-and-replace [graph f]
  (let [remove-nodes (atom #{})
        replace-on-with (atom {})
        remove-root-subgraphs (atom #{})

        _ (f (fn [node] (swap! remove-nodes conj node))
             (fn [from to] (swap! replace-on-with assoc from to))
             (fn [node] (swap! remove-root-subgraphs conj node)))

        replace-on-with @replace-on-with
        remove-nodes @remove-nodes
        replace-on-with (adjust-replace-with replace-on-with)]

    (if (and (empty? remove-nodes) (empty? replace-on-with))
      graph

      (into {}
            (comp (remove (fn [[node _]] (remove-nodes node)))
                  (map (fn [[node [var [on0 on1]]]]
                         (if (varbit? node)
                           [node nil]
                           [node [var [(replace-on-with on0 on0) (replace-on-with on1 on1)]]]))))
            graph))))

;; 1. Flag all nodes that have same on0 and on1 child
;; 2. Remove nodes, join each parent edge to child
(defn simplify-bdd-redundant-nodes [graph]
  (remove-and-replace
   graph
   (fn [remove-node replace-on-with _remove-root-subgraph]
     (doseq [[node [_var [on0 on1]]] graph]
       (when (dnode? node)
         (when (= on0 on1)
           (remove-node node)
           (replace-on-with node on0)))))))

(comment
  (simplify-bdd-redundant-nodes {:n1 [:a [(varbit :r 0) (varbit :r 0)]]
                                 (varbit :r 0) nil})
  ;; => {#doomcalc.core.Varbit{:var :r, :bit 0} nil}

  )

;; Combine any nodes v and w that have the exact same output pairs (on0 and on1).
;; i.e. (v.on0,v.on1) = (w.on0,w.on1)
;; Also known as "merge isomorphic subgraphs".
(defn simplify-bdd-dedupe [graph]
  (remove-and-replace
   graph
   (fn [remove-node replace-on-with _remove-root-subgraph]
     (doseq [duplicate-nodes (keys-with-duplicate-values graph dnode?)]
       (let [[keep-node & rest-nodes] (sort duplicate-nodes)]
         (doseq [node rest-nodes]
           (remove-node node)
           (replace-on-with node keep-node)))))))

;; If any node points to :UNDEFINED, remove that node and make the parents point to the other edge (even if the other edge also points to :UNDEFINED).
(defn simplify-bdd-remove-undefined [graph]
  (remove-and-replace
   graph
   (fn [remove-node replace-on-with]
     (doseq [[node [_var [on0 on1]]] graph]
       (cond
         (= on0 :UNDEFINED)
         (do
           (remove-node node)
           (replace-on-with node on1))

         (= on1 :UNDEFINED)
         (do
           (remove-node node)
           (replace-on-with node on0))

         :else
         nil)))))

;; If a variable is used by exactly one node, and that node is a root node, then the node can be removed.
;; The variable must be written to by something in the graph (i.e. be a varbit node).
;;   Otherwise the variable's assumed to be an "input" and won't be considered.
(defn simplify-graph-remove-singleton-vars
  [graph except-vars]
  (remove-and-replace
   graph
   (fn [remove-node replace-on-with _remove-root-subgraph]
     (let [var->nodes (atom {})
           root-nodes (root-nodes graph)
           vars-written-to (into #{}
                                 (mapcat (fn [[node _]] (when (varbit? node)
                                                          (let [var (:var node)]
                                                            ;; neither the 0 or 1 varbit for the var can be a root node.
                                                            (when-not (or (contains? root-nodes (varbit var 0))
                                                                          (contains? root-nodes (varbit var 1)))
                                                              [var])))))
                                 graph)]
       (doseq [[node [var [on0 on1]]] graph]
         (when (dnode? node)
           (when-not (contains? except-vars var)
             (swap! var->nodes update var conj node))))

       (doseq [[var nodes] @var->nodes]
         (when (contains? vars-written-to var)
           (when (= (count nodes) 1)
             (let [node (first nodes)
                   [_var [on0 on1]] (get graph node)]
               (when (contains? root-nodes node)
                 (remove-node node)
                 (remove-node (varbit var 0))
                 (remove-node (varbit var 1))
                 (replace-on-with (varbit var 0) on0)
                 (replace-on-with (varbit var 1) on1))))))))))

(comment
  (simplify-graph-remove-singleton-vars
   {:node0 [:a [(varbit :r 0) :node1]]
    :node1 [:b [(varbit :r 1) (varbit :r 0)]]

    :node2 [:r [:node3 :node4]]

    (varbit :r 0) nil
    (varbit :r 1) nil
    (varbit :s 0) nil}
   #{})
  ;; => {:node0 [:a [:node3 :node1]], :node1 [:b [:node4 :node3]], #doomcalc.core.Varbit{:var :s, :bit 0} nil}
  )


(defn reduce-graph-vars [graph var-values]
  (let [remove-subgraphs (atom #{})
        remove-subgraphs-weak-parents (atom {})
        graph (remove-and-replace
               graph
               (fn [remove-node replace-on-with remove-root-subgraph]
                 (doseq [[node [var [on0 on1]]] graph]
                   (when (dnode? node)
                     (let [val    (get var-values var)
                           on     (if (= val 0) on0 on1)
                           not-on (if (= val 0) on1 on0)]
                       (when-not (nil? val)
                         (remove-node node)
                         (replace-on-with node on)

                         (remove-root-subgraph not-on)

                         ;; we remove one of the parents of the "on" node
                         (swap! remove-subgraphs-weak-parents update on (fn [v] (inc (or v 0))))
                         (swap! remove-subgraphs conj not-on)))))))]
    graph))

(comment
  
  (graph->dot
   (make-full-adder :cin :a :b :s :cout))
  
  (graph->dot
  (reduce-graph-vars (make-and :a :b :r)
                     {:a 0})
  )

  )

(defn simplify-graph [graph]
  ;; Keep simplifying until nothing changes
  (loop [graph graph]
    (let [new-graph (-> graph
                        (simplify-bdd-dedupe)
                        (simplify-bdd-redundant-nodes)
                        (simplify-bdd-remove-undefined)
                        (simplify-graph-remove-singleton-vars #{})
                        (reduce-graph-vars {:0 0 :1 1}))]
      (if (= new-graph graph)
        new-graph
        (recur new-graph)))))

(defn make-n-bit-adder [cin a-vars b-vars s-vars cout]
  ;; Chain adders together
  (let [a-vars (reverse a-vars)
        b-vars (reverse b-vars)
        s-vars (reverse s-vars)
        bits (count a-vars)
        carry-vars (map (fn [_] (genvar)) (range 1 bits))

        graphs (atom [])
        insert (fn [graph] (swap! graphs conj graph))]
    ;; Pattern:
    #_(comment
        (make-half-adder         :a0 :b0 :s0 :carry0)
        (make-full-adder :carry0 :a1 :b1 :s1 :carry1)
        (make-full-adder :carry1 :a2 :b2 :s2 :carry2)
        (make-full-adder :carry2 :a3 :b3 :s3 :cout))

    (if (nil? cin)
      (insert (make-half-adder (nth a-vars 0) (nth b-vars 0) (nth s-vars 0) (nth carry-vars 0 cout)))
      (insert (make-full-adder cin (nth a-vars 0) (nth b-vars 0) (nth s-vars 0) (nth carry-vars 0 cout))))
    (doseq [i (range 1 bits)]
      (insert (make-full-adder (nth carry-vars (dec i)) (nth a-vars i) (nth b-vars i) (nth s-vars i) (nth carry-vars i cout))))

    (apply compose @graphs)))

(comment
  (graph->dot (simplify-graph (reduce-graph-vars (simplify-graph (make-n-bit-adder :1 [:0 :0] [:b1 :1] [:s1 :s0] :cout))
                                                 {:cin 1})))
  (graph->dot (reduce-graph-vars (make-n-bit-adder :1 [:0 :0] [:b1 :1] [:s1 :s0] :cout)
                                 {:0 0 :1 1}))
  ;;
  )

(defn make-bcd-adder [cin
                      [a3 a2 a1 a0]
                      [b3 b2 b1 b0]
                      [s3 s2 s1 s0]
                      cout]
  (let [graphs (atom [])
        insert (fn [graph] (swap! graphs conj graph))
        z3 (genvar)
        z2 (genvar)
        z1 (genvar)

        zc (genvar)
        cc1 (genvar)
        cc2 (genvar)]
    (insert (make-n-bit-adder cin
                              [a3 a2 a1 a0]
                              [b3 b2 b1 b0]
                              [z3 z2 z1 s0]
                              zc))

    ;; zc + z3*z2 + z3*z1
    ;; i.e. zc + z3*(z2+z1)
    (insert {:node0 [zc [:node1 (varbit cout 1)]]

             ;; zc=0
             :node1 [z3 [(varbit cout 0) :node2]]

             ;; zc=0 AND z3=1
             :node2 [z2 [:node3 (varbit cout 1)]]

             ;; zc=0 AND z3=1 AND z2=0
             :node3 [z1 [(varbit cout 0) (varbit cout 1)]]})

    ;; 
    ;; (insert (make-n-bit-adder nil [z3 z2 z1]
    ;;                           [(constant-0) cout cout]
    ;;                           [s3 s2 s1]
    ;;                           <nowhere>))

    ;; implement a 3-bit adder manually
    ;; i.e. if cout=1, then (z3..z1)+3 -> s3..s1
    (insert (make-half-adder     cout z1 s1 cc1))
    (insert (make-full-adder cc1 cout z2 s2 cc2))
    (insert (make-xor cc2 z3 s3))

    (simplify-graph (apply compose @graphs))))

(comment
  (make-bcd-adder nil
                  [:a3 :a2 :a1 :a0]
                  [:b3 :b2 :b1 :b0]
                  [:s3 :s2 :s1 :s0]
                  :cout)

  (select-keys
   (test-graph
    (make-bcd-adder nil
                    [:a3 :a2 :a1 :a0]
                    [:b3 :b2 :b1 :b0]
                    [:s3 :s2 :s1 :s0]
                    :cout)
    {:a3 0 :a2 0 :a1 1 :a0 1
     :b3 1 :b2 0 :b1 0 :b0 0})
   [:cout :s3 :s2 :s1 :s0])

  (graph->dot
   (make-bcd-adder nil
                   [:a3 :a2 :a1 :a0]
                   [:b3 :b2 :b1 :b0]
                   [:s3 :s2 :s1 :s0]
                   :cout))
  ;;
)



(comment
  (select-keys
   (test-graph
    (make-n-bit-adder nil
                      [:a3 :a2 :a1 :a0]
                      [:b3 :b2 :b1 :b0]
                      [:s3 :s2 :s1 :s0]
                      :cout)
    {:a3 0 :a2 0 :a1 1 :a0 1
     :b3 1 :b2 1 :b1 0 :b0 1})
   [:cout :s3 :s2 :s1 :s0]))


(defn solve-bdds* [node gen-node path-so-far var remaining-vars result-var truth-table]
  (let [on0 (truth-table (assoc path-so-far var 0))
        on1 (truth-table (assoc path-so-far var 1))

        node-on-0 (cond
                    (= on0 0) (varbit result-var 0)
                    (= on0 1) (varbit result-var 1)
                    (empty? remaining-vars) :UNDEFINED
                    :else     (gen-node))

        node-on-1 (cond
                    (= on1 0) (varbit result-var 0)
                    (= on1 1) (varbit result-var 1)
                    (empty? remaining-vars) :UNDEFINED
                    :else     (gen-node))

        bdds-on-0 (if (nil? on0)
                    (mapcat #(solve-bdds* node-on-0 gen-node (assoc path-so-far var 0) % (disj remaining-vars %) result-var truth-table)
                            remaining-vars)
                    [{}])


        bdds-on-1 (if (nil? on0)
                    (mapcat #(solve-bdds* node-on-1 gen-node (assoc path-so-far var 1) % (disj remaining-vars %) result-var truth-table)
                            remaining-vars)
                    [{}])]

    (for [bdd0 bdds-on-0
          bdd1 bdds-on-1]
      (merge {node [var [node-on-0 node-on-1]]}
             bdd0
             bdd1))))

(defn solve-bdds [vars result-var truth-table]
  (let [next-node-id (atom 0)
        gen-node (fn [] (keyword (str "n" (swap! next-node-id inc))))]
    (->> (mapcat #(solve-bdds* :root gen-node {} % (disj vars %) result-var truth-table)
                 vars)
         (map simplify-graph)
         (minimize count))))


#_(do
    (defn test-digit-graph [a b c d]
      (let [result-vars (test-graph digit-graph {:a a :b b :c c :d d})]
        (doseq [y (range digit-height)]
          (doseq [x (range digit-width)]
            (let [pos (+ (* y 4) x)
                  rvar (keyword (str "r" pos))
                  val (get result-vars rvar)]
              (print (if (= val 0) " " "x"))))
          (println))))

    (test-digit-graph 1 0 0 0)
    (test-digit-graph 0 1 0 0)
    (test-digit-graph 1 1 0 0)
    (test-digit-graph 0 0 1 0)
    (test-digit-graph 1 0 1 0)
    (test-digit-graph 0 1 1 0)
    (test-digit-graph 1 1 1 0)
    (test-digit-graph 0 0 0 1)
    (test-digit-graph 1 0 0 1))

(def THING_PLAYER1 0x001)
(def THING_PINKY 3002)
;; pinky has radius of 30, height of 56

(def THING_ZOMBIEMAN 3004)
;; zombieman has radius of 20, height of 56

(def THING_TELEPORTER 0x00E)
(def SPECIAL_DR_DOOR 1)
(def SPECIAL_WR_TELEPORT 97)
(def SPECIAL_S1_DOOR_STAY_OPEN_FAST 112)
(def LINE_FLAG_BLOCK_PLAYERS_AND_MONSTERS 1)
(def LINE_FLAG_BLOCK_MONSTERS 2)


(def MONSTER_THING THING_PINKY)
(def MONSTER_RADIUS 30)
(def MONSTER_HEIGHT 56)

;; for zombieman
;; (def DROOM_WIDTH 48)
;; (def TELE2_X_WIDTH 40)
;; (def TELE2_o_WIDTH 24)
;; (def DIGIT_PIXEL_WIDTH 48)
;; (def DIGIT_PIXEL_HEIGHT 48)

;; for pinky
(def DROOM_WIDTH 64)
(def TELE2_X_WIDTH 60)
(def TELE2_o_WIDTH 32)
(def DIGIT_PIXEL_WIDTH 60)
(def DIGIT_PIXEL_HEIGHT 48)

(def TELE2_TOTAL_WIDTH (+ TELE2_o_WIDTH 8 TELE2_X_WIDTH 8 TELE2_o_WIDTH))

;; Droom diagram:
;; _________
;; |  | |  |
;; |  |Y|  |
;; |__|_|__|
;;
;; Vertex positions:
;; C  D E  F
;;     M
;; B  A H  G
(defn draw-droom [droom-info {:keys [x y outer-sector
                                     make-door-sector
                                     floor-height ceil-height]}]
  (w/push-state)
  (let [floor-tex "MFLR8_1"
        ceil-tex "MFLR8_1"
        side-tex "BLAKWAL2"
        door-tex "SPCDOOR3"

        o-sector (w/create-sector {:floor-height floor-height
                                   :ceil-height ceil-height
                                   :floor-tex floor-tex
                                   :ceil-tex ceil-tex})
        Y-sector (make-door-sector (:Y droom-info))

        room-width DROOM_WIDTH
        Y-width 8

        ;; Offset the teleporter destination slightly. This is a hack to get the engine to behave.
        thing-y-offset 0

        ax (- (quot room-width 2) (quot Y-width 2))

        pos (fn [ox oy] [(+ x ox) (+ y oy)])

        A (pos ax 0)
        B (pos 0 0)
        C (pos 0 room-width)
        D (pos ax room-width)

        E (pos (+ ax Y-width) room-width)
        F (pos room-width room-width)
        G (pos room-width 0)
        H (pos (+ ax Y-width) 0)

        M (pos (quot room-width 2) (+ (quot room-width 2) thing-y-offset))]
    ;; Teleporter
    (w/add-thing {:x (first M)
                  :y (second M)
                  :angle 90
                  :type THING_TELEPORTER})

    ;; Left: [
    (w/set-back {:sector outer-sector
                 :upper-tex side-tex
                 :lower-tex side-tex})
    (w/set-front {:sector o-sector})
    (w/draw-poly A B C D)


    ;; Doors, left and right
    (w/set-back {:sector Y-sector})
    (w/set-front {:sector o-sector
                  :upper-tex door-tex})
    (w/set-line-special SPECIAL_DR_DOOR)
    (w/draw-poly D A)
    (w/draw-poly H E)
    (w/clear-line-special)

    ;; Doors, up and down
    (w/set-back {:sector outer-sector
                 :upper-tex side-tex
                 :lower-tex side-tex})
    (w/set-front {:sector Y-sector})
    (w/draw-poly D E)
    (w/draw-poly H A)

    ;; Right: ]
    (w/set-back {:sector outer-sector
                 :upper-tex side-tex
                 :lower-tex side-tex})
    (w/set-front {:sector o-sector})
    (w/draw-poly E F G H))
  (w/pop-state))

;; Tele2 Diagram:
;; ___________________
;; |  |  |     |  |  |
;; |o0|I0|  X  |I1|o1|
;; |__|__|_____|__|__|
;;
;; Vertex positions:
;; C  D  F     K  G  H
;;          M
;; B  A  E     L  J  I
(defn draw-tele2 [tele2-info {:keys [x y outer-sector
                                     make-door-sector
                                     floor-height ceil-height]}]
  (w/push-state)
  (let [floor-tex "MFLR8_1"
        ceil-tex "MFLR8_1"
        side-tex "BLAKWAL2"
        door-tex "SPCDOOR3"

        o-sector (w/create-sector {:floor-height floor-height
                                   :ceil-height ceil-height
                                   :floor-tex floor-tex
                                   :ceil-tex ceil-tex})
        o0-sector o-sector
        o1-sector o-sector
        i0-sector (make-door-sector (:I0 tele2-info))
        i1-sector (make-door-sector (:I1 tele2-info))
        X-sector (w/create-sector {:floor-height floor-height
                                   :ceil-height ceil-height
                                   :floor-tex floor-tex
                                   :ceil-tex ceil-tex
                                   :tag (:X tele2-info)})

        X-width TELE2_X_WIDTH
        X2-width (quot X-width 2)
        I-width 8
        o-width TELE2_o_WIDTH

        pos (fn [ox oy] [(+ x ox) (+ y oy)])
        A (pos o-width 0)
        B (pos 0 0)
        C (pos 0 X-width)
        D (pos o-width X-width)
        E (pos (+ o-width I-width) 0)
        F (pos (+ o-width I-width) X-width)

        G (pos (+ o-width I-width X-width I-width) X-width)
        H (pos (+ o-width I-width X-width I-width o-width) X-width)
        I (pos (+ o-width I-width X-width I-width o-width) 0)
        J (pos (+ o-width I-width X-width I-width) 0)
        K (pos (+ o-width I-width X-width) X-width)
        L (pos (+ o-width I-width X-width) 0)

        M (pos (+ o-width I-width X2-width) X2-width)]
    ;; Monster or teleporter
    (w/add-thing {:x (first M)
                  :y (second M)
                  :angle 90
                  :type (case (:item tele2-info)
                          :monster MONSTER_THING
                          :teleporter THING_TELEPORTER)})

    ;; Left o: [
    (w/set-back {:sector outer-sector
                 :upper-tex side-tex
                 :lower-tex side-tex})
    (w/set-front {:sector o0-sector})
    (w/draw-poly A B C D)

    (w/set-back {:sector o0-sector
                 :upper-tex door-tex})
    (w/set-front {:sector i0-sector})
    (w/set-line-tag (:o0 tele2-info))
    (w/set-line-special 97)
    (w/draw-poly A D)
    (w/clear-line-tag)
    (w/clear-line-special)

    (w/set-back {:sector outer-sector
                 :upper-tex side-tex
                 :lower-tex side-tex})
    (w/set-front {:sector i0-sector})
    (w/draw-poly E A)
    (w/draw-poly D F)

    (w/set-back {:sector i0-sector})
    (w/set-front {:sector X-sector
                  :upper-tex door-tex})
    (w/draw-poly E F)

    ;; Top and bottom
    (w/set-back {:sector outer-sector
                 :upper-tex side-tex
                 :lower-tex side-tex})
    (w/set-front {:sector X-sector})
    (w/draw-poly F K)
    (w/draw-poly L E)

    ;; Right o: ]
    (w/set-back {:sector outer-sector
                 :upper-tex side-tex
                 :lower-tex side-tex})
    (w/set-front {:sector o1-sector})
    (w/draw-poly G H I J)

    (w/set-back {:sector o1-sector
                 :upper-tex door-tex})
    (w/set-front {:sector i1-sector})
    (w/set-line-tag (:o1 tele2-info))
    (w/set-line-special SPECIAL_WR_TELEPORT)
    (w/draw-poly G J)
    (w/clear-line-tag)
    (w/clear-line-special)

    (w/set-back {:sector outer-sector
                 :upper-tex side-tex
                 :lower-tex side-tex})
    (w/set-front {:sector i1-sector})
    (w/draw-poly K G)
    (w/draw-poly J L)

    (w/set-back {:sector i1-sector})
    (w/set-front {:sector X-sector
                  :upper-tex door-tex})
    (w/draw-poly K L))
  (w/pop-state))

(defn draw-digit-display [wadtags {:keys [x y pixels-w pixels-h outer-sector base-floor-height ceil-height]}]
  (w/push-state)
  (let [floor-tex "CEIL4_1"
        ceil-tex "MFLR8_1"
        side-tex "STONE2"
        backdrop-tex "BLAKWAL2"

        increment-floor-by DIGIT_PIXEL_HEIGHT
        pixel-size DIGIT_PIXEL_WIDTH

        pos (fn [ix iy ox oy] [(+ x ox (* pixel-size ix))
                               (+ y oy (* pixel-size (- (- pixels-h 1) iy)))])

        last-row-sectors (atom (vec (repeat pixels-w outer-sector)))]
    
    (w/set-line-flags LINE_FLAG_BLOCK_PLAYERS_AND_MONSTERS)

    (doseq [iy (range pixels-h)]
      (let [floor-height (+ base-floor-height (* increment-floor-by (- (- pixels-h 1) iy)))
            row-sectors (mapv (fn [ix]
                                (let [i (+ (* iy pixels-w) ix)]
                                  (w/create-sector {:floor-height floor-height
                                                    :ceil-height ceil-height
                                                    :floor-tex floor-tex
                                                    :ceil-tex ceil-tex
                                                    :light 255
                                                    :tag (nth wadtags i)})))
                              (range pixels-w))]
        (doseq [ix (range pixels-w)]
          ;; draw "r" shapes (left and top lines)
          (let [A (pos ix iy 0 0)
                B (pos ix iy 0 pixel-size)
                C (pos ix iy pixel-size pixel-size)
                M (pos ix iy (quot pixel-size 2) (quot pixel-size 2))]
            (w/add-thing {:x (first M)
                          :y (second M)
                          :angle 90
                          :type THING_TELEPORTER})

            (w/set-front {:sector (nth row-sectors ix)
                          :lower-tex backdrop-tex})
            (w/set-back  {:sector (nth row-sectors (dec ix) outer-sector)
                          :lower-tex side-tex})
            (w/draw-poly A B)

            (w/set-back {:sector (nth @last-row-sectors ix)
                         :lower-tex side-tex})
            (w/draw-poly B C)))
        ;; draw right-most line for the row
        (let [A (pos pixels-w iy 0 0)
              B (pos pixels-w iy 0 pixel-size)]
          (w/set-front {:sector outer-sector
                        :lower-tex side-tex})
          (w/set-back {:sector (last row-sectors)})
          (w/draw-poly A B))
        (reset! last-row-sectors row-sectors)))

    ;; draw bottom-most lines for the last column
    (doseq [ix (range pixels-w)]
      (let [B (pos ix pixels-h 0 pixel-size)
            C (pos ix pixels-h pixel-size pixel-size)]
        (w/set-front {:sector outer-sector
                      :lower-tex side-tex})
        (w/set-back {:sector (nth @last-row-sectors ix)})
        (w/draw-poly B C))))
  (w/pop-state))

;; Vertex positions:
;; C               B
;; D  E .......... A
(defn draw-switches [wadtags {:keys [x y outer-sector base-floor-height ceil-height]}]
  (w/push-state)
  (let [outer-tex "STONE2"
        switch-tex "SW1EXIT"

        switches (count wadtags)

        switch-width 32
        switch-height 64
        switch-thick 8

        switch-sector (w/create-sector {:floor-height (+ base-floor-height switch-height)
                                        :ceil-height ceil-height
                                        :floor-tex "MFLR8_1"
                                        :ceil-tex "MFLR8_1"})

        pos (fn [ox oy] [(+ x ox) (+ y oy)])

        A (pos (* switch-width switches) 0)
        B (pos (* switch-width switches) switch-thick)
        ;; C (pos (+ switch-width) switch-thick)
        C (pos 0 switch-thick)
        D (pos 0 0)]
    (w/set-front {:sector outer-sector
                  :lower-tex outer-tex})
    (w/set-back {:sector switch-sector})
    (w/draw-poly A B C D)

    (w/set-front {:sector outer-sector
                  :lower-tex switch-tex})
    (w/set-line-special SPECIAL_S1_DOOR_STAY_OPEN_FAST)
    (doseq [i (range switches)]
      (w/set-line-tag (nth wadtags i))
      (w/draw-poly (pos (* switch-width i) 0)
                   (pos (* switch-width (inc i)) 0))))
  (w/pop-state))

(defn digit-graph-at-position [position a b c d r]
  (let [f (fn [digit] (let [v (get (get digits/digits digit) position)]
                        (if (= v '_) 0 1)))]
    (solve-bdds #{a b c d} r {{d 0 c 0 b 0 a 0} (f 0)
                              {d 0 c 0 b 0 a 1} (f 1)
                              {d 0 c 0 b 1 a 0} (f 2)
                              {d 0 c 0 b 1 a 1} (f 3)
                              {d 0 c 1 b 0 a 0} (f 4)
                              {d 0 c 1 b 0 a 1} (f 5)
                              {d 0 c 1 b 1 a 0} (f 6)
                              {d 0 c 1 b 1 a 1} (f 7)
                              {d 1 c 0 b 0 a 0} (f 8)
                              {d 1 c 0 b 0 a 1} (f 9)})))

(defn make-digit-input [[da01 da23 da45 da67 da89] [a3 a2 a1 a0]]
  {:n01-0 [da01 [(varbit a0 0) (varbit a0 1)]]
   :n01-1 [da01 [(varbit a1 0) (varbit a1 0)]]
   :n01-2 [da01 [(varbit a2 0) (varbit a2 0)]]
   :n01-3 [da01 [(varbit a3 0) (varbit a3 0)]]

   :n23-0 [da23 [(varbit a0 0) (varbit a0 1)]]
   :n23-1 [da23 [(varbit a1 1) (varbit a1 1)]]
   :n23-2 [da23 [(varbit a2 0) (varbit a2 0)]]
   :n23-3 [da23 [(varbit a3 0) (varbit a3 0)]]

   :n45-0 [da45 [(varbit a0 0) (varbit a0 1)]]
   :n45-1 [da45 [(varbit a1 0) (varbit a1 0)]]
   :n45-2 [da45 [(varbit a2 1) (varbit a2 1)]]
   :n45-3 [da45 [(varbit a3 0) (varbit a3 0)]]

   :n67-0 [da67 [(varbit a0 0) (varbit a0 1)]]
   :n67-1 [da67 [(varbit a1 1) (varbit a1 1)]]
   :n67-2 [da67 [(varbit a2 1) (varbit a2 1)]]
   :n67-3 [da67 [(varbit a3 0) (varbit a3 0)]]

   :n89-0 [da89 [(varbit a0 0) (varbit a0 1)]]
   :n89-1 [da89 [(varbit a1 0) (varbit a1 0)]]
   :n89-2 [da89 [(varbit a2 0) (varbit a2 0)]]
   :n89-3 [da89 [(varbit a3 1) (varbit a3 1)]]})

(defn digit-graphs [a b c d r-prefix]
  (for [pos (range digits/digit-positions)]
    (let [r-var (keyword (str r-prefix pos))]
      (digit-graph-at-position pos a b c d r-var))))


(defn graph->dot [graph]
  (println "digraph G {")
  (let [Q "\""
        f (memoize (fn [x]
                     (cond
                       (varbit? x)
                       (str Q "varbit_" (name (:var x)) "." (:bit x) Q)

                       (dnode? x)
                       (str Q (name x) Q))))
        flabel (memoize (fn [x]
                          (cond
                            (varbit? x)
                            (str Q (name (:var x)) "." (:bit x) Q)

                            (dnode? x)
                            (str Q (name x) Q))))

        encountered (atom #{})
        new? (fn [x]
               (let [[old _new] (swap-vals! encountered conj x)]
                 (not (contains? old x))))]
    (doseq [[node [var [on0 on1]]] graph]
      (when (and (varbit? node) (new? node))
        (println (str (f node) " [label=" (flabel node) ",shape=rectangle]")))
      (when-not (varbit? node)
        (println (str (f node) " [label=" (flabel var) "]"))

        (println (str (f node) " -> " (f on0) " [label=0,style=dashed]"))
        (println (str (f node) " -> " (f on1) " [label=1]"))


        (when (and (varbit? on0) (new? on0))
          (println (str (f on0) " [label=" (flabel on0) ",shape=rectangle]")))
        (when (and (varbit? on1) (new? on1))
          (println (str (f on1) " [label=" (flabel on1) ",shape=rectangle]"))))))
  (println "}"))

(comment
(graph->dot {:n1 [:a [(varbit :r 0) (varbit :r 0)]]
             (varbit :r 0) nil
             (varbit :s 0) nil})
)

(def digit-graph
  (with-new-genvar
    (apply compose
           (make-digit-input [:da01 :da23 :da45 :da67 :da89]
                             [:a3 :a2 :a1 :a0])
           (make-digit-input [:db01 :db23 :db45 :db67 :db89]
                             [:b3 :b2 :b1 :b0])
           (make-digit-input [:dc01 :dc23 :dc45 :dc67 :dc89]
                             [:c3 :c2 :c1 :c0])
           (make-digit-input [:dd01 :dd23 :dd45 :dd67 :dd89]
                             [:d3 :d2 :d1 :d0])
           (make-bcd-adder nil
                           [:b3 :b2 :b1 :b0]
                           [:d3 :d2 :d1 :d0]
                           [:s3 :s2 :s1 :s0]
                           :cout0)
           (make-bcd-adder :cout0
                           [:a3 :a2 :a1 :a0]
                           [:c3 :c2 :c1 :c0]
                           [:t3 :t2 :t1 :t0]
                           :cout1)
           (concat
            (digit-graphs :a0 :a1 :a2 :a3 "digit-a-")
            (digit-graphs :b0 :b1 :b2 :b3 "digit-b-")
            (digit-graphs :c0 :c1 :c2 :c3 "digit-c-")
            (digit-graphs :d0 :d1 :d2 :d3 "digit-d-")
            (digit-graphs :s0 :s1 :s2 :s3 "r-ones-")
            (digit-graphs :t0 :t1 :t2 :t3 "r-tens-")))))

(comment
  (count (simplify-graph digit-graph))
  (graph->dot digit-graph)
  (graph->dot {:root [:a [:b0 :b1]]
               :b0 [:b [:0 :c0]]
               :b1 [:b [:c0 :1]]
               :c0 [:c [:0 :1]]})
  (graph->dot (simplify-graph {:root [:a [:b0 :b1]], :b0 [:b [:0 :c0]], :b1 [:b [:c0 :1]], :c0 [:c [:0 :1]]}))

  (graph->dot (digit-graph-at-position 14 :a :b :c :d :r))

  (let [graph (apply compose
                     (make-bcd-adder nil
                                     [:b3 :b2 :b1 :b0]
                                     [:d3 :d2 :d1 :d0]
                                     [:s3 :s2 :s1 :s0]
                                     :cout0)
                     (make-bcd-adder :cout0
                                     [:a3 :a2 :a1 :a0]
                                     [:c3 :c2 :c1 :c0]
                                     [:t3 :t2 :t1 :t0]
                                     :cout1)
                     (concat
                      (digit-graphs :a0 :a1 :a2 :a3 "digit-a-")
                      (digit-graphs :b0 :b1 :b2 :b3 "digit-b-")
                      (digit-graphs :c0 :c1 :c2 :c3 "digit-c-")
                      (digit-graphs :d0 :d1 :d2 :d3 "digit-d-")
                      (digit-graphs :s0 :s1 :s2 :s3 "r-ones-")
                      (digit-graphs :t0 :t1 :t2 :t3 "r-tens-")))]
    (graph->dot graph))
  ;; => nil


  )


(defn generate-digit-input-varbits [start da01 da23 da45 da67 da89]
  {(varbit da01 0) (+ start 0)
   (varbit da01 1) (+ start 1)
   (varbit da23 0) (+ start 2)
   (varbit da23 1) (+ start 3)
   (varbit da45 0) (+ start 4)
   (varbit da45 1) (+ start 5)
   (varbit da67 0) (+ start 6)
   (varbit da67 1) (+ start 7)
   (varbit da89 0) (+ start 8)
   (varbit da89 1) (+ start 9)})

(defn generate-binary-input-varbits [start & vars]
  (into {}
        (mapcat (fn [i]
                  (let [var (nth vars i)]
                    [[(varbit var 0) (+ start (* i 2) 0)]
                     [(varbit var 1) (+ start (* i 2) 1)]])))
        (range (count vars))))

(def THE-VOID 32767)
(defn generate-digit-display-varbits [start prefix]
  (into
   {}
   (mapcat (fn [pos] [[(varbit (keyword (str prefix pos)) 0) THE-VOID]
                      [(varbit (keyword (str prefix pos)) 1) (+ start pos)]]))
   (range digits/digit-positions)))


(comment
  
(do
  (def parts
    (materialize-graph
     digit-graph
     #_(merge
        (generate-binary-input-varbits 10 :a3 :a2 :a1 :a0)
        (generate-binary-input-varbits 20 :b3 :b2 :b1 :b0)
        (generate-binary-input-varbits 30 :c3 :c2 :c1 :c0)
        (generate-binary-input-varbits 40 :d3 :d2 :d1 :d0))
     (merge
      (generate-digit-input-varbits 10 :da01 :da23 :da45 :da67 :da89)
      (generate-digit-input-varbits 20 :db01 :db23 :db45 :db67 :db89)
      (generate-digit-input-varbits 30 :dc01 :dc23 :dc45 :dc67 :dc89)
      (generate-digit-input-varbits 40 :dd01 :dd23 :dd45 :dd67 :dd89))

     (merge
      (generate-digit-display-varbits 50 "r-ones-")
      (generate-digit-display-varbits 80 "r-tens-")
      (generate-digit-display-varbits 200 "digit-a-")
      (generate-digit-display-varbits 230 "digit-b-")
      (generate-digit-display-varbits 260 "digit-c-")
      (generate-digit-display-varbits 290 "digit-d-"))

     ;; start new wadtags at
     500)))
  
  ;; put monster tele2s first
  (def parts 
    (let [partitioned (group-by #(and (-> % (first) (= :tele2))
                                    (-> % (second) :item (= :monster)))
                              parts)]
    (vec (concat (partitioned true) (partitioned false)))))

  (sort < [1 2 3 4 5])

  )

(defn draw-digit-switches [wadtags info]
  (draw-switches (take 5 wadtags) info)
  (draw-switches (drop 5 wadtags) (update info :x #(+ % (* 5 32) 8))))

(comment

  (w/with-new-wad-builder
    (let [interior-ceil-height 1200

          interior-sector (w/create-sector {:floor-height 0
                                            :ceil-height interior-ceil-height
                                            :floor-tex "MFLR8_1"
                                            :ceil-tex "MFLR8_1"
                                            :tag 0})

          make-door-sector (memoize (fn [wadtag]
                                      (w/create-sector {:floor-height 32
                                                        :ceil-height 32
                                                        :floor-tex "MFLR8_1"
                                                        :ceil-tex "MFLR8_1"
                                                        :tag wadtag})))

          tele2-counter (atom 0)
          droom-counter (atom 0)

          tele2-array-length 16
          tele2-spacing-x (+ TELE2_TOTAL_WIDTH 24)
          tele2-spacing-y (+ TELE2_X_WIDTH 24)

          droom-array-length 32
          droom-spacing-x (+ DROOM_WIDTH 16)
          droom-spacing-y (+ DROOM_WIDTH 16)

          tens-digit-x 224
          ones-digit-x 512
          digits-y-length 512]
      (w/set-front {:sector interior-sector
                    :middle-tex "STONE2"})
      (w/draw-poly [0 0] [0 7168] [7168 7168] [7168 0] [912 0] [912 1280] [896 1280] [896 0] [0 0])

      (w/add-thing {:x 512 :y 192 :angle 90
                    :type THING_PLAYER1})

      ;; A
      (draw-digit-display (range 200 (+ 200 28))
                          {:x tens-digit-x :y (+ 1152 digits-y-length digits-y-length)
                           :pixels-w 4 :pixels-h 7
                           :outer-sector interior-sector
                           :base-floor-height (+ 32 376 376)
                           :ceil-height interior-ceil-height})

      ;; B
      (draw-digit-display (range 230 (+ 230 28))
                          {:x ones-digit-x :y (+ 1152 digits-y-length digits-y-length)
                           :pixels-w 4 :pixels-h 7
                           :outer-sector interior-sector
                           :base-floor-height (+ 32 376 376)
                           :ceil-height interior-ceil-height})

      ;; C
      (draw-digit-display (range 260 (+ 260 28))
                          {:x tens-digit-x :y (+ 1152 digits-y-length)
                           :pixels-w 4 :pixels-h 7
                           :outer-sector interior-sector
                           :base-floor-height (+ 32 376)
                           :ceil-height interior-ceil-height})

      ;; D
      (draw-digit-display (range 290 (+ 290 28))
                          {:x ones-digit-x :y (+ 1152 digits-y-length)
                           :pixels-w 4 :pixels-h 7
                           :outer-sector interior-sector
                           :base-floor-height (+ 32 376)
                           :ceil-height interior-ceil-height})


      ;; tens
      (draw-digit-display (range 80 (+ 80 28))
                          {:x tens-digit-x :y 1152
                           :pixels-w 4 :pixels-h 7
                           :outer-sector interior-sector
                           :base-floor-height 32
                           :ceil-height interior-ceil-height})

      ;; ones
      (draw-digit-display (range 50 (+ 50 28))
                          {:x ones-digit-x :y 1152
                           :pixels-w 4 :pixels-h 7
                           :outer-sector interior-sector
                           :base-floor-height 32
                           :ceil-height interior-ceil-height})

      (draw-digit-switches (range 10 20) {:x 64 :y 448
                                          :outer-sector interior-sector
                                          :base-floor-height 0
                                          :ceil-height interior-ceil-height})
      (draw-digit-switches (range 20 30) {:x 448 :y 448
                                          :outer-sector interior-sector
                                          :base-floor-height 0
                                          :ceil-height interior-ceil-height})
      (draw-digit-switches (range 30 40) {:x 64 :y 320
                                          :outer-sector interior-sector
                                          :base-floor-height 0
                                          :ceil-height interior-ceil-height})
      (draw-digit-switches (range 40 50) {:x 448 :y 320
                                          :outer-sector interior-sector
                                          :base-floor-height 0
                                          :ceil-height interior-ceil-height})

      (doseq [[part-type info] parts]
        (case part-type
          :tele2 (let [i (swap! tele2-counter inc)]
                   (draw-tele2 info
                               {:x (+ 1024 (* (mod i tele2-array-length) tele2-spacing-x))
                                :y (+ 128 (* (quot i tele2-array-length) tele2-spacing-y))
                                :outer-sector interior-sector
                                :make-door-sector make-door-sector
                                :floor-height 32
                                :ceil-height 92}))
          :droom (let [i (swap! droom-counter inc)]
                   (draw-droom info
                               {:x (+ 1024 (* tele2-spacing-x tele2-array-length) (* (mod i droom-array-length) droom-spacing-x))
                                :y (+ 128 (* (quot i droom-array-length) droom-spacing-y))
                                :outer-sector interior-sector
                                :make-door-sector make-door-sector
                                :floor-height 32
                                :ceil-height 92})))))

    ;; (println (w/wad-data))

    (spit-pwad "out5.wad" (w/wad-data)))

  )
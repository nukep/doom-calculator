(ns doomcalc.machine-builder
  (:require [doomcalc.tree :as t :refer [mkvar]]
            [doomcalc.wad-builder :as w]
            [doomcalc.wad-constants :as wc]))

(def MONSTER_THING wc/THING_PINKY)
(def MONSTER_RADIUS wc/PINKY_RADIUS)

(defn make-level-parts [root-trees output-vars var->door-tag tree->tele-tag tree-has-tele-tag?]
  (let [root-trees (t/flatten-vectors root-trees)

        is-root? (set root-trees)
        is-output-var? (set output-vars)

        out (atom [])
        add (fn [type v] (swap! out conj [type v]))]
    (doseq [tree root-trees]
      (when (t/out? tree)
        (when (tree-has-tele-tag? tree)
          (add :const {:o (tree->tele-tag tree)}))))
    (t/traverse-trees-uniquely
     root-trees
     (fn
       ([t varfn varval outval]
        (when-not (is-output-var? varfn)
          (let [Y (tree->tele-tag t)]
            (add :droom {:Y Y}))))
       ([t varfn varval l r]
        (let [X (if (is-root? t) 0 (tree->tele-tag t))
              I0 (var->door-tag varfn 0)
              I1 (var->door-tag varfn 1)
              O0 (tree->tele-tag l)
              O1 (tree->tele-tag r)
              item (if (is-root? t) :monster :teleporter)]
          (add :tele2 {:X X, :I0 I0, :I1 I1, :o0 O0, :o1 O1, :item item})))))
    @out))

(defn- interpose-many-every-n [lv coll n]
  (apply concat (interpose lv (partition n coll))))

(defn- interpose-every-n [v coll n]
  (interpose-many-every-n [v] coll n))

(defn- round-up-to-nearest-even [x]
  (if (= 0 (mod x 2))
    x
    (inc x)))

(defn get-tessellated-machines-dimensions
  [consts tele2s drooms]
  (let [tc-count (+ (quot (round-up-to-nearest-even (count consts)) 2) (count tele2s))
        total-count (max tc-count (count drooms))
        cols (int (Math/ceil (Math/sqrt tc-count)))
        rows (int (Math/ceil (/ total-count cols)))]
    {:rows rows
     :cols cols
     :width (* cols 128)
     :height (* rows 128)}))

(defn draw-tessellated-machines
  "To minimize the amount of lines drawn, we will tessellate the machines.
   We will tessellate the 'tele2' parts as L-shaped triominoes.
   'droom' parts will fit in any gaps, or where those gaps would go.
   
   Example:
   0 _ 0 _ 0 _
   X 1 X 1 X 1
   0 _ 0 _ 0 _
   X 1 X 1 X 1
   0 _ 0 _ 0 _
   X 1 X 1 X 1
   
   (where X is the monster or teleport destination, 0 and 1 are doors, and _ is a potential droom)
   "
  [consts tele2s drooms outer-sector make-door-sector]
  (let [floor-height 32
        ceil-height 92
        floor-tex "MFLR8_1"
        ceil-tex "MFLR8_1"
        side-tex "BLAKWAL2"
        door-tex "SPCDOOR3"

        {:keys [rows cols]} (get-tessellated-machines-dimensions consts tele2s drooms)
        consts-count-half (quot (round-up-to-nearest-even (count consts)) 2)

        squares
        (for [i (range (* rows 2))]
          (for [j (range (* cols 2))]
            (let [even-i? (= 0 (mod i 2))
                  even-j? (= 0 (mod j 2))
                  n (+ (quot j 2) (* cols (quot i 2)))
                  const-1 (get consts (* n 2))
                  const-2 (get consts (inc (* n 2)))
                  const (or const-1 const-2)
                  tele2 (get tele2s (- n consts-count-half))
                  droom (get drooms n)]
              (cond
                ;; X
                (and even-i? even-j?)
                (cond
                  tele2
                  {:sector (w/create-sector {:floor-height floor-height
                                             :ceil-height ceil-height
                                             :floor-tex floor-tex
                                             :ceil-tex ceil-tex
                                             :tag (:X tele2)})
                   :draw (fn [] (w/add-thing {:angle 90
                                              :type (case (:item tele2)
                                                      :monster MONSTER_THING
                                                      :teleporter wc/THING_TELEPORTER)}))
                   :t {:tag (:o0 tele2) :special wc/SPECIAL_WR_TELEPORT :upper-tex door-tex}
                   :r {:tag (:o1 tele2) :special wc/SPECIAL_WR_TELEPORT :upper-tex door-tex}
                   :b {}
                   :l {}}

                  const
                  {:sector (w/create-sector {:floor-height floor-height
                                             :ceil-height ceil-height
                                             :floor-tex floor-tex
                                             :ceil-tex ceil-tex})})

                ;; 0
                (and (not even-i?) even-j?)
                (cond
                  tele2
                  {:sector (make-door-sector (:I0 tele2))}

                  const-1
                  {:sector (w/create-sector {:floor-height floor-height
                                             :ceil-height ceil-height
                                             :floor-tex floor-tex
                                             :ceil-tex ceil-tex})
                   :draw (fn [] (w/add-thing {:angle 270 :type MONSTER_THING}))
                   :b {:tag (:o const-1) :special wc/SPECIAL_WR_TELEPORT}})

                ;; 1
                (and even-i? (not even-j?))
                (cond
                  tele2
                  {:sector (make-door-sector (:I1 tele2))}

                  const-2
                  {:sector (w/create-sector {:floor-height floor-height
                                             :ceil-height ceil-height
                                             :floor-tex floor-tex
                                             :ceil-tex ceil-tex})
                   :draw (fn [] (w/add-thing {:angle 180 :type MONSTER_THING}))
                   :l {:tag (:o const-2) :special wc/SPECIAL_WR_TELEPORT}})

                ;; droom
                :else
                (when droom
                  (let [droom-sector (w/create-sector {:floor-height 80
                                                       :ceil-height ceil-height
                                                       :floor-tex floor-tex
                                                       :ceil-tex ceil-tex})
                        door-sector (make-door-sector (:Y droom))
                        szh 8
                        szw 8
                        nszw (- szw)
                        nszh (- szh)

                        A [nszw nszh]
                        B [nszw szh]
                        C [szw szh]
                        D [szw nszh]]
                    {:sector droom-sector
                     :draw (fn []
                             ;; draw a door that monsters can open.
                             ;; only one linedef needs to have the special (and it's better that it's only one, to avoid spechit overruns in vanilla doom)
                             (w/draw-poly-ex {:front {:sector door-sector}
                                              :back {:sector droom-sector :upper-tex door-tex :lower-tex door-tex :special wc/SPECIAL_DR_DOOR}}
                                             A B)
                             (w/draw-poly-ex {:front {:sector door-sector}
                                              :back {:sector droom-sector :upper-tex door-tex :lower-tex door-tex}}
                                             B C D A)
                             (w/add-thing {:angle 90 :type wc/THING_TELEPORTER}))
                     :t {}
                     :r {}
                     :b {}
                     :l {}}))))))

        ;; add spaces for debugging and demonstration purposes
        add-spaces? true

        squares (if add-spaces?
                  (mapv (fn [v] (vec (interpose-many-every-n [nil] v 2))) squares)
                  squares)
        squares (if add-spaces?
                  (vec (interpose-many-every-n [[]] squares 2))
                  squares)

        sizef (if add-spaces?
                (fn [i] (let [m (mod i 3)]
                          (case m
                            0 64
                            1 60
                            2 4)))
                64)]
    (w/draw-square-lattice squares
                           {:sector outer-sector
                            :upper-tex side-tex
                            :lower-tex side-tex}
                           sizef sizef)))

(defn pop-v [queue path]
  (let [[x & xs] (get-in queue path)
        queue (assoc-in queue path (vec xs))]
    [queue x]))

(defn conjv [coll v] (conj (or coll []) v))

(defn push-v [queue path newvar]
  (update-in queue path conjv newvar))

(defn take-queue-pseudovar [queue var v]
  (let [[queue pseudovar] (pop-v queue [var v])]
    (if pseudovar
      [queue pseudovar]

      (let [newvar (mkvar)
            queue (-> queue
                      (push-v [var 0] newvar)
                      (push-v [var 1] newvar))]
        (pop-v queue [var v])))))

(defn coll-has-value? [coll x]
  (cond
    (set? coll) (contains? coll x)
    :else (or (some #(= x %) coll) false)))

(defn conj-unique [coll x]
  (if (coll-has-value? coll x)
    coll
    (conj coll x)))

(defn compile-machines [level-fn]
  (let [wadtag-start 1
        wadtag-counter (atom (dec wadtag-start))
        new-wadtag (fn [] (swap! wadtag-counter inc))

        pseudo-out-trees (atom [])
        pseudo-out-vars (atom [])

        pseudo-out-queue (atom {})
        tree-has-tele-tag? (atom #{})
        tree->tele-tag (memoize (fn [tree]
                                  (swap! tree-has-tele-tag? conj tree)
                                  (new-wadtag)))

        pseudo-out-tag (fn [varr v]
                         (let [[queue pseudovar] (take-queue-pseudovar @pseudo-out-queue varr v)]
                           (reset! pseudo-out-queue queue)
                           (swap! pseudo-out-trees conj-unique (varr (pseudovar 0) (pseudovar 1)))
                           (swap! pseudo-out-vars conj-unique pseudovar)
                           (tree->tele-tag (pseudovar v))))

        var->door-tag (fn [varr v] (tree->tele-tag (varr v)))

        var->tele-tag  (fn [varr v] (pseudo-out-tag varr v))]
    (let [interior-ceil-height 400

          main-room-sector
          (w/create-sector {:floor-height 0
                            :ceil-height interior-ceil-height
                            :floor-tex "MFLR8_1"
                            :ceil-tex "MFLR8_1"
                            :tag 0})

          machines-room-sector
          (w/create-sector {:floor-height 0
                            :ceil-height interior-ceil-height
                            :floor-tex "MFLR8_1"
                            :ceil-tex "MFLR8_1"
                            :tag 0})

          components (level-fn)]

      (doseq [draw (map :draw components)]
        (when draw
          (w/with-pushpop-state
            (draw var->door-tag var->tele-tag {:outer-sector main-room-sector :floor-height 0 :ceil-height interior-ceil-height}))))

      (let [trees (mapv :trees components)
            trees [trees @pseudo-out-trees]
            trees (t/simplify-trees trees)
            trees (t/prune-unreachable-trees trees @pseudo-out-vars)
            parts (make-level-parts trees @pseudo-out-vars var->door-tag tree->tele-tag @tree-has-tele-tag?)

            consts (into [] (comp (filter #(= (first %) :const)) (map second)) parts)
            tele2s (into [] (comp (filter #(= (first %) :tele2)) (map second)) parts)
            drooms (into [] (comp (filter #(= (first %) :droom)) (map second)) parts)

            make-door-sector (memoize (fn [tag]
                                        (w/create-sector {:floor-height 32
                                                          :ceil-height 32
                                                          :floor-tex "MFLR8_1"
                                                          :ceil-tex "MFLR8_1"
                                                          :tag tag})))

            mres (get-tessellated-machines-dimensions consts tele2s drooms)]
        (w/with-pushpop-state
          (w/translate -256 256)
          (w/translate (- (:width mres)) 0)
          (draw-tessellated-machines consts
                                     tele2s
                                     drooms
                                     machines-room-sector
                                     make-door-sector))

        ;; draw the room perimeter after we know the dimensions of what's inside of it
        (let [main-w 4500 main-h 2048 machines-w (+ (:width mres) 256 256) machines-h (+ (:height mres) 256 256) gap-w 8 gap-h 8
              door-w 128 door-overhang 64
              door-overhang-sector (w/create-sector {:floor-height 0
                                                     :ceil-height 128
                                                     :floor-tex "MFLR8_1"
                                                     :ceil-tex "MFLR8_1"
                                                     :tag 0})
              door-sector  (w/create-sector {:floor-height 0 :ceil-height 0})]
          (w/draw-poly-ex
           {:front {:sector main-room-sector :middle-tex "STONE2"}}
           [0 (+ gap-h door-w)] [0 main-h] [main-w main-h] [main-w 0]
           [door-overhang 0])
          (w/draw-poly-ex
           {:front {:sector machines-room-sector :middle-tex "STONE2"}}
           [(- door-overhang) 0] [(- gap-w machines-w) 0] [(- gap-w machines-w) machines-h] [(- gap-w) machines-h] [(- gap-w) (+ gap-h door-w)])

          ;; right overhang
          (w/draw-poly-ex
           {:front {:sector main-room-sector :upper-tex "STONE2"}
            :back {:sector door-overhang-sector}}
           [door-overhang 0] [door-overhang (+ gap-h door-w)] [0 (+ gap-h door-w)])
          ;; left overhang
          (w/draw-poly-ex
           {:front {:sector machines-room-sector :upper-tex "STONE2"}
            :back {:sector door-overhang-sector}}
           [(- gap-w) (+ gap-h door-w)] [(- door-overhang) (+ gap-h door-w)] [(- door-overhang) 0])
          (w/draw-poly-ex
           {:front {:sector door-overhang-sector :middle-tex "STONE2"}}
           [door-overhang 0] [(- door-overhang) 0])
          ;; door
          (w/draw-poly-ex
           {:front {:sector door-overhang-sector :upper-tex "BIGDOOR2" :special wc/SPECIAL_DR_DOOR}
            :back {:sector door-sector}}
           [(- gap-w) (+ gap-h door-w)] [(- gap-w) gap-h] [0 gap-h] [0 (+ gap-h door-w)])
          (w/draw-poly-ex
           {:front {:sector door-sector :middle-tex "DOORTRAK" :flags wc/ML_DONTPEGBOTTOM}}
           [(- gap-w) (+ gap-h door-w)] [0 (+ gap-h door-w)]))))))

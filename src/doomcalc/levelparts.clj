(ns doomcalc.levelparts
  (:require [doomcalc.logic :as l]
            [doomcalc.tree :as t :refer [mkvar]]
            [doomcalc.wad-builder :as w]
            [doomcalc.digits :as digits]))


(def THING_PLAYER1 0x001)
(def THING_PINKY 3002)
(def MONSTER_THING THING_PINKY)
;; pinky has radius of 30, height of 56

(def THING_ZOMBIEMAN 3004)
;; zombieman has radius of 20, height of 56

(def THING_TELEPORTER 0x00E)
(def SPECIAL_DR_DOOR 1)
(def SPECIAL_WR_LIFT_ALSO_MONSTERS 88)
(def SPECIAL_WR_TELEPORT 97)
(def SPECIAL_S1_DOOR_STAY_OPEN_FAST 112)
(def LINE_FLAG_BLOCK_PLAYERS_AND_MONSTERS 1)
(def LINE_FLAG_BLOCK_MONSTERS 2)



(def DIGIT_PIXEL_WIDTH 60)
(def DIGIT_PIXEL_HEIGHT 48)

;; A Component couples drawing and logic.
;; I found that passing lists of variables around like hot potato
;; from tree-building procedures to drawing procedures was discouraging and confusing.

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

(defn digit-input [{:keys [x y]}]
  (let [mkvar-i (fn [] (mkvar nil :optimize-same-left-right? false))
        da01 (mkvar-i) da23 (mkvar-i) da45 (mkvar-i) da67 (mkvar-i) da89 (mkvar-i)
        b3 (mkvar) b2 (mkvar) b1 (mkvar) b0 (mkvar)

        tree (l/make-digit-input [da01 da23 da45 da67 da89]
                                 [b3 b2 b1 b0])]

    {:trees [tree]
     :vars [b3 b2 b1 b0]
     :draw (fn [var->door-tag var->tele-tag {:keys [outer-sector floor-height ceil-height]}]
             (let [tags [(var->door-tag da01 0) (var->door-tag da01 1) (var->door-tag da23 0) (var->door-tag da23 1)
                         (var->door-tag da45 0) (var->door-tag da45 1) (var->door-tag da67 0) (var->door-tag da67 1)
                         (var->door-tag da89 0) (var->door-tag da89 1)]
                   x2 (+ x (* 5 32) 8)]
               ;; draw 2 sets of switch arrays: 0 to 4, and 5 to 9
               (draw-switches (take 5 tags) {:x x,  :y y, :outer-sector outer-sector, :base-floor-height floor-height, :ceil-height ceil-height})
               (draw-switches (drop 5 tags) {:x x2, :y y, :outer-sector outer-sector, :base-floor-height floor-height, :ceil-height ceil-height})))}))

(defn binary-4-input [{:keys [x y]}]
  (let [b3 (mkvar) b2 (mkvar) b1 (mkvar) b0 (mkvar)]
    {:trees []
     :vars [b3 b2 b1 b0]
     :draw (fn [var->door-tag var->tele-tag {:keys [outer-sector floor-height ceil-height]}]
             (let [ds (fn [v offx] (draw-switches [(var->door-tag v 0) (var->door-tag v 1)]
                                                  {:x (+ x offx), :y y, :outer-sector outer-sector, :base-floor-height floor-height, :ceil-height ceil-height}))]
               (ds b3 0)
               (ds b2 80)
               (ds b1 160)
               (ds b0 240)))}))

(defn binary-sequence-4
  "Returns a sequence of the number paired with 4 binary digits in big-endian order:
   [0 [0 0 0 0]], [1 [0 0 0 1]], [2 [0 0 1 0]] ... [15 [1 1 1 1]]"
  []
  (map-indexed vector
               (for [b3 (range 2) b2 (range 2) b1 (range 2) b0 (range 2)]
                 [b3 b2 b1 b0])))

(defn make-base10-digit-at-position [position b3 b2 b1 b0 r]
  (let [f (fn [digit] (let [v (get (get digits/digits digit) position)]
                        (if (= v '_) 0 1)))]
    (l/solve-truth-table [b3 b2 b1 b0] r
                         (into {}
                               (comp (take 10) (map (fn [[i x]] [x (f i)])))
                               (binary-sequence-4)))))

(defn make-base16-digit-at-position [position b3 b2 b1 b0 r]
  (let [f (fn [digit] (let [v (get (get digits/digits digit) position)]
                        (if (= v '_) 0 1)))]
    (l/solve-truth-table [b3 b2 b1 b0] r
                         (into {}
                               (comp (take 16) (map (fn [[i x]] [x (f i)])))
                               (binary-sequence-4)))))

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

(defn digit-display [{:keys [x y bits base-floor-height]}]
  (let [[b3 b2 b1 b0] bits
        output-vars (mapv (fn [_] (mkvar)) (range digits/digit-positions))
        trees (vec
               (for [position (range digits/digit-positions)]
                 (make-base10-digit-at-position position b3 b2 b1 b0 (nth output-vars position))))]

    {:trees trees
     :vars {}
     :draw (fn [var->door-tag var->tele-tag {:keys [outer-sector floor-height ceil-height]}]
             (let [tags (mapv #(var->tele-tag % 1) output-vars)]
               (draw-digit-display tags {:x x :y y :pixels-w digits/digit-width :pixels-h digits/digit-height
                                         :outer-sector outer-sector
                                         :base-floor-height (+ floor-height base-floor-height)
                                         :ceil-height ceil-height})))}))


(defn player [x y angle]
  {:draw (fn [_ _ _]
           (w/add-thing {:x x :y y :angle angle
                         :type THING_PLAYER1}))})


;; Vertex positions:
;; F    E     D
;;   M0    M1
;; A    B     C
(defn variable-display
  "Debugging tool to show a variable result"
  [{:keys [x y v base-floor-height]
    :or {base-floor-height 0}}]
  (let []
    {:draw (fn [var->door-tag var->tele-tag {:keys [outer-sector floor-height ceil-height]}]
             (w/push-state)
             (w/set-line-flags LINE_FLAG_BLOCK_PLAYERS_AND_MONSTERS)
             (let [floor-tex "CEIL4_1"
                   ceil-tex "MFLR8_1"
                   side-tex "STONE2"
                   pos (fn [ox oy] [(+ x ox) (+ y oy)])
                   pixel-size DIGIT_PIXEL_WIDTH

                   inner-sector-0
                   (w/create-sector {:floor-height (+ floor-height base-floor-height)
                                     :ceil-height ceil-height
                                     :floor-tex floor-tex
                                     :ceil-tex ceil-tex
                                     :light 255
                                     :tag (var->tele-tag v 0)})
                   inner-sector-1
                   (w/create-sector {:floor-height (+ floor-height base-floor-height)
                                     :ceil-height ceil-height
                                     :floor-tex floor-tex
                                     :ceil-tex ceil-tex
                                     :light 255
                                     :tag (var->tele-tag v 1)})

                   A (pos 0 0)
                   B (pos pixel-size 0)
                   C (pos (* pixel-size 2) 0)
                   D (pos (* pixel-size 2) pixel-size)
                   E (pos pixel-size pixel-size)
                   F (pos 0 pixel-size)
                   M0 (pos (quot pixel-size 2) (quot pixel-size 2))
                   M1 (pos (+ pixel-size (quot pixel-size 2)) (quot pixel-size 2))]



               (w/add-thing {:x (first M0) :y (second M0) :angle 90 :type THING_TELEPORTER})
               (w/add-thing {:x (first M1) :y (second M1) :angle 90 :type THING_TELEPORTER})

               (w/set-front {:sector outer-sector :lower-tex side-tex})
               (w/set-back  {:sector inner-sector-0})
               (w/draw-poly E F A B)
               (w/set-back  {:sector inner-sector-1})
               (w/draw-poly B C D E)
               (w/set-front {:sector inner-sector-0})
               (w/draw-poly E B))
             (w/pop-state))}))

;; 1. draw everything. this also keeps track of output tags in the process.

;; 2. draw the tele2 and drooms.

(defn make-level-parts [root-trees output-vars var->door-tag tree->tele-tag]
  (let [root-trees (t/flatten-vectors root-trees)

        is-root? (set root-trees)
        is-output-var? (set output-vars)

        out (atom [])
        add (fn [type v] (swap! out conj [type v]))]
    (t/traverse-trees-uniquely
     root-trees
     (fn
       ([t varfn varval outval]
        (let [Y (tree->tele-tag t)]
          (when-not (is-output-var? varfn)
            (add :droom {:Y Y}))))
       ([t varfn varval l r]
        (let [X (tree->tele-tag t)
              I0 (var->door-tag varfn 0)
              I1 (var->door-tag varfn 1)
              O0 (tree->tele-tag l)
              O1 (tree->tele-tag r)
              item (if (is-root? t) :monster :teleporter)]
          (add :tele2 {:X X, :I0 I0, :I1 I1, :o0 O0, :o1 O1, :item item})))))
    @out))


;; for pinky
(def DROOM_WIDTH 64)
(def TELE2_X_WIDTH 60)
(def TELE2_I_WIDTH 40)
(def DIGIT_PIXEL_WIDTH 60)
(def DIGIT_PIXEL_HEIGHT 48)

;; (def TELE2_o_WIDTH 32)

(def TELE2_TOTAL_WIDTH (+ TELE2_I_WIDTH TELE2_X_WIDTH TELE2_I_WIDTH))
;; (def TELE2_TOTAL_WIDTH (+ TELE2_o_WIDTH 8 TELE2_X_WIDTH 8 TELE2_o_WIDTH))

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
;; _____________
;; |  |     |  |
;; |I0|  X  |I1|
;; |__|_____|__|
;;
;; Vertex positions:
;; C     D     K     H
;;          M
;; B     A     L     I

(defn draw-tele2 [tele2-info {:keys [x y outer-sector
                                     make-door-sector
                                     floor-height ceil-height]}]
  (w/push-state)
  (let [floor-tex "MFLR8_1"
        ceil-tex "MFLR8_1"
        side-tex "BLAKWAL2"
        door-tex "SPCDOOR3"

        i0-sector (make-door-sector (:I0 tele2-info))
        i1-sector (make-door-sector (:I1 tele2-info))
        X-sector (w/create-sector {:floor-height floor-height
                                   :ceil-height ceil-height
                                   :floor-tex floor-tex
                                   :ceil-tex ceil-tex
                                   :tag (:X tele2-info)})

        X-width TELE2_X_WIDTH
        X2-width (quot X-width 2)

        TT-width TELE2_I_WIDTH

        pos (fn [ox oy] [(+ x ox) (+ y oy)])
        B (pos 0 0)
        A (pos (+ TT-width) 0)
        C (pos 0 X-width)
        D (pos (+ TT-width) X-width)

        H (pos (+ TT-width X-width TT-width) X-width)
        I (pos (+ TT-width X-width TT-width) 0)
        K (pos (+ TT-width X-width) X-width)
        L (pos (+ TT-width X-width) 0)

        M (pos (+ TT-width X2-width) X2-width)]
    ;; Monster or teleporter
    (w/add-thing {:x (first M)
                  :y (second M)
                  :angle 90
                  :type (case (:item tele2-info)
                          :monster MONSTER_THING
                          :teleporter THING_TELEPORTER)})


    ;; Left
    (w/set-back {:sector outer-sector
                 :upper-tex side-tex
                 :lower-tex side-tex})
    (w/set-front {:sector i0-sector})
    (w/draw-poly A B C D)

    (w/set-back {:sector i0-sector
                 :upper-tex door-tex})
    (w/set-front {:sector X-sector
                  :upper-tex door-tex})
    (w/set-line-tag (:o0 tele2-info))
    (w/set-line-special SPECIAL_WR_TELEPORT)
    (w/draw-poly A D)
    (w/clear-line-tag)
    (w/clear-line-special)

    ;; Top and bottom
    (w/set-back {:sector outer-sector
                 :upper-tex side-tex
                 :lower-tex side-tex})
    (w/set-front {:sector X-sector})
    (w/draw-poly D K)
    (w/draw-poly L A)

    ;; Right
    (w/set-back {:sector outer-sector
                 :upper-tex side-tex
                 :lower-tex side-tex})
    (w/set-front {:sector i1-sector})
    (w/draw-poly K H I L)

    (w/set-back {:sector i1-sector
                 :upper-tex door-tex})
    (w/set-front {:sector X-sector
                  :upper-tex door-tex})
    (w/set-line-tag (:o1 tele2-info))
    (w/set-line-special SPECIAL_WR_TELEPORT)
    (w/draw-poly K L)
    (w/clear-line-tag)
    (w/clear-line-special))
  (w/pop-state))


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

(defn compile-level [level-fn]
  (let [wadtag-start 1
        wadtag-counter (atom (dec wadtag-start))
        new-wadtag (fn [] (swap! wadtag-counter inc))

        pseudo-out-trees (atom [])
        pseudo-out-vars (atom [])

        pseudo-out-queue (atom {})
        tree->tele-tag (memoize (fn [tree] (new-wadtag)))

        pseudo-out-tag (fn [varr v]
                         (let [[queue pseudovar] (take-queue-pseudovar @pseudo-out-queue varr v)]
                           (reset! pseudo-out-queue queue)
                           (swap! pseudo-out-trees conj-unique (varr (pseudovar 0) (pseudovar 1)))
                           (swap! pseudo-out-vars conj-unique pseudovar)
                           (tree->tele-tag (pseudovar v))))

        var->door-tag (fn [varr v] (tree->tele-tag (varr v)))

        var->tele-tag  (fn [varr v] (pseudo-out-tag varr v))]
    (let [interior-ceil-height 400

          interior-sector (w/create-sector {:floor-height 0
                                            :ceil-height interior-ceil-height
                                            :floor-tex "MFLR8_1"
                                            :ceil-tex "MFLR8_1"
                                            :tag 0})

          components (level-fn)]

      (w/set-front {:sector interior-sector
                    :middle-tex "STONE2"})
      (let [main-w 4500 main-h 2048 machines-w 6000 machines-h 7500 gap-w 8 gap-h 8]
        (w/draw-poly [0 gap-h] [0 main-h] [main-w main-h] [main-w 0] [0 0]
                     [(- gap-w) 0] [(- gap-w machines-w) 0] [(- gap-w machines-w) machines-h] [(- gap-w) machines-h] [(- gap-w) gap-h]
                     [0 gap-h]))

      (doseq [draw (map :draw components)]
        (when draw
          (w/push-state)
          (draw var->door-tag var->tele-tag {:outer-sector interior-sector :floor-height 0 :ceil-height interior-ceil-height})
          (w/pop-state)))

      (let [trees (mapv :trees components)
            trees [trees @pseudo-out-trees]
            trees (t/simplify-trees trees)
            trees (t/prune-unreachable-trees trees @pseudo-out-vars)
            parts (make-level-parts trees @pseudo-out-vars var->door-tag tree->tele-tag)

            make-door-sector (memoize (fn [tag]
                                        (w/create-sector {:floor-height 32
                                                          :ceil-height 32
                                                          :floor-tex "MFLR8_1"
                                                          :ceil-tex "MFLR8_1"
                                                          :tag tag})))
            tele2-counter (atom -1)
            droom-counter (atom -1)

            tele2-array-length 16
            tele2-spacing-x (+ TELE2_TOTAL_WIDTH 24)
            tele2-spacing-y (+ TELE2_X_WIDTH 24)
            droom-array-length 32
            droom-spacing-x (+ DROOM_WIDTH 16)
            droom-spacing-y (+ DROOM_WIDTH 16)]
        (doseq [[part-type info] parts]
          (case part-type
            :tele2 (let [i (swap! tele2-counter inc)]
                     (draw-tele2 info
                                 {:x (+ -3000 (* (mod i tele2-array-length) tele2-spacing-x))
                                  :y (+ 64 (* (quot i tele2-array-length) tele2-spacing-y))
                                  :outer-sector interior-sector
                                  :make-door-sector make-door-sector
                                  :floor-height 32
                                  :ceil-height 92}))
            :droom (let [i (swap! droom-counter inc)]
                     (draw-droom info
                                 {:x (+ -8200 (* tele2-spacing-x tele2-array-length) (* (mod i droom-array-length) droom-spacing-x))
                                  :y (+ 64 (* (quot i droom-array-length) droom-spacing-y))
                                  :outer-sector interior-sector
                                  :make-door-sector make-door-sector
                                  :floor-height 32
                                  :ceil-height 92}))))))))

(defn bcd-adding-machine
  "An adding machine component.
   Inputs are lists of 4-item vectors."
  [bits-a bits-b]
  (let [bits-a (reverse bits-a)
        bits-b (reverse bits-b)]
    (loop [trees []
           vars []
           cin nil
           [in-a & bits-a] bits-a
           [in-b & bits-b] bits-b]
      (if (and in-a in-b)
        (let [cout (mkvar)
              out [(mkvar) (mkvar) (mkvar) (mkvar)]
              trees (conj trees
                          (l/make-bcd-adder cin
                                            in-a
                                            in-b
                                            out
                                            cout))
              vars (conj vars out)]
          (recur trees vars cout bits-a bits-b))

        ;; we're done!
        {:trees trees
         :vars {:carry cin
                :sum (vec (reverse vars))}}))))

(defn digit-input-and-display [{:keys [x y]}]
  (let [di (digit-input {:x x :y y})
        dd (digit-display {:x x :y (+ y 64)
                           :bits (:vars di)
                           :base-floor-height 0})]
    {:trees [(:trees di) (:trees dd)]
     :vars (:vars di)
     :draw (fn [var->door-tag var->tele-tag outer]
             ((:draw di) var->door-tag var->tele-tag outer)
             ((:draw dd) var->door-tag var->tele-tag outer))}))

(defn level []
  (let [di1 (digit-input-and-display {:x 100 :y 320})
        di2 (digit-input-and-display {:x 500 :y 320})
        di3 (digit-input-and-display {:x 900 :y 320})
        di4 (digit-input-and-display {:x 1300 :y 320})
        di5 (digit-input-and-display {:x 1700 :y 320})
        di6 (digit-input-and-display {:x 2100 :y 320})

        addm (bcd-adding-machine [(:vars di1) (:vars di2) (:vars di3)]
                                 [(:vars di4) (:vars di5) (:vars di6)])]
    [(player 512 192 90)
     di1 di2 di3 di4 di5 di6
     addm
     (digit-display {:x 3000 :y 320
                     :bits (-> addm :vars :sum (nth 0))
                     :base-floor-height 0})
     (digit-display {:x 3400 :y 320
                     :bits (-> addm :vars :sum (nth 1))
                     :base-floor-height 0})
     (digit-display {:x 3800 :y 320
                     :bits (-> addm :vars :sum (nth 2))
                     :base-floor-height 0})]))

(defn testlevel []
  (let [d1 (binary-4-input {:x 64 :y 200})]
    [(player 512 192 90)
     d1
     (variable-display {:x 550 :y 512 :v (-> d1 :vars (nth 0))})
     (variable-display {:x 700 :y 512 :v (-> d1 :vars (nth 1))})
     (variable-display {:x 900 :y 512 :v (-> d1 :vars (nth 2))})
     (variable-display {:x 1050 :y 512 :v (-> d1 :vars (nth 3))})]))

(comment
  (w/with-debug-svg
    (compile-level testlevel))


  (w/with-new-wad-builder
    (compile-level testlevel)
    (doomcalc.write-pwad/spit-pwad "out2.wad" (w/wad-data)))
  (compile-level level)
  )
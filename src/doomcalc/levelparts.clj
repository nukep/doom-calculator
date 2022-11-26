(ns doomcalc.levelparts
  (:require [doomcalc.logic :as l]
            [doomcalc.tree :as t :refer [mkvar]]
            [doomcalc.wad-builder :as w]
            [doomcalc.digits :as digits]))


(def THING_PLAYER1 0x001)
(def THING_PINKY 3002)
(def MONSTER_THING THING_PINKY)
(def MONSTER_RADIUS 30)
;; pinky has radius of 30, height of 56

(def THING_ZOMBIEMAN 3004)
;; zombieman has radius of 20, height of 56

(def THING_TELEPORTER 0x00E)
(def SPECIAL_DR_DOOR 1)
(def SPECIAL_WR_LIFT_ALSO_MONSTERS 88)
(def SPECIAL_WR_TELEPORT 97)
(def SPECIAL_S1_DOOR_STAY_OPEN_FAST 112)
(def ML_BLOCKING 1)        ;; block players and monsters
(def ML_BLOCKMONSTERS 2)   ;; block monsters only
(def ML_DONTPEGBOTTOM 16)  ;;

;; a size of 60 is the bare minimum for a Pinky to fit teleport somewhere without any problems
;; i.e. 2 times the radius
(def MONSTER_TELE_DEST_MIN_WIDTH (* MONSTER_RADIUS 2))


(def DIGIT_PIXEL_HEIGHT 48)

;; Vanilla Doom hardcodes the switch texture names and which textures they become.
;; I'm choosing to override a switch texture in a PWAD.
;; It falls back gracefully to the built-in SW1EXIT if our texture PWAD isn't used.
(def DIGIT-SWITCH-TEXTURES-HACK
  {0 ["SW1EXIT" (* 32 0) (* 72 0)]
   1 ["SW1EXIT" (* 32 1) (* 72 0)]
   2 ["SW1EXIT" (* 32 2) (* 72 0)]
   3 ["SW1EXIT" (* 32 3) (* 72 0)]
   4 ["SW1EXIT" (* 32 4) (* 72 0)]
   5 ["SW1EXIT" (* 32 5) (* 72 0)]
   6 ["SW1EXIT" (* 32 0) (* 72 1)]
   7 ["SW1EXIT" (* 32 1) (* 72 1)]
   8 ["SW1EXIT" (* 32 2) (* 72 1)]
   9 ["SW1EXIT" (* 32 3) (* 72 1)]})

(def DIGIT-SWITCH-TEXTURES DIGIT-SWITCH-TEXTURES-HACK)


;; A Component couples drawing and logic.
;; I found that passing lists of variables around like hot potato
;; from tree-building procedures to drawing procedures was discouraging and confusing.

;; Vertex positions:
;; C               B
;; D  E .......... A
(defn draw-switches [tags switch-info {:keys [x y outer-sector base-floor-height ceil-height]}]
  (w/with-pushpop-state
    (let [outer-tex "STONE2"

          switches (count tags)

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
          C (pos 0 switch-thick)
          D (pos 0 0)]
      (w/set-front {:sector outer-sector
                    :lower-tex outer-tex})
      (w/set-back {:sector switch-sector})
      (w/draw-poly A B C D)

      (w/set-line-special SPECIAL_S1_DOOR_STAY_OPEN_FAST)
      (doseq [i (range switches)]
        (let [[tex xoff yoff] (switch-info i)]
          (w/set-line-tag (nth tags i))
          (w/set-front {:sector outer-sector
                        :lower-tex tex
                        :xoff xoff
                        :yoff yoff})
          (w/draw-poly (pos (* switch-width i) 0)
                       (pos (* switch-width (inc i)) 0)))))))

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
                   x2 (+ x (* 5 32) 4)]
               ;; draw 2 sets of switch arrays: 0 to 4, and 5 to 9
               (draw-switches (take 5 tags) (fn [i] (get DIGIT-SWITCH-TEXTURES i))
                              {:x x,  :y y, :outer-sector outer-sector, :base-floor-height floor-height, :ceil-height ceil-height})
               (draw-switches (drop 5 tags) (fn [i] (get DIGIT-SWITCH-TEXTURES (+ i 5)))
                              {:x x2, :y y, :outer-sector outer-sector, :base-floor-height floor-height, :ceil-height ceil-height})))}))

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

(defn make-base2-digit-at-position [position b0 r]
  ;; it's just 0 or 1, to show the carry digit
  (let [f (fn [digit] (let [v (get (get digits/digits digit) position)]
                        (if (= v '_) 0 1)))]
    (b0 (r (f 0))
        (r (f 1)))))

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

(defn draw-digit-display [tags {:keys [pixels-w pixels-h outer-sector base-floor-height ceil-height increment-floor-by]}]
  (w/draw-square-lattice
   (for [i (reverse (range pixels-h))]
     (for [j (range pixels-w)]
       {:sector (w/create-sector {:floor-height (+ base-floor-height (* increment-floor-by (- (- pixels-h 1) i)))
                                  :ceil-height ceil-height
                                  :floor-tex "CEIL4_1"
                                  :ceil-tex "MFLR8_1"
                                  :light 255
                                  :tag (nth tags (+ (* i pixels-w) j))})
        :t {:lower-tex "BLAKWAL2"}
        :r {:flags ML_BLOCKING}
        :b {:flags ML_BLOCKING}
        :draw (fn [] (w/add-thing {:angle 90 :type THING_TELEPORTER}))}))
   {:sector outer-sector
    :lower-tex "STONE2"
    :flags ML_DONTPEGBOTTOM}
   MONSTER_TELE_DEST_MIN_WIDTH MONSTER_TELE_DEST_MIN_WIDTH))

(defn digit-carry-display [{:keys [x y bit base-floor-height]}]
  (let [output-vars (mapv (fn [_] (mkvar)) (range digits/digit-positions))
        trees (vec
               (for [position (range digits/digit-positions)]
                 (make-base2-digit-at-position position bit (nth output-vars position))))]

    {:trees trees
     :vars {}
     :draw (fn [var->door-tag var->tele-tag {:keys [outer-sector floor-height ceil-height]}]
             (let [tags (mapv #(var->tele-tag % 1) output-vars)]
               (w/with-pushpop-state
                 (w/translate x y)
                 (draw-digit-display tags
                                     {:pixels-w digits/digit-width :pixels-h digits/digit-height
                                      :outer-sector outer-sector
                                      :base-floor-height (+ floor-height base-floor-height)
                                      :ceil-height ceil-height
                                      :increment-floor-by DIGIT_PIXEL_HEIGHT}))))}))

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
               (w/with-pushpop-state
                 (w/translate x y)
                 (draw-digit-display tags
                                     {:pixels-w digits/digit-width :pixels-h digits/digit-height
                                      :outer-sector outer-sector
                                      :base-floor-height (+ floor-height base-floor-height)
                                      :ceil-height ceil-height
                                      :increment-floor-by DIGIT_PIXEL_HEIGHT}))))}))

(defn glyph
  "A component that draws a vertical (looking) 2x2 grid with the pattern."
  [cells {:keys [x y size]
                    :or {size 64}}]
  {:draw (fn [_ _ {:keys [outer-sector floor-height ceil-height]}]
           (let [empty-row (repeat (count (first cells)) '_)
                 ;; prepend an empty row to render the top
                 cells (cons empty-row cells)]
             (w/with-pushpop-state
               (w/translate x y)
               (w/draw-square-lattice
                (for [[i row] (map-indexed vector (reverse cells))]
                  (for [cell row]
                    {:sector (w/create-sector {:floor-height (+ floor-height (* size i))
                                               :ceil-height ceil-height
                                               :floor-tex "CEIL4_1"
                                               :ceil-tex "MFLR8_1"
                                               :light (if (= cell '_) 0 255)})
                     :t {:lower-tex (if (= cell '_) "ASHWALL2" "ASHWALL2")}}))
                {:sector outer-sector
                 :lower-tex "STONE2"
                 :flags ML_DONTPEGBOTTOM}
                4 size))))})

(defn player [x y angle]
  {:draw (fn [_ _ _]
           (w/add-thing {:x x :y y :angle angle
                         :type THING_PLAYER1}))})


(defn variable-display
  "Debugging tool to show a variable result"
  [{:keys [x y v base-floor-height]
    :or {base-floor-height 0}}]
  (let [floor-tex "CEIL4_1"
        ceil-tex "MFLR8_1"
        side-tex "STONE2"]
    {:draw (fn [var->door-tag var->tele-tag {:keys [outer-sector floor-height ceil-height]}]
             (w/with-pushpop-state
               (w/translate x y)
               (w/draw-square-lattice
                [[{:sector (w/create-sector {:floor-height (+ floor-height base-floor-height)
                                             :ceil-height ceil-height
                                             :floor-tex floor-tex
                                             :ceil-tex ceil-tex
                                             :light 255
                                             :tag (var->tele-tag v 0)})
                   :draw (fn [] (w/add-thing {:angle 90 :type THING_TELEPORTER}))
                   :t {:flags ML_BLOCKING} :b {:flags ML_BLOCKING} :l {:flags ML_BLOCKING} :r {:flags ML_BLOCKING}}
                  {:sector (w/create-sector {:floor-height (+ floor-height base-floor-height)
                                             :ceil-height ceil-height
                                             :floor-tex floor-tex
                                             :ceil-tex ceil-tex
                                             :light 255
                                             :tag (var->tele-tag v 1)})
                   :draw (fn [] (w/add-thing {:angle 90 :type THING_TELEPORTER}))
                   :t {:flags ML_BLOCKING} :b {:flags ML_BLOCKING} :l {:flags ML_BLOCKING} :r {:flags ML_BLOCKING}}]]
                {:sector outer-sector
                 :lower-tex side-tex}

                MONSTER_TELE_DEST_MIN_WIDTH MONSTER_TELE_DEST_MIN_WIDTH)))}))

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


;; for pinky
(def DROOM_WIDTH 64)
(def TELE2_X_WIDTH 60)
(def TELE2_I_WIDTH 40)

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
                                                      :teleporter THING_TELEPORTER)}))
                   :t {:tag (:o0 tele2) :special SPECIAL_WR_TELEPORT :upper-tex door-tex}
                   :r {:tag (:o1 tele2) :special SPECIAL_WR_TELEPORT :upper-tex door-tex}
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
                   :b {:tag (:o const-1) :special SPECIAL_WR_TELEPORT}})

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
                   :l {:tag (:o const-2) :special SPECIAL_WR_TELEPORT}})

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
                                              :back {:sector droom-sector :upper-tex door-tex :lower-tex door-tex :special SPECIAL_DR_DOOR}}
                                             A B)
                             (w/draw-poly-ex {:front {:sector door-sector}
                                              :back {:sector droom-sector :upper-tex door-tex :lower-tex door-tex}}
                                             B C D A)
                             (w/add-thing {:angle 90 :type THING_TELEPORTER}))
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

(defn draw-old-machines
  "The old method of drawing machines. Uses more lines."
  [parts outer-sector make-door-sector]
  (let [tele2-counter (atom -1)
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
                              :outer-sector outer-sector
                              :make-door-sector make-door-sector
                              :floor-height 32
                              :ceil-height 92}))
        :droom (let [i (swap! droom-counter inc)]
                 (draw-droom info
                             {:x (+ -8200 (* tele2-spacing-x tele2-array-length) (* (mod i droom-array-length) droom-spacing-x))
                              :y (+ 64 (* (quot i droom-array-length) droom-spacing-y))
                              :outer-sector outer-sector
                              :make-door-sector make-door-sector
                              :floor-height 32
                              :ceil-height 92}))))))


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
           {:front {:sector door-overhang-sector :upper-tex "BIGDOOR2" :special SPECIAL_DR_DOOR}
            :back {:sector door-sector}}
           [(- gap-w) (+ gap-h door-w)] [(- gap-w) gap-h] [0 gap-h] [0 (+ gap-h door-w)])
          (w/draw-poly-ex
           {:front {:sector door-sector :middle-tex "DOORTRAK" :flags ML_DONTPEGBOTTOM}}
           [(- gap-w) (+ gap-h door-w)] [0 (+ gap-h door-w)]))))))

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
        dd (digit-display {:x x :y (+ y 768)
                           :bits (:vars di)
                           :base-floor-height 64})]
    {:trees [(:trees di) (:trees dd)]
     :vars (:vars di)
     :draw (fn [var->door-tag var->tele-tag outer]
             ((:draw di) var->door-tag var->tele-tag outer)
             ((:draw dd) var->door-tag var->tele-tag outer))}))

(defn level []
  (let [di01 (digit-input-and-display {:x 100 :y 400})
        di00 (digit-input-and-display {:x 450 :y 400})
        di11 (digit-input-and-display {:x 1300 :y 400})
        di10 (digit-input-and-display {:x 1650 :y 400})

        addm (bcd-adding-machine [(:vars di01) (:vars di00)]
                                 [(:vars di11) (:vars di10)])]
    [(player 512 192 90)
     di01 di00
     di11 di10
     addm
     (glyph '[[_ _ x _ _]
              [_ _ x _ _]
              [x x x x x]
              [_ _ x _ _]
              [_ _ x _ _]]
            {:x 850 :y 1024})
     (glyph '[[_ _ _ _ _]
              [x x x x x]
              [_ _ _ _ _]
              [x x x x x]
              [_ _ _ _ _]]
            {:x 2048 :y 1024})
     (digit-carry-display {:x 2500 :y (+ 400 768)
                           :bit (-> addm :vars :carry)
                           :base-floor-height 64})
     (digit-display {:x 2800 :y (+ 400 768)
                     :bits (-> addm :vars :sum (nth 0))
                     :base-floor-height 64})
     (digit-display {:x 3100 :y (+ 400 768)
                     :bits (-> addm :vars :sum (nth 1))
                     :base-floor-height 64})]))

(defn testlevel []
  (let [d1 (binary-4-input {:x 64 :y 200})]
    [(player 512 192 90)
     d1
     (digit-carry-display {:x 128 :y (+ 400 512)
                           :bit (-> d1 :vars (nth 0))
                           :base-floor-height 64})]))

(comment
  (w/with-debug-svg
    (compile-level level)
    (count (:linedefs (w/wad-data))))


  (w/with-debug-svg
    (compile-level level)
    (doomcalc.write-pwad/spit-pwad "out2.wad" (w/wad-data)))

  )
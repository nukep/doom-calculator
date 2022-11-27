(ns doomcalc.components
  (:require [doomcalc.logic :as l]
            [doomcalc.tree :as t :refer [mkvar]]
            [doomcalc.wad-builder :as w]
            [doomcalc.digits :as digits]
            [doomcalc.wad-constants :as wc]))

;; A Component couples drawing and logic.
;; I found that passing lists of variables around like hot potato
;; from tree-building procedures to drawing procedures was discouraging and confusing.

(def MONSTER_THING wc/THING_PINKY)
(def MONSTER_RADIUS wc/PINKY_RADIUS)

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

      (w/set-line-special wc/SPECIAL_S1_DOOR_STAY_OPEN_FAST)
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
                                                  (fn [i] (get DIGIT-SWITCH-TEXTURES i))
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
        :r {:flags wc/ML_BLOCKING}
        :b {:flags wc/ML_BLOCKING}
        :draw (fn [] (w/add-thing {:angle 90 :type wc/THING_TELEPORTER}))}))
   {:sector outer-sector
    :lower-tex "STONE2"
    :flags wc/ML_DONTPEGBOTTOM}
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
                 :flags wc/ML_DONTPEGBOTTOM}
                4 size))))})

(defn player [x y angle]
  {:draw (fn [_ _ _]
           (w/add-thing {:x x :y y :angle angle
                         :type wc/THING_PLAYER1}))})


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
                   :draw (fn [] (w/add-thing {:angle 90 :type wc/THING_TELEPORTER}))
                   :t {:flags wc/ML_BLOCKING} :b {:flags wc/ML_BLOCKING} :l {:flags wc/ML_BLOCKING} :r {:flags wc/ML_BLOCKING}}
                  {:sector (w/create-sector {:floor-height (+ floor-height base-floor-height)
                                             :ceil-height ceil-height
                                             :floor-tex floor-tex
                                             :ceil-tex ceil-tex
                                             :light 255
                                             :tag (var->tele-tag v 1)})
                   :draw (fn [] (w/add-thing {:angle 90 :type wc/THING_TELEPORTER}))
                   :t {:flags wc/ML_BLOCKING} :b {:flags wc/ML_BLOCKING} :l {:flags wc/ML_BLOCKING} :r {:flags wc/ML_BLOCKING}}]]
                {:sector outer-sector
                 :lower-tex side-tex}

                MONSTER_TELE_DEST_MIN_WIDTH MONSTER_TELE_DEST_MIN_WIDTH)))}))


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

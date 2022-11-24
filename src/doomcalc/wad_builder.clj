(ns doomcalc.wad-builder)

(def ^:dynamic *CTX* nil)

(def NIL-SIDEDEF 65535)

(deftype AutoId [on-new-value cache])

(defn get-id [autoid value]
  (if (contains? @(.-cache autoid) value)
    (get @(.-cache autoid) value)

    (let [id ((.-on-new-value autoid) value)]
      (swap! (.-cache autoid) assoc value id)
      id)))

(defrecord WadBuilder [wad-data state state-history vertex-auto-id sidedef-auto-id])

(defn wad-data []
  @(:wad-data *CTX*))

(defn add-record [type value]
  (let [new-wad-data (swap! (:wad-data *CTX*) update type conj value)]
    ;; return id
    (dec (count (get new-wad-data type)))))

(defn make-wad-data-auto-id* [wad-data-atom type]
  (AutoId. (fn [value]
             (let [new-wad-data (swap! wad-data-atom update type conj value)
                   new-id (dec (count (get new-wad-data type)))]
               new-id))
           (atom {})))

(defn new-wad-builder []
  (let [wad-data-atom (atom {:sectors []
                             :vertexes []
                             :sidedefs []
                             :linedefs []
                             :things []})]
    (WadBuilder. wad-data-atom
                 (atom {})
                 (atom (list))
                 (make-wad-data-auto-id* wad-data-atom :vertexes)
                 (make-wad-data-auto-id* wad-data-atom :sidedefs))))

(defmacro with-new-wad-builder [& body]
  `(binding [*CTX* (new-wad-builder)]
     ~@body))

(defn get-state*
  ([key]
   (get @(:state *CTX*) key))
  ([key default]
   (get @(:state *CTX*) key default)))
(defn set-state* [key value]
  (swap! (:state *CTX*) assoc key value))

(defn push-state []
  (swap! (:state-history *CTX*) conj @(:state *CTX*)))
(defn pop-state []
  (let [[old _new] (swap-vals! (:state-history *CTX*) pop)
        popped (peek old)]
    (reset! (:state *CTX*) popped)))

(defn line-tag []
  (get-state* :line-tag 0))
(defn set-line-tag [wadtag]
  (set-state* :line-tag wadtag))
(defn clear-line-tag []
  (set-state* :line-tag 0))

(defn line-special []
  (get-state* :line-special 0))
(defn set-line-special [special]
  (set-state* :line-special special))
(defn clear-line-special []
  (set-state* :line-special 0))

(defn line-flags []
  (get-state* :line-flags 0))
(defn set-line-flags [flags]
  (set-state* :line-flags flags))
(defn clear-line-flags []
  (set-state* :line-flags 0))

(defn back-sidedef-id []
  (get-state* :back-sidedef-id NIL-SIDEDEF))
(defn front-sidedef-id []
  (get-state* :front-sidedef-id NIL-SIDEDEF))

(defn create-sector [sector-info]
  (let [sector-defaults {:floor-height 0
                         :ceil-height 0
                         :floor-tex "MFLR8_1"
                         :ceil-tex "MFLR8_1"
                         :light 160
                         :tag 0}]
    (add-record :sectors (merge sector-defaults
                                    (select-keys sector-info
                                                 (keys sector-defaults))))))

(defn vertex->id [x y]
  (get-id (:vertex-auto-id *CTX*) [x y]))

(defn sidedef->id [sidedef-info]
  (let [sidedef-defaults {:xoff 0
                          :yoff 0
                          :upper-tex "-"
                          :middle-tex "-"
                          :lower-tex "-"
                          :sector 0}]
    (get-id (:sidedef-auto-id *CTX*) (merge sidedef-defaults
                                          (select-keys sidedef-info
                                                       (keys sidedef-defaults))))))


(defn set-back [sidedef-info]
  (set-state* :back-sidedef-id (sidedef->id sidedef-info)))
(defn set-front [sidedef-info]
  (set-state* :front-sidedef-id (sidedef->id sidedef-info)))
(defn flip-sidedefs []
  (let [f (front-sidedef-id)
        b (back-sidedef-id)]
    (set-state* :back-sidedef-id f)
    (set-state* :front-sidedef-id b)))


(defn get-transform-matrix []
  (get-state* :transform [[1 0 0]
                          [0 1 0]]))

(defn set-transform-matrix [matrix]
  (set-state* :transform matrix))

(defn- multiply-2x3-matrices [a b]
  ;; we assume the 3rd row of each matrix is [0 0 1]
  (let [dot (fn [x y] (reduce + (map * x y)))
        row (fn [m i] (if (= i 2) [0 0 1] (nth m i)))
        col (fn [m j] (mapv #(nth (row m %) j) (range 3)))]
    (vec (for [i (range 2)]
           (vec (for [j (range 3)]
                  (dot (row a i) (col b j))))))))


(defn- det-of-2x3-matrix [[[a b _c]
                           [d e _f]]]
  (- (* a e) (* b d)))

(defn- transform-point
  "Get the transformed point given the transform matrix. Round to integers.
   
   Right-multiply the matrix with the column vector [x;y;1]:

   [x']   [m11 m12 m13] [x]
   [y'] = [m21 m22 m23] [y]
   [ 1]   [  0   0   1] [1]
   "
  [matrix [x y]]
  ;; we assume the 3rd row of the matrix is [0 0 1]
  (let [[[m11 m12 m13] [m21 m22 m23]] matrix]
    [(Math/round (double (+ (* m11 x) (* m12 y) m13)))
     (Math/round (double (+ (* m21 x) (* m22 y) m23)))]))

(defn transform
  "Right-multiply the current transform matrix"
  [matrix]
  (set-transform-matrix (multiply-2x3-matrices (get-transform-matrix) matrix)))

(defn translate
  "Translates the transform."
  [x y]
  (transform [[1 0 x]
              [0 1 y]]))

(defn scale
  "Scales the transform."
  ([s] (scale s s))
  ([x y] (transform [[x 0 0]
                     [0 y 0]])))

(defn rotate
  "Rotates the transform counter-clockwise."
  [degrees]
  (let [radians (* (/ degrees 360) Math/PI 2)
        c (Math/cos radians)
        s (Math/sin radians)]
    (transform [[c (- s) 0]
                [s c     0]])))

(defn- transform-angle
  "Calculate the new angle in degrees after it's been transformed. Round to an integer."
  [matrix degrees]
  (let [[[m11 m12 _m13]
         [m21 m22 _m23]] matrix]
    (if (and (= m12 m21 0) (= m11 m22))
      ;; don't do anything if matrix is identity or a uniform scale
      degrees

      (let [radians (* (/ degrees 360) Math/PI 2)
            c (Math/cos radians)
            s (Math/sin radians)

            unit-x (+ (* c m11) (* s m12))
            unit-y (+ (* c m21) (* s m22))
            new-radians (Math/atan2 unit-y unit-x)]
        (Math/round (* (/ new-radians (* Math/PI 2)) 360))))))

(defn add-thing [{:keys [x y angle type]
                  ;; default type is health potion
                  :or {x 0 y 0 angle 0 type 0x7DE}}]
  (let [matrix (get-transform-matrix)
        [x y] (transform-point matrix [x y])
        angle (transform-angle matrix angle)]
    (add-record :things {:x x :y y :angle angle :type type})))

(defn draw-poly
  "Draw connected lines."
  [& points]
  ;; if the determinant of the matrix is negative, the orientation is flipped.
  ;; so we'll reverse the points to preserve orientation
  (let [matrix (get-transform-matrix)
        det (det-of-2x3-matrix matrix)
        points (if (>= det 0) points (reverse points))]

    (doseq [i (range (dec (count points)))]
      (let [[x0 y0] (transform-point matrix (nth points i))
            [x1 y1] (transform-point matrix (nth points (inc i)))

            front (front-sidedef-id)
            back (back-sidedef-id)
            has-front? (not= front NIL-SIDEDEF)
            has-back? (not= back NIL-SIDEDEF)]
        (add-record :linedefs
                    {:v1 (vertex->id x0 y0)
                     :v2 (vertex->id x1 y1)
                     :flags (bit-or (if (and has-front? has-back?) 4 0)
                                    (line-flags))
                     :special (line-special)
                     :sector-tag (line-tag)
                     :front-sidedef front
                     :back-sidedef back})))))

(defn draw-poly-ex
  "An extended version of draw-poly that also sets sidedefs and the line tag/special/flags.
   If the linedef special is defined on the back, the lines are flipped so it can be activated by walking from that side.
   "
  [{:keys [front back]} & points]

  (let [special-on-back? (and (not (contains? front :special)) (contains? back :special))]
    ;; the front tag/special/flags are prioritized. fallback to the back if the front doesn't specify them.
    (set-line-tag     (or (:tag front) (:tag back) 0))
    (set-line-special (or (:special front) (:special back) 0))
    (set-line-flags   (or (:flags front) (:flags back) 0))
    (set-front (select-keys front [:sector :upper-tex :lower-tex :middle-tex :xoff :yoff]))
    (set-back  (select-keys back  [:sector :upper-tex :lower-tex :middle-tex :xoff :yoff]))
    (if special-on-back?
      (do
        (flip-sidedefs)
        (apply draw-poly (reverse points)))

      (apply draw-poly points))))

(defn- lookup-or-constantly [x]
  (cond
    (fn? x) x
    (map? x) x
    :else (constantly x)))

(defn draw-square-lattice
  "Draw a 2D array of squares. Adjancent squares share lines.
   Each square has a :sector, and 4 lines (:t :b :l :r for top, bottom, left, right) with the properties:
     :upper-tex :lower-tex :middle-tex :xoff :yoff
   Those lines can have additional line properties:
     :tag :special :flags

   "
  [squares outer row-size col-size]
  (let [row-size (lookup-or-constantly row-size)
        col-size (lookup-or-constantly col-size)
        rows (count squares)
        cols (reduce max (map count squares))
        row-y (vec (cons 0 (reductions + (map row-size (range rows)))))
        col-x (vec (cons 0 (reductions + (map col-size (range cols)))))
        pos (fn [i j] [(get col-x j) (get row-y i)])
        square-at (fn [i j] (nth (nth squares i nil) j nil))]
    (doseq [i (range rows)
            j (range cols)]
      (when-let [sq (square-at i j)]
        (let [north-sq (square-at (inc i) j)
              east-sq  (square-at i       (inc j))
              south-sq (square-at (dec i) j)
              west-sq  (square-at i       (dec j))

              current-sector (get sq :sector)
              north-sector (get north-sq :sector (:sector outer))
              east-sector  (get east-sq  :sector (:sector outer))
              south-sector (get south-sq :sector (:sector outer))
              west-sector  (get west-sq  :sector (:sector outer))
              draw-inside-square (:draw sq)]
          ;; if there's no north square, draw the top line
          ;; if there's no east square, draw the right line
          ;; always draw the bottom and left lines

          ;; top
          ;; front is north, back is current
          (when-not north-sq
            (draw-poly-ex {:front (if north-sq
                                    (merge (-> north-sq :b) {:sector north-sector})
                                    outer)
                           :back  (merge (-> sq :t) {:sector current-sector})}
                          (pos (inc i) (inc j)) (pos (inc i) j)))
          ;; right
          ;; front is east, back is current
          (when-not east-sq
            (draw-poly-ex {:front (if east-sq
                                    (merge (-> east-sq :l) {:sector east-sector})
                                    outer)
                           :back  (merge (-> sq :r) {:sector current-sector})}
                          (pos i (inc j)) (pos (inc i) (inc j))))

          ;; bottom
          ;; front is current, back is south
          (draw-poly-ex {:front (merge (-> sq :b) {:sector current-sector})
                         :back  (if south-sq
                                  (merge (-> south-sq :t) {:sector south-sector})
                                  outer)}
                        (pos i (inc j)) (pos i j))
          ;; left
          ;; front is current, back is west

          (draw-poly-ex {:front (merge (-> sq :l) {:sector current-sector})
                         :back  (if west-sq
                                  (merge (-> west-sq :r) {:sector west-sector})
                                  outer)}
                        (pos i j) (pos (inc i) j))

          (when draw-inside-square
            (let [[x1 y1] (pos i j)
                  [x2 y2] (pos (inc i) (inc j))
                  x (quot (+ x1 x2) 2)
                  y (quot (+ y1 y2) 2)]
              (push-state)
              (translate x y)
              (draw-inside-square)
              (pop-state))))))))

(defn debug-svg []
  ;; just draw linedefs
  (let [data (wad-data)
        vertices (atom [])

        body
        (with-out-str
          (doseq [thing (:things data)]
            (println "<circle cx=\"" (:x thing) "\" cy=\"" (:y thing) "\" r=\"4\" fill=\"black\" />")
            #_(println thing))
          
          (doseq [linedef (:linedefs data)]
            (let [[v1x v1y] (nth (:vertexes data) (:v1 linedef))
                  [v2x v2y] (nth (:vertexes data) (:v2 linedef))]
              (swap! vertices conj [v1x v1y] [v2x v2y])
              (println (str "<line x1=\"" v1x "\" y1=\"" v1y "\" x2=\"" v2x "\" y2=\"" v2y "\" stroke=\"black\" />"))
              ;; draw perpendicular line. rotate (v2-v1) 90 degrees counterclockwise.
              (let [dx (- v2x v1x)
                    dy (- v2y v1y)
                    len (Math/sqrt (+ (* dx dx) (* dy dy)))
                    dx (* dx (/ 4 len))
                    dy (* dy (/ 4 len))
                    x1 (/ (+ v1x v2x) 2)
                    y1 (/ (+ v1y v2y) 2)
                    x2 (+ x1 dy)
                    y2 (- y1 dx)

                    x1 (double x1)
                    y1 (double y1)
                    x2 (double x2)
                    y2 (double y2)]
                (println (str "  <line x1=\"" x1 "\" y1=\"" y1 "\" x2=\"" x2 "\" y2=\"" y2 "\" stroke=\"black\" />"))))))

        vertices @vertices

        min-x (reduce min (map first vertices))
        max-x (reduce max (map first vertices))
        min-y (reduce min (map second vertices))
        max-y (reduce max (map second vertices))
        margin 16
        min-x (- min-x margin) min-y (- min-y margin)
        max-x (+ max-x margin) max-y (+ max-y margin)
        
        ;; flip y to make Y point up
        [min-y max-y] [(- max-y) (- min-y)]

        svg (str "<svg viewBox=\"" min-x " " min-y" " (- max-x min-x) " " (- max-y min-y) "\" xmlns=\"http://www.w3.org/2000/svg\">"
                 "<g transform=\"scale(1 -1)\">"
                 ;; draw axis lines
                 (str "<line x1=\"0\" y1=\"" 2048 "\" x2=\"0\" y2=\"" -2048 "\" stroke=\"green\" />")
                 (str "<line y1=\"0\" x1=\"" 2048 "\" y2=\"0\" x2=\"" -2048 "\" stroke=\"red\" />")
                 ;; draw everything else
                 body
                 "</g>"
                 "</svg>")]
    (spit "out.svg" svg)))

(defmacro with-debug-svg [& body]
  `(with-new-wad-builder
     (let [out# (do ~@body)]
       (debug-svg)
       out#)))

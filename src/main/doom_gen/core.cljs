(ns doom-gen.core
  (:require [reagent.core :as r]))

(defprotocol AWackyHighLevelMachine
  (gen-id [ctx])
  (external-switch [ctx id tag])
  (external-teleporter [ctx id tag])
  (tele2 [ctx info-keys]))

(deftype WackyHighLevelMachine [id-counter program]
  AWackyHighLevelMachine

  (gen-id              [ctx]        (let [[old _] (swap-vals! id-counter inc)] old))
  (external-switch     [ctx id tag] (swap! program conj {:type :external-switch :id id :tag tag}))
  (external-teleporter [ctx id tag] (swap! program conj {:type :external-teleporter :id id :tag tag}))
  (tele2               [ctx info]   (swap! program conj (-> info (assoc :type :tele2)))))

(defn gen-port [ctx]
  {0 (gen-id ctx)
   1 (gen-id ctx)})

(defn make-nand [ctx a b r]
  (let [id0 (gen-id ctx)
        id1 (gen-id ctx)]
    (tele2 ctx {:id id0 :door0 (a 0) :door1 (a 1) :on0 (r 1) :on1 id1})
    (tele2 ctx {:id id1 :door0 (b 0) :door1 (b 1) :on0 (r 1) :on1 (r 0)})))

(defn make-and [ctx a b r]
  (let [id0 (gen-id ctx)
        id1 (gen-id ctx)]
    (tele2 ctx {:id id0 :door0 (a 0) :door1 (a 1) :on0 (r 0) :on1 id1})
    (tele2 ctx {:id id1 :door0 (b 0) :door1 (b 1) :on0 (r 0) :on1 (r 1)})))

(defn make-or [ctx a b r]
  (let [id0 (gen-id ctx)
        id1 (gen-id ctx)]
    (tele2 ctx {:id id0 :door0 (a 0) :door1 (a 1) :on0 id1   :on1 (r 1)})
    (tele2 ctx {:id id1 :door0 (b 0) :door1 (b 1) :on0 (r 0) :on1 (r 1)})))

(defn make-xor [ctx a b r]
  (let [id0 (gen-id ctx)
        id1 (gen-id ctx)
        id2 (gen-id ctx)]
    (tele2 ctx {:id id0 :door0 (a 0) :door1 (a 1) :on0 id1   :on1 id2})
    (tele2 ctx {:id id1 :door0 (b 0) :door1 (b 1) :on0 (r 0) :on1 (r 1)})
    (tele2 ctx {:id id2 :door0 (b 0) :door1 (b 1) :on0 (r 1) :on1 (r 0)})))

(defn make-half-adder [ctx a b c s]
  (make-and ctx a b c)
  (make-xor ctx a b s))

(defn make-full-adder [ctx a b cin cout s]
  (let [xor-ab         (gen-port ctx)
        and-ab         (gen-port ctx)
        and-cin_xor-ab (gen-port ctx)]
    (make-xor ctx a      b              xor-ab)
    (make-xor ctx xor-ab cin            s)
    (make-and ctx a      b              and-ab)
    (make-and ctx cin    xor-ab         and-cin_xor-ab)
    (make-or  ctx and-ab and-cin_xor-ab cout)))

(defn conj-set [s v] (conj (or s #{}) v))
(defn ensure-set [s] (or s #{}))

(defn program->adjacency-list [program]
  (let [adj (atom {})]
    (doseq [x program]
      (case (:type x)
        :tele2
        (do
          (swap! adj update (:door0 x) ensure-set)
          (swap! adj update (:door1 x) ensure-set)
          (swap! adj update (:on0 x) conj-set (:id x))
          (swap! adj update (:on1 x) conj-set (:id x))
          (swap! adj update (:id x) conj-set (:door0 x))
          (swap! adj update (:id x) conj-set (:door1 x)))
        nil))
    @adj))

(defn program->id-map [program]
  (let [m (atom {})]
    (doseq [x program]
      (swap! m assoc (:id x) x))
    @m))

(defn layout-program [program]
  (let [adj     (program->adjacency-list program)
        layers  (atom [])]
    (loop [adj adj]
      (when-not (empty? adj)
        (let [removed (atom #{})
              p1 (into {} (filter (fn [[id l]]
                                    (if (empty? l)
                                      (do (swap! removed conj id)
                                          false)
                                      true))
                                  adj))
              removed @removed
              p2 (into {} (map (fn [[id l]] [id (clojure.set/difference l removed)])) p1)]
          (swap! layers conj (sort removed))
          (recur p2))))
    @layers))


(defn tele2-component [x y left-open? right-open?]
  (let [width 48
        height 20
        side-space 10
        door-width 4

        xoff (+ x (- 0 (/ width 2)))
        yoff (+ y (- 0 (/ height 2)))]
    [:g
     [:rect {:x (+ xoff 0) :y yoff :width width :height height :fill "#ddd" :stroke "#aaa"}]
     (when-not left-open?
       [:rect {:x (+ xoff side-space)                      :y yoff :width door-width :height height :fill "#222"}])
     (when-not right-open?
       [:rect {:x (+ xoff (- width side-space door-width)) :y yoff :width door-width :height height :fill "#222"}])]))

(defn lerp [a b p]
  (+ a (* (- b a) p)))

;; Note: assumes we're pointing down
(defn directed-curve-component [x1 y1 x4 y4]
  (let [stroke-width 1
        head-height 0 #_(* stroke-width 2)
        y4 (- y4 head-height)
        
        p  0.5
        x2 x1
        y2 (lerp y1 y4 p)
        x3 x4
        y3 (lerp y4 y1 p)]
    #_[:path {:stroke-width stroke-width  :fill "none" :stroke "rgba(0,0,0,1)"
            :d (str "M" x1 "," y1 " L" x4 "," y4)}]
    [:path {;;:marker-end "url(#head)"
            :stroke-width stroke-width :fill "none" :stroke "rgba(0,0,0,1)"
            :d (str "M" x1 "," y1
                    " C" x2 "," y2
                    " " x3 "," y3
                    " " x4 "," y4)}]))

(def test-program
  (let [bits 8
        ;; 0 1, 2 3, 4 5 = A0 + B0 = R0

        program-a (atom (->> (range bits)
                             (map (fn [i] (let [i (* i 8)]
                                            [{:id (+ i 0) :type :external-switch}     {:id (+ i 1) :type :external-switch}
                                             {:id (+ i 2) :type :external-switch}     {:id (+ i 3) :type :external-switch}
                                             {:id (+ i 4) :type :external-teleporter} {:id (+ i 5) :type :external-teleporter}])))
                             (reduce concat)
                             (vec)))

        start-at (* bits 8)

        ctx (->WackyHighLevelMachine (atom start-at) program-a)
        gp (fn [] (gen-port ctx))

        ;; the first digit has to use a half-adder
        _ (make-half-adder ctx {0 0, 1 1} {0 2, 1 3} {0 6, 1 7} {0 4, 1 5})

        #_#__ (make-full-adder ctx {0 0 1 1} {0 2 1 3} {0 4 1 5} {0 6 1 7} {0 8 1 9})]
    (doseq [n (range 1 bits)]
      (let [i (* n 8)
            prev-i (* (- n 1) 8)]
        (make-full-adder ctx
                         {0 (+ i 0), 1 (+ i 1)} {0 (+ i 2), 1 (+ i 3)} {0 (+ prev-i 6), 1 (+ prev-i 7)}
                         {0 (+ i 6), 1 (+ i 7)} {0 (+ i 4), 1 (+ i 5)})))
    @program-a))

(def test-program-
  (let [program-a (atom [{:id 0 :type :external-switch}     {:id 1 :type :external-switch}
                         {:id 2 :type :external-switch}     {:id 3 :type :external-switch}
                         {:id 4 :type :external-teleporter} {:id 5 :type :external-teleporter}])
        ctx (->WackyHighLevelMachine (atom 6) program-a)
        gp (fn [] (gen-port ctx))
        _ (make-nand ctx {0 0 1 1} {0 2 1 3} {0 4 1 5})]
    @program-a))
(comment
  
  test-program
  )


(defn translate [x y xo yo]
  [(+ x xo) (+ y yo)])

(defn enumerate [coll] (map-indexed #(do [%1 %2]) coll))

(defn make-id->xy [layout]
  (let [id->xy (atom {})]
    (doseq [[y row] (enumerate layout)]
      (let [items (count row)]
        (doseq [[x id] (enumerate row)]
          (swap! id->xy assoc id [(+ 50 (* x 60))
                                  #_(+ 50 (* (if (> items 1 )
                                             (/ x (dec items))
                                             0) 1200))
                                  (+ 30 (* y 60))]))))
    @id->xy))

(defn program->effect-graph [program]
  (let [pm (program->id-map program)
        rooms (atom (sorted-map))]
    (doseq [{:keys [id type on0 on1 door0 door1]} program]
      (swap! rooms assoc-in [id :type] type)
      
      (when (= type :tele2)
        (when door0 (swap! rooms update-in [door0 :opens] conj-set [:0 id]))
        (when door1 (swap! rooms update-in [door1 :opens] conj-set [:1 id]))
        (when on0
          (swap! rooms assoc-in [id :on0] on0)
          (swap! rooms assoc-in [on0 :teleporter?] true))
        (when on1
          (swap! rooms assoc-in [id :on1] on1)
          (swap! rooms assoc-in [on1 :teleporter?] true))

        (when-not (get pm on0)
          (swap! rooms assoc-in [on0 :type] :droom))
        (when-not (get pm on1)
          (swap! rooms assoc-in [on1 :type] :droom))))
    @rooms))


;; How to simulate?
;; have a list of monsters. each monster has internal state. a monster will change directions randomly, for random amounts of time. teleportation resets its direction.
;; to simplify the simulation, we can assocate a monster to a single enclosed area (where the monster is to stay). its coordinates will be relative to the origin of this area.
;; 
;; 
;; if on0/on1 goes "nowhere", it's a droom.

(def tele2-door-width 16)
(def tele2-width 64)
(def tele2-height 64)

(def PI_180 (/ Math/PI 180))

(Math/sin (* PI_180 90))



;; r is in degrees
(defn move-monster [{:keys [x y r walk-for-ticks] :as monster}]
  (let [distance 1
        offx (* (Math/cos (* PI_180 r)) distance)
        offy (* (Math/sin (* PI_180 r)) distance)

        ;; new random angle
        r (if (= walk-for-ticks 0)
            (rand-int 360)
            r)

        ;; new random walking duration
        walk-for-ticks (if (= walk-for-ticks 0)
                         (+ 30 (rand-int 60))
                         (dec walk-for-ticks))]
    (merge monster
           {:x (+ x offx)
            :y (+ y offy)
            :r r
            :walk-for-ticks walk-for-ticks})))

(let [r 68
      
      r (- (mod (+ r 360) 360) 90)  ;; -90 to +90
      r (- 0 r)  ;; +90 to -90
      r (mod r 360)]
  r)

(defn rotation->signed [r]
  (let [r (mod r 360)
        r (if (> r 180) (- r 360) r)]
    r))

(defn rotation->unsigned [r]
  (mod r 360))

(defn rotation-reflect [r reflection]
  (rotation->unsigned (+ (- 0
                            (rotation->signed (- r reflection)))
                         reflection)))

(comment
  (rotation->signed 270)
  ;; => -90

  (rotation->signed 300)
  ;; => -60

  (rotation->signed 359)
  ;; => -1

  (rotation->signed 0)
  ;; => 0

  (rotation->signed (+ 360 270))
  ;; => -90

  (rotation->unsigned -90)
  ;; => 270
  ;; 
  )

(defn clip-monster [{:keys [x y r walk-for-ticks] :as monster} box-width box-height]
  (let [min-x (- 0 (/ box-width 2))
        max-x (+ 0 (/ box-width 2))
        min-y (- 0 (/ box-height 2))
        max-y (+ 0 (/ box-height 2))

        clip-min-x? (< x min-x)
        clip-max-x? (> x max-x)
        clip-min-y? (< y min-y)
        clip-max-y? (> y max-y)]
    (merge monster
           {:x (cond clip-min-x? min-x, clip-max-x? max-x, :else x)
            :y (cond clip-min-y? min-y, clip-max-y? max-y, :else y)

            ;; reflect off the surface we hit, like a ping-pong ball. probably not the most doom-like, but aesthetic enough.
            :r (cond clip-max-x? (rotation-reflect r 90)
                     clip-max-y? (rotation-reflect r 180)
                     clip-min-x? (rotation-reflect r 270)
                     clip-min-y? (rotation-reflect r 360)
                     :else r)})))


(defn tick-simulation [prev play-sound]
  (let [{:keys [doors monsters effect-graph]} prev

        monsters-atom (atom [])

        doors-atom (atom doors)]

    ;; if a monster is inside a droom, automatically open doors
    (doseq [monster monsters]
      (let [in-droom? (->> monster :at (get effect-graph) :type (= :droom))
            opens (->> monster :at (get effect-graph) :opens)]
        (when in-droom?
          (swap! doors-atom clojure.set/union opens))))

    (doseq [monster monsters]
      (let [in-tele2? (->> monster :at (get effect-graph) :type (= :tele2))
            monster (move-monster monster)

                     ;; teleport the monster, if applicable
            monster (if in-tele2?
                      (let [x (-> monster :x)
                            at (-> monster :at)]
                        (cond
                          (and (< x -32) (doors [:0 at]))
                          (do (play-sound :teleport)
                              {:at (->> monster :at (get effect-graph) :on0)
                               :x 0
                               :y 0
                               :r 90
                               :walk-for-ticks 0})

                          (and (> x 32) (doors [:1 at]))
                          (do (play-sound :teleport)
                              {:at (->> monster :at (get effect-graph) :on1)
                               :x 0
                               :y 0
                               :r 90
                               :walk-for-ticks 0})

                          :else
                          monster))
                      monster)

            monster (clip-monster monster 64 64)]
        (swap! monsters-atom conj monster)))

    {:doors @doors-atom
     :decorations []
     :monsters @monsters-atom
     :effect-graph effect-graph}))

(defn initial-simulation-state [program]
  (let [eg (program->effect-graph program)]
    {:doors #{}
     
     ;; stuff like teleporter animations
     :decorations []
     
     :monsters
     (let [monsters (atom [])]
       (doseq [[id {:keys [teleporter? type]}] eg]
         (when (and (= type :tele2) (not teleporter?))
           (swap! monsters conj {:at id
                                 :x 0
                                 :y 0
                                 :r 90
                                 :walk-for-ticks 0})))
       @monsters)

     :effect-graph eg}))

(def simulation-state (r/atom (-> (initial-simulation-state test-program)
                                  (assoc :doors #{}))))

(program->effect-graph test-program)

@simulation-state

(defn advance-simulation-state []
  (let [play-sound (fn [_])]
    (swap! simulation-state (fn [state]
                              (tick-simulation state play-sound)))))

(defn click-on-id [clicked-id]
  (println "CLICK ON" clicked-id)
  (swap! simulation-state (fn [state]
                            (update state :doors clojure.set/union (->> clicked-id (get (-> state :effect-graph)) :opens)))))

(def click-on-id-m (memoize (fn [id] (fn [] (click-on-id id)))))

(defn start-simulation-animation []
  (js/setInterval (fn []
                    (advance-simulation-state)
                    (advance-simulation-state)
                    (advance-simulation-state)
                    (advance-simulation-state))
                  16))
(defn app []
  [:div
   [:button {:on-click start-simulation-animation} "Advance"]
   [:p]
   (let [state @simulation-state
         doors (-> state :doors)
         monster-at (into {} (map (fn [m] [(:at m) m]) (-> state :monsters)))
         effect-graph (-> state :effect-graph)]
     [:svg {:width 1600 :height 1024}
      (into [:g {:transform (str "scale(0.25)")}]
            (for [[id {:keys [type]}] effect-graph]
              (let [boxes-per-row 32
                    box-x (mod id boxes-per-row)
                    box-y (quot id boxes-per-row)

                    box-x (* box-x 96)
                    box-y (* box-y 80)
                    
                    box-x (+ box-x 16)
                    ]
                ^{key id} [:g {:transform (str "translate(" box-x "," box-y ")")}
                 
                 ;; doors
                 (when (= type :tele2)
                   [:g
                    [:rect {:x -12 :y 0 :width 12 :height 64 :fill "#88c"}]
                    [:rect {:x 64 :y 0 :width 12 :height 64 :fill "#88c"}]

                    (when-not (doors [:0 id])
                      [:rect {:x -4 :y 0 :width 4 :height 64 :fill  "#000"}])
                    
                    (when-not (doors [:1 id])
                      [:rect {:x 64 :y 0 :width 4 :height 64 :fill  "#000"}])])
                 
                 ;; room
                 [:rect {:x 0 :y 0 :width 64 :height 64
                         :on-click (click-on-id-m id)
                         :style {:cursor "pointer"}
                         :fill (cond
                                 (= type :tele2)
                                 "#88c"

                                 (= type :droom)
                                 "#ccc"

                                 (= type :external-teleporter)
                                 "#bfb"

                                 :else
                                 "#822")}]
                 [:text {:y 64} (str id)]
                 (when-let [monster (get monster-at id)]
                   [:circle {:cx (-> monster :x (+ 32))
                             :cy (-> monster :y (+ 32))
                             :r 5}])])))]
     #_[:pre (with-out-str (cljs.pprint/pprint state))])
   
   #_[:pre (with-out-str (cljs.pprint/pprint (program->effect-graph #_program->adjacency-list test-program)))]])

#_(defn app []
  (let [layout  (layout-program test-program)
        id->map (program->id-map test-program)
        id->xy (make-id->xy layout)
        ids (sort (keys id->xy))

        main-entities (atom [])
        arrows (atom [])
        io-entities (atom [])]

    (doseq [id ids]
      (let [[x y] (id->xy id)
            info (id->map id)]
        (case (:type info)
          :tele2
          (let []
            (swap! main-entities conj
                   [tele2-component x y false false])
            (when-let [[dx dy] (-> info :on0 id->xy)]
              (let [[sx sy] (translate x y -18 0)]
                (swap! arrows conj
                       [directed-curve-component sx sy dx dy])))

            (when-let [[dx dy] (-> info :on1 id->xy)]
              (let [[sx sy] (translate x y 18 0)]
                (swap! arrows conj
                       [directed-curve-component sx sy dx dy])))

            (when-let [[sx sy] (-> info :door0 id->xy)]
              (let [[dx dy] (translate x y -12 -10)]
                (swap! arrows conj
                       [directed-curve-component sx sy dx dy])))

            (when-let [[sx sy] (-> info :door1 id->xy)]
              (let [[dx dy] (translate x y 12 -10)]
                (swap! arrows conj
                       [directed-curve-component sx sy dx dy]))))

          :external-switch
          (swap! io-entities conj
                 [:circle {:cx x :cy y :r 8 :fill "#66c"}])

          :external-teleporter
          (let [size 24]
            (swap! io-entities conj
                   [:rect {:x (- x (/ size 2)) :y (- y (/ size 2)) :width size :height size :fill "#282"}]))

          ;; Unknown
          (swap! main-entities conj
                 [:circle {:cx x :cy y :r 6 :fill "#ccc"}]))))

    [:div
     [:div (str layout)]
     [:div (str test-program)]
     [:svg {:width 1400 :height 2400}
      [:defs
       [:marker {:id "head" :orient "auto" :marker-width 2 :marker-height 4 :ref-x 0 :ref-y 2}
        [:path {:d "M0,0 V4 L2,2 Z" :fill "black"}]]]

      [into [:<>] @main-entities]
      [into [:<>] @arrows]
      [into [:<>] @io-entities]]]))



(defn ^:dev/after-load start []
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init []
  (println "Hello!")
  (start))
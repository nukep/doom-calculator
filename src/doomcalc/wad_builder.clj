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
  `(binding [w/*CTX* (w/new-wad-builder)]
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

(defn add-thing [thing-info]
  (let [thing-defaults {:x 0
                        :y 0
                        :angle 0
                        ;; health potion
                        :type 0x7DE}]
    (add-record :things (merge thing-defaults
                                   (select-keys thing-info
                                                (keys thing-defaults))))))

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

(defn draw-poly [& points]
  (doseq [i (range (dec (count points)))]
    (let [[x0 y0] (nth points i)
          [x1 y1] (nth points (inc i))
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
                   :back-sidedef back}))))

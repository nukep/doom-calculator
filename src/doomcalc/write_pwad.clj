(ns doomcalc.write-pwad
  (:require [doomcalc.write-primitives :as w]))

(def things-schema
  [[:s16 :x]
   [:s16 :y]
   [:s16 :angle]
   [:u16 :type]
   ;; flags: all skill levels
   [:u16 (fn [_] 0x0007)]])


(def sectors-schema
  [[:s16  :floor-height]
   [:s16  :ceil-height]
   [:str8 :floor-tex]
   [:str8 :ceil-tex]
   [:u16  :light]
   ;; sector type (or "special")
   [:u16  (fn [_] 0)]
   [:u16  :tag]])


(def linedefs-schema
  [[:u16 :v1]
   [:u16 :v2]
   [:u16 :flags]
   [:u16 :special]
   [:u16 :sector-tag]
   [:u16 :front-sidedef]
   [:u16 :back-sidedef]])


(def sidedefs-schema
  [[:s16  :xoff]
   [:s16  :yoff]
   [:str8 :upper-tex]
   [:str8 :lower-tex]
   [:str8 :middle-tex]
   [:u16  :sector]])


(def vertexes-schema
  [[:s16 (fn [[x _]] x)]
   [:s16 (fn [[_ y]] y)]])


(defn write-schema-to-vec [schema records]
  (let [v (transient [])
        byte-out (fn [b] (conj! v (int b)))]
    (w/write-schema schema records byte-out)
    (persistent! v)))


(defn write-pwad [byte-out {:keys [sectors vertexes sidedefs linedefs things]}]
  (let [lumps           [["MAP01" []]     ; map marker
                         ["THINGS"   (write-schema-to-vec things-schema   things)]
                         ["LINEDEFS" (write-schema-to-vec linedefs-schema linedefs)]
                         ["SIDEDEFS" (write-schema-to-vec sidedefs-schema sidedefs)]
                         ["VERTEXES" (write-schema-to-vec vertexes-schema vertexes)]
                         ["SECTORS"  (write-schema-to-vec sectors-schema  sectors)]]

        total-lump-size (->> lumps
                             (map second)
                             (map count)
                             (reduce +))]

    ;; Magic header
    (byte-out (int \P)) (byte-out (int \W)) (byte-out (int \A)) (byte-out (int \D))

    ;; Number of lumps
    (w/output-u32 byte-out (count lumps))

    ;; Directory offset
    (w/output-u32 byte-out (+ 12 total-lump-size))

    ;; Lump data
    (doseq [[lump-name lump-data] lumps
            value                 lump-data]
      (byte-out value))

    ;; Directory starts here
    (let [offset (atom 12)]
      (doseq [[lump-name lump-data] lumps]
        ;; location of lump data in file
        (w/output-u32 byte-out @offset)
        ;; size of lump data
        (w/output-u32 byte-out (count lump-data))
        ;; name of lump
        (w/output-str8 byte-out lump-name)

        ;; increase offset
        (swap! offset #(+ % (count lump-data)))))))

(defn spit-pwad [file-path data]
  (with-open [out-file (clojure.java.io/output-stream file-path)]
    (let [byte-out (fn [b] (.write out-file (int b)))]
      (write-pwad byte-out data))))

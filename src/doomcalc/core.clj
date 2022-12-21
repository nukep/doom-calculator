(ns doomcalc.core
  (:require [doomcalc.write-pwad :refer [spit-pwad]]
            [doomcalc.wad-builder :as w]
            [doomcalc.machine-builder :refer [compile-machines]]
            [doomcalc.components :as c]))

(defn level []
  (let [di01 (c/digit-input-and-display {:x 388 :y 400})
        di00 (c/digit-input-and-display {:x 738 :y 400})
        di11 (c/digit-input-and-display {:x 1588 :y 400})
        di10 (c/digit-input-and-display {:x 1938 :y 400})

        addm (c/bcd-adding-machine [(:vars di01) (:vars di00)]
                                   [(:vars di11) (:vars di10)])]
    [(c/player 688 64 90)
     di01 di00
     di11 di10
     addm
     (c/glyph '[[_ _ x _ _]
                [_ _ x _ _]
                [x x x x x]
                [_ _ x _ _]
                [_ _ x _ _]]
              {:x 1138 :y 1536})
     (c/glyph '[[_ _ _ _ _]
                [x x x x x]
                [_ _ _ _ _]
                [x x x x x]
                [_ _ _ _ _]]
              {:x 2336 :y 1536})
     (c/digit-carry-display {:x 2788 :y (+ 400 768)
                             :bit (-> addm :vars :carry)
                             :base-floor-height 64})
     (c/digit-display {:x 3088 :y (+ 400 768)
                       :bits (-> addm :vars :sum (nth 0))
                       :base-floor-height 64})
     (c/digit-display {:x 3388 :y (+ 400 768)
                       :bits (-> addm :vars :sum (nth 1))
                       :base-floor-height 64})]))


(defn summarize-wad-data [data]
  (println (str " Vertex count: " (count (:vertexes data))))
  (println (str "Linedef count: " (count (:linedefs data))))
  (println (str "Sidedef count: " (count (:sidedefs data))))
  (println (str " Sector count: " (count (:sectors data))))
  (println (str "  Thing count: " (count (:things data)))))

(defn -main []
  (w/with-debug-svg
    (compile-machines level)
    (summarize-wad-data (w/wad-data))
    (spit-pwad "out.wad" (w/wad-data))))


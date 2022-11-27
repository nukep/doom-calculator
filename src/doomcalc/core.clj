(ns doomcalc.core
  (:require [doomcalc.write-pwad :refer [spit-pwad]]
            [doomcalc.wad-builder :as w]
            [doomcalc.machine-builder :refer [compile-machines]]
            [doomcalc.components :as c]))

(defn level []
  (let [di01 (c/digit-input-and-display {:x 100 :y 400})
        di00 (c/digit-input-and-display {:x 450 :y 400})
        di11 (c/digit-input-and-display {:x 1300 :y 400})
        di10 (c/digit-input-and-display {:x 1650 :y 400})

        addm (c/bcd-adding-machine [(:vars di01) (:vars di00)]
                                   [(:vars di11) (:vars di10)])]
    [(c/player 512 192 90)
     di01 di00
     di11 di10
     addm
     (c/glyph '[[_ _ x _ _]
                [_ _ x _ _]
                [x x x x x]
                [_ _ x _ _]
                [_ _ x _ _]]
              {:x 850 :y 1024})
     (c/glyph '[[_ _ _ _ _]
                [x x x x x]
                [_ _ _ _ _]
                [x x x x x]
                [_ _ _ _ _]]
              {:x 2048 :y 1024})
     (c/digit-carry-display {:x 2500 :y (+ 400 768)
                             :bit (-> addm :vars :carry)
                             :base-floor-height 64})
     (c/digit-display {:x 2800 :y (+ 400 768)
                       :bits (-> addm :vars :sum (nth 0))
                       :base-floor-height 64})
     (c/digit-display {:x 3100 :y (+ 400 768)
                       :bits (-> addm :vars :sum (nth 1))
                       :base-floor-height 64})]))

(defn testlevel []
  (let [d1 (c/binary-4-input {:x 64 :y 200})]
    [(c/player 512 192 90)
     d1
     (c/digit-carry-display {:x 128 :y (+ 400 512)
                             :bit (-> d1 :vars (nth 0))
                             :base-floor-height 64})]))

(defn -main []
  (w/with-debug-svg
    (compile-machines level)
    (doomcalc.write-pwad/spit-pwad "out.wad" (w/wad-data))))

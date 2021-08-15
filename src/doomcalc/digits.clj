(ns doomcalc.digits)

(def digit-width 4)
(def digit-height 7)
(def digit-positions (* digit-width digit-height))

(def digits
  '{0 [_ x x _
       x _ _ x
       x _ _ x
       x _ _ x
       x _ _ x
       x _ _ x
       _ x x _]

    1 [_ _ x _
       _ x x _
       _ _ x _
       _ _ x _
       _ _ x _
       _ _ x _
       _ x x x]

    2 [_ x x _
       x _ _ x
       _ _ _ x
       _ _ x _
       _ x _ _
       x _ _ _
       x x x x]

    3 [_ x x _
       x _ _ x
       _ _ _ x
       _ x x _
       _ _ _ x
       x _ _ x
       _ x x _]

    4 [_ _ _ x
       x _ _ x
       x _ _ x
       x x x x
       _ _ _ x
       _ _ _ x
       _ _ _ x]

    5 [x x x x
       x _ _ _
       x _ _ _
       x x x _
       _ _ _ x
       _ _ _ x
       x x x _]

    6 [_ x x _
       x _ _ x
       x _ _ _
       x x x _
       x _ _ x
       x _ _ x
       _ x x _]

    7 [x x x x
       _ _ _ x
       _ _ _ x
       _ _ x _
       _ _ x _
       _ x _ _
       _ x _ _]

    8 [_ x x _
       x _ _ x
       x _ _ x
       _ x x _
       x _ _ x
       x _ _ x
       _ x x _]

    9 [_ x x _
       x _ _ x
       x _ _ x
       _ x x x
       _ _ _ x
       x _ _ x
       _ x x _]

    10 [_ _ _ _
        _ x x _
        x _ _ x
        _ x x x
        x _ _ x
        x _ _ x
        _ x x x]

    11 [x _ _ _
        x _ _ _
        x _ _ _
        x x x _
        x _ _ x
        x _ _ x
        x x x _]

    12 [_ _ _ _
        _ _ _ _
        _ _ _ _
        _ x x _
        x _ _ _
        x _ _ _
        _ x x _]

    13 [_ _ _ x
        _ _ _ x
        _ _ _ x
        _ x x x
        x _ _ x
        x _ _ x
        _ x x x]

    14 [_ _ _ _
        _ _ _ _
        _ x x _
        x _ _ x
        x x x x
        x _ _ _
        _ x x x]

    15 [_ x x _
        x _ _ x
        x _ _ _
        x x x _
        x _ _ _
        x _ _ _
        x _ _ _]})
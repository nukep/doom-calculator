(defproject doomcalc "1.0.0"
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :repl-options {:init-ns doomcalc.core}
  :source-paths ["src"]
  
  :cljfmt {:indents {doomcalc.core/remove-and-replace [[:inner 0]]}})
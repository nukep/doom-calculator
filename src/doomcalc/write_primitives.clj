(ns doomcalc.write-primitives)

;; Takes an ASCII string, and returns a zero-padded sequence of bytes that's always n items long.
(defn str->null-terminated-seq [n s]
  (let [s-seq (seq (.getBytes s "ascii"))]
    (take n (concat s-seq (repeat 0)))))


(defn output-u8 [byte-out value]
  (assert (and (integer? value)
               (<= 0 value 255)))
  (byte-out value))


(defn output-u16 [byte-out value]
  (assert (and (integer? value)
               (<= 0 value 65535)))
  ;; little-endian
  (byte-out (bit-and value 255))
  (byte-out (bit-shift-right value 8)))


(defn output-s16 [byte-out value]
  (assert (and (integer? value)
               (<= -32768 value 32767)))
  ;; little-endian
  (byte-out (bit-and value 255))
  (byte-out (bit-shift-right value 8)))


(defn output-u32 [byte-out value]
  (assert (and (integer? value)
               (<= 0 value 0xFFFFFFFF)))
  ;; little-endian
  (byte-out (bit-and value 255))
  (byte-out (bit-and (bit-shift-right value 8)  255))
  (byte-out (bit-and (bit-shift-right value 16) 255))
  (byte-out (bit-and (bit-shift-right value 24) 255)))


(defn output-str8 [byte-out value]
  (assert (and (string? value)
               (<= (count value) 8)))
  (dorun (map byte-out (str->null-terminated-seq 8 value))))


(defn write-schema [schema records byte-out]
  (doseq [record records
          [type f] schema]
    (let [value (f record)]
      (when (nil? value)
        (throw (ex-info (str "Missing value in record") {:f f :schema schema})))
      (case type
        :u8  (output-u8 byte-out value)
        :u16 (output-u16 byte-out value)
        :s16 (output-s16 byte-out value)
        :str8 (output-str8 byte-out value)))))

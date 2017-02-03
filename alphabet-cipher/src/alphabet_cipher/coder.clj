(ns alphabet-cipher.coder)

(def alphabet (vec "abcdefghijklmnopqrstuvwxyz"))

(def indexed-alphabet (zipmap alphabet (iterate inc 0)))

(defn char->index [c]
  (indexed-alphabet c))

(defn index->char [i]
  (alphabet i))

(defn encode-char [c secret]
  (let [col (char->index secret)
        row (char->index c)
        encoded-index (mod (+ row col) (count alphabet))]
    (index->char encoded-index)))

(defn encode [keyword message]
  (apply str (map encode-char message (cycle keyword))))

(defn decode-char [c secret]
  (let [col (char->index secret)
        row (char->index c)
        decoded-index (mod (+ row (- (count alphabet) col)) (count alphabet))]
    (index->char decoded-index)))

(defn decode [keyword message]
  (apply str (map decode-char message (cycle keyword))))

(defn secret-char [original cipher]
  (index->char (mod (- (char->index cipher) (char->index original)) (count alphabet))))

(defn full-secret [original cipher]
  (map secret-char original cipher))

(defn encodes? [secret original cipher]
  (= (encode secret original) cipher))

(defn pyramid [tail]
  (rest (reductions conj [] tail)))

(defn secret-candidates [secret message cipher]
  (filter #(encodes? (apply str %) message cipher) (pyramid secret)))

(defn decipher [cipher message]
  (let [secret (full-secret message cipher)]
    (->> (secret-candidates secret message cipher) first (apply str))))

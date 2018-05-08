(ns music-scales.core)

(def chromatic-flats ["A" "Bb" "B" "C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab"])
(def chromatic-sharps ["A" "A#" "B" "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#"])

(def w 2) ;; whole step
(def h 1) ;; half step

(def major-scale [w w h w w w h])

(defn steps->notes
  ([root steps]
   (steps->notes root steps false))
  ([root steps sharps?]
   (let [chromatics (cycle (if sharps? chromatic-sharps chromatic-flats))]
     (loop [notes []
            remaining (take 12 (drop-while #(not= root %) chromatics))
            steps steps]
       (if (empty? steps)
         notes
         (recur (conj notes (first remaining))
                (drop (first steps) remaining)
                (rest steps)))))))

(defn scale->thirds [scale]
  (flatten (take 7 (partition 1 2 (cycle scale)))))

(defn scale->modes [scale]
  (take 7 (partition 7 1 (cycle scale))))

(defn scale->chords [scale]
  (take 7 (map scale->thirds (scale->modes scale))))

(def P1 0)
(def m2 1)
(def M2 2)
(def m3 3)
(def M3 4)
(def P4 5)
(def A4 6)
(def d5 6)
(def P5 7)
(def m6 8)
(def M6 9)
(def m7 10)
(def M7 11)

(defn intervals->notes [root intervals]
  (let [chromatic (vec (take 12 (drop-while #(not= root %) (cycle chromatic-flats))))]
    (concat [(first chromatic)] (map #(get chromatic %) intervals))))

(comment
  (intervals->notes "C" [m3 d5 m2])
  (def c (steps->notes "C" major-scale))
  (scale->modes c)
  (scale->chords c)
  (steps->notes "F" major-scale)
  (-> "F"
      (steps->notes major-scale)
      scale->modes)
  (steps->notes "E" major-scale true)
  (steps->notes "A" major-scale true)
  :end)



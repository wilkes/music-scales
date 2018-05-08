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

(comment
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



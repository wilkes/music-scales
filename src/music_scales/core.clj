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
   (loop [notes []
          remaining (take 12 (drop-while #(not= root %) (cycle (if sharps?
                                                                 chromatic-sharps
                                                                 chromatic-flats))))
          steps steps]
     (if (empty? steps)
       notes
       (recur (conj notes (first remaining))
              (drop (first steps) remaining)
              (rest steps))))))

(comment
  (steps->notes "C" major-scale)
  (steps->notes "F" major-scale)
  (steps->notes "E" major-scale true)
  (steps->notes "A" major-scale true)
  :end)



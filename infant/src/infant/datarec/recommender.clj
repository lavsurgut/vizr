(ns infant.datarec.recommender)


(defn count-frequences-by-key
  [k data]
  (->>
    (reduce (fn [x y] (conj x (k y))) [] data)
    (frequencies))
  )


(defn count-frequences
  [data]
  (->>
    (map (fn [k] {k (count-frequences-by-key k data)})
         (keys (first data)))
    ))

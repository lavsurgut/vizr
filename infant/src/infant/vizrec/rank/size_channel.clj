(ns infant.vizrec.rank.size-channel)


(defn- set-name
  [mark channel]
  (str mark "-" channel))


(defn get-score
  [spec]
  (let [fields (::sp/fields spec)
        mark (::sp/mark spec)
        scores {(set-name ::sp/bar ::sp/size)  -2
                (set-name ::sp/tick ::sp/size) -2}
        initial-score 0]
    (reduce (fn [x y]
              (+ x (get scores (set-name mark (::sp/channel y)) initial-score)))
            initial-score fields)))

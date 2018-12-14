(ns infant.misc.funcs)


(defn group-data [& names]
  (apply concat (for [n names]
                  (map-indexed (fn [i x] {:x i :y x :col n}) (take 20 (repeatedly #(rand-int 100)))))))


(defn cartesian-join [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
          more (cartesian-join (rest colls))]
      (cons x more))))

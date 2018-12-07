(ns infant.vizrec.schema)


(defn find-value-types
  ([data] (find-value-types (first data) {}))
  ([map result]
   (if (empty? map)
     result
     (let [key (first (keys map))
           type (type (first (vals map)))]
       (recur (rest map) (assoc result key type))))))

(defn find-cardinalities
  ([data]
   (->>
     (map (fn [k] {k (reduce (fn [x y] (conj x (k y))) [] data)} )
          (keys (first data)))
     (map (fn [rec] {(key (first rec)) (count (distinct (val (first rec))))}))
     (reduce (fn [x y] (conj x y))))))

(defn make-schema
  [types cardinalities]
  (->>
    (map (fn [k] {k {:type (k types) :cardinality (k cardinalities)}})
         (keys types))
    (reduce (fn [x y] (conj x y)))))

(defn get-schema
  [data]
  (let [types (find-value-types data)
        cardinalities (find-cardinalities data)
        schema (make-schema types cardinalities)]
    schema))

(ns heap.core)

(defn swap [coll a b]
  (assoc coll
         a (nth coll b)
         b (nth coll a)))

(defn heapify [coll i]
  (let [left (inc (* 2 i))
        right (inc left)
        nodes (->> [i left right]
                   (#(zipmap % (map (partial get coll) %)))
                   (filter (comp not nil? second)))]
    (if (empty? nodes) coll
        (let [largest-index (first (apply max-key second nodes))]
          (if (= i largest-index)
            coll
            (recur (swap coll i largest-index) largest-index))))))

(defn heap [coll]
  (loop [i (dec (quot (count coll) 2))
         coll coll]
    (if (neg? i)
      coll
      (recur (dec i) (heapify coll i)))))

(defn heapsort [coll]
  (let [h (heap coll)]
    (loop [i (dec (count coll))
           h h]
      (if (neg? i)
        h
        (let [h (swap h 0 i)
              left (subvec h 0 i)
              right (subvec h i)]
          (recur (dec i)
                 (apply conj (heapify left 0) right)))))))

(ns ^{:doc "Sequence utility methos"
      :author "Jeremy Bondeson"}
  data.seq)


(defn ^:private ^:static
  memoize-by-id
  "Memoizes based upon a supplied object rather than the whole argument list"
  [f]
  (let [mem (atom {})]
    (fn [id & args]
      (if-let [e (find @mem id)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc id ret)
          ret)))))

(defn ^:static
  seq-equals
  "Determines whether two sequences expand equally"
  [a b]
  (boolean
   (when (or (sequential? b) (instance? java.util.List b))
     (loop [a (seq a), b (seq b)]
       (when (= (nil? a) (nil? b))
         (or (nil? a)
             (when (= (first a) (first b))
               (recur (next a) (next b)))))))))

(def ^{:doc "Generates a hash code for a sequence based upon a unique-id."
       :static true}
  seq-hash
  (memoize-by-id
   (fn [coll]
     (reduce (fn [h x] (unchecked-add-int
                       (unchecked-multiply-int h 31)
                       (hash x)))
             1 coll))))

(def ^{:doc "Generates a count for a sequence based upon a unique-id."
       :static true}
  seq-count
  (memoize-by-id
   (fn [coll]
     (reduce (fn [x _] (inc x)) 0 coll))))

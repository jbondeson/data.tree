(ns ^{:doc "IFn Interface Helpers"
      :author "Jeremy Bondeson"}
  data.ifn
  (:import (clojure.lang IFn ArityException)))

(defmacro throw-arity
  [this arity]
  `(let [classname#  (-> ~this .getClass .getSimpleName)
         suf-idx#    (.lastIndexOf classname# "__")
         normalized# (if (= suf-idx# -1)
                       classname#
                       (-> name (.substring 0 suf-idx#) (.replace "_" "-")))]
       (throw (ArityException. ~arity normalized#))))

(defn- parse-invokes
  [invmap]
  (let [params (fn [n] (map #(symbol (str "p" %)) (range 1 (inc n))))
        forms (fn [n] (if-let [form (get invmap (keyword (str n)))]
                       form
                       `(~'invoke [~'this ~@(params n)] (throw-arity ~'this ~n))))]
    (map forms (range 0 22))))

;; Crude... but effective.
(defmacro deftype&ifn
  [name fields invokes & opts+specs]
  `(deftype ~name ~fields
     clojure.lang.IFn
     ~@(parse-invokes invokes)
     ~@opts+specs))
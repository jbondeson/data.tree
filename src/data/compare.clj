(ns ^{:doc "Comparison helpers"
      :author "Jeremy Bondeson"}
  data.compare
  (:refer-clojure :exclude [comparator comp]))

(set! *warn-on-reflection* true)

(defmacro with-compare
  "Macro to automate the binding of a comparator result"
  ([^java.util.Comparator comparator result lhs rhs & body]
     `(let [^int ~result (int (.compare ~comparator ~lhs ~rhs))]
        ~@body)))

(def ^{:doc "Default comparator"
       :static true}
  default
  clojure.lang.RT/DEFAULT_COMPARATOR)
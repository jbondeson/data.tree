(ns ^{:doc "Reference object to data structures"
      :author "Jeremy Bondeson"}
    data.util.tref
    (:import (data.util ThreadRef)))

(set! *warn-on-reflection* true)

(defn thread-ref
  ^{:inline (fn [v] `(ThreadRef. v))
    :inline-arities #{1}}
  [value]
  (ThreadRef. value))

(defn set!
  ^{:inline (fn [r v] `(.set r v))
    :inline-arities #{2}}
  [^data.util.ThreadRef ref value]
  (.set ref value))

(defn freeze!
  ^{:inline (fn [r] `(.freeze r))
    :inline-arities #{1}}
  [^data.util.ThreadRef ref]
  (.freeze ref))

(comment
  (let [r (thread-ref 1)]
    (prn @r)
    (ref-set! r 2)
    (prn @r)))
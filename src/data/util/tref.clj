(ns ^{:doc "Reference object to data structures"
      :author "Jeremy Bondeson"}
    data.util.tref
    (:import (data.util ThreadBoundRef)))

(set! *warn-on-reflection* true)

(defn thread-bound-ref ^data.util.ThreadBoundRef
  ^{:inline (fn [v] `(ThreadBoundRef. v))
    :inline-arities #{1}}
  [value]
  (ThreadBoundRef. value))

(defn set!
  ^{:inline (fn [^data.util.ThreadBoundRef r v] `(.set r v))
    :inline-arities #{2}}
  [^data.util.ThreadBoundRef ref value]
  (.set ref value))

(defn freeze!
  ^{:inline (fn [^data.util.ThreadBoundRef r] `(.freeze r))
    :inline-arities #{1}}
  [^data.util.ThreadBoundRef ref]
  (.freeze ref))

(comment
  (let [r (thread-ref 1)]
    (prn @r)
    (ref-set! r 2)
    (prn @r)))
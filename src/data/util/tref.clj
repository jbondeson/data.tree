(ns ^{:doc "Reference object to data structures"
      :author "Jeremy Bondeson"}
    data.util.tref
    (:import (data.util ThreadBoundRef EditContext)))

(set! *warn-on-reflection* true)

(defn edit-context
  ^{:inline (fn [] `(EditContext.))
    :inline-arities #{0}}
  []
  (EditContext.))

(defn persist!
  [^EditContext edit]
  (.persist edit))

(defn thread-bound-ref ^ThreadBoundRef
  ^{:inline (fn [v e] `(ThreadBoundRef. v e))
    :inline-arities #{2}}
  ([value]
     (ThreadBoundRef. value))
  ([value ^EditContext edit]
     (ThreadBoundRef. value edit)))

(defn set!
  ^{:inline (fn [^ThreadBoundRef r v] `(.set r v))
    :inline-arities #{2}}
  [^ThreadBoundRef ref value]
  (.set ref value))

(defn editable?
  [^ThreadBoundRef ref]
  (.isEditable ref))

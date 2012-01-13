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

(defn thread-bound-ref ^data.util.ThreadBoundRef
  ^{:inline (fn [v e] `(ThreadBoundRef. v e))
    :inline-arities #{2}}
  ([value]
     (ThreadBoundRef. value))
  ([value ^data.util.EditContext edit]
     (ThreadBoundRef. value edit)))

(defn set!
  ^{:inline (fn [^data.util.ThreadBoundRef r v] `(.set r v))
    :inline-arities #{2}}
  [^data.util.ThreadBoundRef ref value]
  (.set ref value))

(defn editable?
  [^data.util.ThreadBoundRef ref]
  (.isEditable ref))
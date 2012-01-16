(ns ^{:doc "Reference object to data structures"
      :author "Jeremy Bondeson"}
  data.util.tref
  (:refer-clojure :exclude [persistent!])
  (:import (data.util ThreadBoundRef EditContext)))

(set! *warn-on-reflection* true)

(defn edit-context
  ^{:inline (fn [] `(EditContext.))
    :inline-arities #{0}}
  []
  (EditContext.))

(defn persistent!
  [^EditContext edit]
  (.persistent edit))

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


(defn ensure-editable
  [ref-or-edit]
    (let [type (type ref-or-edit)]
    (cond
     (isa? type EditContext)    (.ensureEditable ^EditContext ref-or-edit)
     (isa? type ThreadBoundRef) (.ensureEditable ^ThreadBoundRef ref-or-edit))))

(defn editable?
  [ref-or-edit]
  (let [type (type ref-or-edit)]
    (cond
     (isa? type EditContext)    (.isEditable ^EditContext ref-or-edit)
     (isa? type ThreadBoundRef) (.isEditable ^ThreadBoundRef ref-or-edit))))

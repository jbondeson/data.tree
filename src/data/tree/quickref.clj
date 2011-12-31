(ns data.tree.quickref
  (:refer-clojure :exclude [set! ref deref]))

(defn ref
  "Creates and returns a quick (non-stm controlled) reference.
  NOTE: These object should never leak outside their scope."
  [^Class type]
  (make-array type 1))

(defn deref
  "Dereferences a quick reference and returns the current value."
  [ref]
  (clojure.core/aget ref 0))

(defn set!
  "Updates the value of a quick reference."
  [ref val]
  (clojure.core/aset ref 0 val))
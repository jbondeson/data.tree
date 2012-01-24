(ns ^{:doc "Transient helpers"
      :author "Jeremy Bondeson"}
  data.transient
  (:import (java.util.concurrent.atomic AtomicReference)))

(set! *warn-on-reflection* true)

(defprotocol ITransient
  (ensure-editable [this ^AtomicReference edit]))

(defn edit-context
  ^{:inline (fn [] `(AtomicReference. (Thread/currentThread)))
    :inline-arities #{0}}
  []
  (AtomicReference. (Thread/currentThread)))
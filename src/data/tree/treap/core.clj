(ns ^{:doc "Persistent Treap"
      :author "Jeremy Bondeson"}
  data.tree.treap.core
  (:refer-clojure :exclude [comparator comp merge])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:use [data.tree.printable])
  (:require [data.compare :as cmp])
  (:import (java.util Comparator)))

(set! *warn-on-reflection* true)

(defprotocol INode
  (^data.tree.treap.core.INode
    insert [this item priority ^Comparator comp])
  (^data.tree.treap.core.INode
    delete [this item ^Comparator comp])
  (^data.tree.treap.core.INode
    rotate-left [this])
  (^data.tree.treap.core.INode
    rotate-right [this])
  (retrieve [this item ^Comparator comp])
  (^data.tree.treap.core.INode
    left [node])
  (^data.tree.treap.core.INode
    right [node])
  (priority [node])
  (value [node]))

#_(def transient-type ::transient)

#_(def persistent-type ::persistent)

#_(derive clojure.lang.IPersistentCollection persistent-type)

#_(derive clojure.lang.ITransientCollection transient-type)

(defmulti
  build-tree
  "Takes a collection of values and returns a vector containing the
tree and its size (which may be different if duplicates exist in the input"
  (fn [type & _] (identity type)))

(defmethod build-tree :default
  [_ vals comparator & options]
  (apply build-tree :polymorphic vals comparator options))
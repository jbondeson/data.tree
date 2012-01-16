(ns ^{:doc "Persistent Treap"
      :author "Jeremy Bondeson"}
  data.tree.treap.core
  (:refer-clojure :exclude [comparator comp])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:use [data.tree.printable])
  (:require [data.compare :as cmp])
  (:import (java.util Comparator)))

(set! *warn-on-reflection* true)

(defprotocol INode
  (^data.tree.treap.core.INode
    insert [this item ^double priority ^Comparator comp])
  (^data.tree.treap.core.INode
    delete [this item ^Comparator comp])
  (retrieve [this item ^Comparator comp])
  (^data.tree.treap.core.INode
    left [node])
  (^data.tree.treap.core.INode
    right [node])
  (^double priority [node])
  (value [node]))

(defrecord LeafNode
    [value ^double priority])

(defrecord LeftyNode
    [value ^double priority ^data.tree.treap.core.INode left])

(defrecord RightyNode
    [value ^double priority ^data.tree.treap.core.INode right])

(defrecord FullNode
    [value ^double priority ^data.tree.treap.core.INode left
     ^data.tree.treap.core.INode right])

(defn- ^:static const-nil [_] nil)

;;-- Leaf Node INode Implementation
(defn- ^data.tree.treap.core.INode
  leaf-insert
  [^LeafNode node item ^double priority ^Comparator comp]
  nil
  )
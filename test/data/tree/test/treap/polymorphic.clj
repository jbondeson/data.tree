(ns ^{:doc "Tests for the treap data structure"
      :author "Jeremy Bondeson"}
  data.tree.test.treap.polymorphic
  (:refer-clojure :exclude [merge])
  (:use [data.tree.treap])
  (:use [data.tree.treap.core])
  (:use [data.tree.treap.polymorphic])
  (:use [data.tree.test.treap])
  (:use [clojure.test])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:require [clojurecheck.core :as cc])
  (:require [data.compare :as cmp])
  (:import (data.tree.treap.polymorphic LeafNode LeftyNode RightyNode FullNode)))

(defn build-node-tree-def
  [vals]
  (first (build-tree :polymorphic vals cmp/default)))

(node-tree-prop-tests
 node-tree-properties
 build-node-tree-def)

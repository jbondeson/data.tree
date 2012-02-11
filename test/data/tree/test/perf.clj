(ns data.tree.test.perf
  (:require [data.compare :as cmp])
  (:require [data.tree.bst :as bst])
  (:require [data.tree.bst.core :as bstcore])
  (:use [data.tree.treap.core])
  (:use [data.tree.treap.polymorphic])
  (:use [data.tree.treap.simple])
  (:use [data.tree.test.bst])
  (:use [data.tree.test.treap])
  (:use [clojure.test])
  (:import (data.tree.bst EmptyBinarySearchTree BinarySearchTree)))

;; Tree Ops
(defn tree-trans-node-time []
  (let [coll (balanced-seq 10000)] 
    (time (do (doall (apply build-trans-def coll)) nil))))

(defn tree-trans-time []
  (let [coll (balanced-seq 10000)] 
    (time (do (apply build-trans-tree-def coll) nil))))

(defn tree-time []
  (let [coll (balanced-seq 10000)] 
    (time (do (doall (apply build-def coll)) nil))))


(defn treap-time-poly []
  (let [coll (balanced-seq 10000)] 
    (time (do (build-tree :polymorphic coll cmp/default) nil))))


(defn treap-time-simple []
  (let [coll (balanced-seq 10000)] 
    (time (do (build-tree :simple coll cmp/default) nil))))


(defn tree-baseline []
  (let [coll (balanced-seq 10000)] 
    (time (do (doall (apply sorted-set coll)) nil))))
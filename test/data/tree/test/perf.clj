(ns data.tree.test.perf
  (:require [data.tree.bst :as bst])
  (:require [data.tree.bst.core :as bstcore])
  (:require [data.tree.treap.core :as treapcore])
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


(defn treap-time []
  (let [coll (balanced-seq 10000)] 
    (time (do (build-node-tree-def coll) nil))))


(defn tree-baseline []
  (let [coll (balanced-seq 10000)] 
    (time (do (doall (apply sorted-set coll)) nil))))
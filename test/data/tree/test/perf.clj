(ns data.tree.test.perf
  (:use [data.tree.bst])
  (:use [data.tree.bst.core])
  (:use [data.tree.test.bst])
  (:use [clojure.test])
  (:import (data.tree.bst EmptyBinarySearchTree BinarySearchTree)))

;; Tree Ops
(defn tree-trans-time []
  (let [coll (balanced-seq 10000)] 
    (time (do (doall (apply build-trans-def coll)) nil))))

(defn tree-time []
  (let [coll (balanced-seq 10000)] 
    (time (do (doall (apply build-def coll)) nil))))

(defn tree-baseline []
  (let [coll (balanced-seq 10000)] 
    (time (do (doall (apply sorted-set coll)) nil))))
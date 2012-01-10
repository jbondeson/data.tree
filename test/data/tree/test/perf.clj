(ns data.tree.test.perf
  (:use [data.tree.bst])
  (:use [data.tree.test.bst])
  (:use [clojure.test])
  (:require [data.tree.quickref :as qref])
  (:import (data.tree.bst EmptyBinarySearchTree BinarySearchTree
                          LeafNode LeftyNode RightyNode FullNode
                          TransNode
                          INode 
                          )))

;; Quick Reference Tests

(qref/def-create inode-ref INode)
(qref/def-deref inode-deref INode)
(qref/def-set inode-set! INode)

(defn ref-access-baseline []
  (let [a (into-array INode [nil])]
    ;; Warm-up Hotspot
    (dotimes [i 100000]
      (aget ^"[Ldata.tree.bst.INode;" a (int 0)))
    (time
     (dotimes [j 100000]
       (aget ^"[Ldata.tree.bst.INode;" a (int 0))))
    nil))

(defn ref-access-time []
  (let [a (inode-ref nil)]
    ;; Warm-up Hotspot
    (dotimes [i 100000]
      (inode-deref a))
    (time
     (dotimes [j 100000]
       (inode-deref a)))
    nil))

(defn ref-set-baseline []
  (let [a (into-array INode [nil])]
    ;; Warm-up Hotspot
    (dotimes [i 100000]
      (aset ^"[Ldata.tree.bst.INode;" a (int 0) nil))
    (time
     (dotimes [j 100000]
       (aset ^"[Ldata.tree.bst.INode;" a (int 0) nil)))
    nil))

(defn ref-set-time []
  (let [a (inode-ref nil)]
    ;; Warm-up Hotspot
    (dotimes [i 100000]
      (inode-set! a nil))
    (time
     (dotimes [j 100000]
       (inode-set! a nil)))
    nil))


(defn ref-create-baseline []
  ;;Warm-up Hotspot
  (dotimes [i 100000]
    (object-array [nil]))
  (time
   (dotimes [j 100000]
     (object-array [nil])))
  nil
  )

(defn ref-create-time []
  ;;Warm-up Hotspot
  (dotimes [i 100000]
    (inode-ref nil))
  (time
   (dotimes [j 100000]
     (inode-ref nil)))
  nil
  )

(comment
  (defn perf-ref-access-baseline ^"[Ldata.tree.bst.INode;"
    [array access-times set-times]
    (do
      (dotimes [i access-times]
        (aget ^"[Ldata.tree.bst.INode;"
              array (int 0))
        (dotimes [j set-times]
          (aset ^"[Ldata.tree.bst.INode;" array (int 0) nil)))
      nil))

  (defn do-stuff
    [creation-times access-times set-times]
    (do
      (dotimes [i creation-times]
        (buh-ref nil))
      (let [iref (buh-ref (LeafNode. 1))]
        (dotimes [j access-times]
          (buh-deref iref))
        (dotimes [k set-times]
          (buh-set! iref nil))))))

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
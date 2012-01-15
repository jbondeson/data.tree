(ns ^{:doc "Binary Search Tree Sequence Implementation"
      :author "Jeremy Bondeson"}
  data.tree.bst.seq
  (:use [data.seq :only [seq-equals seq-hash seq-count]])
  (:require data.tree.bst.core)
  (:import (clojure.lang IObj Seqable Sequential ISeq
                         IPersistentCollection Counted Sorted
                         IPersistentMap)
           (data.tree.bst.core INode)))

(set! *warn-on-reflection* true)

(defn- ^:static push-stack
  [^INode node ^ISeq stack asc]
  (let [f (if asc
            (fn [^INode x] (.left x))
            (fn [^INode x] (.right x)))]
    (loop [^INode t node
           ^ISeq s stack]
      (if (nil? t)
        s
        (recur (f t) (cons t s))))))

(deftype Seq [^IPersistentMap mdata ^ISeq stack asc cnt id]
  Object
  (equals [this x] (seq-equals this x))
  (hashCode [this] (seq-hash id this))
  IObj
  (meta [_] mdata)
  (withMeta [_ mdata] (Seq. mdata stack asc cnt id))
  Sequential
  Seqable
  (seq [this] this)
  ISeq
  (first [_] (let [^INode node (first stack)]
               (.value node)))
  (more [this] (or (next this) (list)))
  (next [_] (let [f (if asc
                      (fn [^INode x] (.right x))
                      (fn [^INode x] (.left x)))
                  t (first stack)
                  s (push-stack (f t) (next stack) asc)]
              (when (not (nil? s))
                (Seq. mdata s asc (if cnt (dec cnt) nil) (Object.)))))
  IPersistentCollection
  (count [this] (or cnt (seq-count id this)))
  (empty [_] (list))
  (equiv [this x] (.equals this x))
  (cons [this item] (cons item this)))

(defn
  make-seq
  ([^INode tree asc]
     (Seq. nil (push-stack tree nil asc) asc nil (Object.)))
  ([^INode tree asc cnt]
     (Seq. nil (push-stack tree nil asc) asc cnt (Object.)))
  ([mdata ^INode tree asc cnt]
     (Seq. mdata (push-stack tree nil asc) asc cnt (Object.)))
  ([mdata ^INode tree asc cnt id]
     (Seq. mdata (push-stack tree nil asc) asc cnt id)))

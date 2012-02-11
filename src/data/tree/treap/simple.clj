(ns ^{:doc "Simple Node Implementation of Persistent Treap"
      :author "Jeremy Bondeson"}
  data.tree.treap.simple
  (:refer-clojure :exclude [comparator comp merge])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:use [data.tree.printable])
  (:use [data.tree.treap.core])
  (:require [data.compare :as cmp])
  (:import (java.util Comparator)))

(set! *warn-on-reflection* true)

(declare merge)

(deftype Node
    [value
     priority
     ^data.tree.treap.core.INode left
     ^data.tree.treap.core.INode right]
  INode
  (insert [this item priority' comparator]
    (cmp/case ^Comparator comparator item value
              := (throw+ {:duplicate-key? true})
              :> (let [r (if right
                           (insert right item priority' comparator)
                           (Node. item priority' nil nil))
                       n (Node. value priority left r)]
                   (if (< priority' priority)
                     (rotate-left n)
                     n))
              :< (let [l (if left
                           (insert left item priority' comparator)
                           (Node. item priority' nil nil))
                       n (Node. value priority l right)]
                   (if (< priority' priority)
                     (rotate-right n)
                     n))))
  (delete [this item comparator]
    (cmp/case ^Comparator comparator item value
              := (merge left right)
              :> (if right
                   (Node. value priority left (delete right item comparator))
                   this)
              :< (if left
                   (Node. value priority (delete left item comparator) right)
                   this)))
  (rotate-left [this]
    (if right
      (Node. (.value right)
             (.priority right)
             (Node. value priority left (.left right))
             (.right right))
      this))
  (rotate-right [this]
    (if left
      (Node. (.value left)
             (.priority left)
             (.left left)
             (Node. value priority (.right left) right))
      this))
  (retrieve [this item comparator]
    (cmp/case ^Comparator comparator item value
              := value
              :> (when right (retrieve right item comparator))
              :< (when left (retrieve left item comparator))))
  (left [_] left)
  (right [_] right)
  (priority [_] priority)
  (value [_] value))

(defn- 
  merge
  [^data.tree.treap.core.INode fst
   ^data.tree.treap.core.INode snd
   ^Comparator comp]
  (if (and fst snd)
    (let [[^data.tree.treap.core.INode l
           ^data.tree.treap.core.INode r]
          (cmp/case comp (value fst) (value snd)
                    :< [fst snd]
                    :> [snd fst]
                    := (throw+ {:duplicate-key? true}))
          lp (priority l)
          rp (priority r)]
      (if (< lp rp)
        (Node. (value l) lp (left l) (merge (right l) r comp))
        (Node. (value r) rp (merge (left r) l comp) (right r))))
    (or fst snd)))

;;-- Creation

(defmethod build-tree :simple
  [type vals comparator & options]
  (when-let [coll (seq vals)]
    (let [[x & xs] coll
          root (Node. x (rand) nil nil)
          ins (fn [[^INode t c :as whole] v]
                (try+
                 [(insert t v (rand) comparator) (inc c)]
                 (catch :duplicate-key? _
                   whole)))]
      (reduce ins [root 1] xs))))

;;-- Pretty Printing

(extend-protocol PrintableTree
  Node
  (print-tree [x] (prtree (str "Node p" (.priority x)) (.value x) (.left x) (.right x))))
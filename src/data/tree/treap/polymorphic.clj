(ns ^{:doc "Polymorphic Node Implementation of Persistent Treap"
      :author "Jeremy Bondeson"}
  data.tree.treap.polymorphic
  (:refer-clojure :exclude [comparator comp merge])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:use [data.tree.printable])
  (:use [data.tree.treap.core])
  (:require [data.compare :as cmp])
  (:import (java.util Comparator)))

(set! *warn-on-reflection* true)

;;-- Types
(deftype LeafNode   [v ^double p])
(deftype LeftyNode  [v ^double p ^data.tree.treap.core.INode l])
(deftype RightyNode [v ^double p ^data.tree.treap.core.INode r])
(deftype FullNode   [v ^double p ^data.tree.treap.core.INode l ^data.tree.treap.core.INode r])

;;-- Helpers

(defmacro
  make-node ^:private ^:static
  [value priority left right]
  `(let [l# ~left
         r# ~right]
     (cond
      (and l# r#) (FullNode. ~value ~priority l# r#)
      l#          (LeftyNode. ~value ~priority l#)
      r#          (RightyNode. ~value ~priority r#)
      :else       (LeafNode. ~value ~priority))))

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
        (make-node (value l) lp (left l) (merge (right l) r comp))
        (make-node (value r) rp (merge (left r) l comp) (right r))))
    (or fst snd)))

;;-- Extensions

(extend-type LeafNode
  INode
  (insert [this item priority comparator]
    (cmp/case ^Comparator comparator item (.v this)
              := (throw+ {:duplicate-key? true})
              :> (if (< priority (.p this))
                   (LeftyNode. item priority this)
                   (RightyNode. (.v this) (.p this) (LeafNode. item priority)))
              :< (if (< priority (.p this))
                   (RightyNode. item priority this)
                   (LeftyNode. (.v this) (.p this) (LeafNode. item priority)))))

  (delete [this item comparator]
    (cmp/if-not= ^Comparator comparator item (.v this)
                 (throw+ {:not-found? true})))

  (rotate-left [this] this)
  (rotate-right [this] this)
  (retrieve [this item comparator]
    (cmp/if= ^Comparator comparator item (.v this)
             (.v this)))
  (left [_] nil)
  (right [_] nil)
  (priority [this] (.p this))
  (value [this] (.v this)))

(extend-type LeftyNode
  INode
  (insert [this item priority comparator]
    (cmp/case ^Comparator comparator item (.v this)
              := (throw+ {:duplicate-key? true})
              :< (let [node (LeftyNode. (.v this) (.p this) (insert (.l this) item priority comparator))]
                   (if (< priority (.p this))
                     (rotate-right node)
                     node))
              :> (if (< priority (.p this))
                   (LeftyNode. item priority this)
                   (FullNode. (.v this) (.p this) (.l this) (LeafNode. item priority)))))
  (delete [this item comparator]
    (cmp/case ^Comparator comparator item (.v this)
              := (.l this)
              :< (if-let [node (delete (.l this) item comparator)]
                   (LeftyNode. (.v this) (.p this) node)
                   (LeafNode. (.v this) (.p this)))
              :> (throw+ {:not-found? true})))
  (rotate-left [this] this)
  (rotate-right [this]
    (let [lv  (value (.l this))
          lp  (priority (.l this))
          ll  (left (.l this))
          lr  (right (.l this))]
    (cond
     (and ll lr) (FullNode. lv lp ll (LeftyNode. (.v this) (.p this) lr))
     ll          (FullNode. lv lp ll (LeafNode. (.v this) (.p this)))
     lr          (RightyNode. lv lp (LeftyNode. (.v this) (.p this) lr))
     :else       (RightyNode. lv lp (LeafNode. (.v this) (.p this))))))
  (retrieve [this item comparator]
    (cmp/case ^Comparator comparator item (.v this)
              := (.v this)
              :< (retrieve (.l this) item comparator)))
  (left [this] (.l this))
  (right [_] nil)
  (priority [this] (.p this))
  (value [this] (.v this)))

(extend-type RightyNode
  INode
  (insert [this item priority comparator]
    (cmp/case ^Comparator comparator item (.v this)
              := (throw+ {:duplicate-key? true})
              :> (let [node (RightyNode. (.v this) (.p this) (insert (.r this) item priority comparator))]
                   (if (< priority (.p this))
                     (rotate-left node)
                     node))
              :< (if (< priority (.p this))
                   (RightyNode. item priority this)
                   (FullNode. (.v this) (.p this) (LeafNode. item priority) (.r this)))))
  (delete [this item comparator]
    (cmp/case ^Comparator comparator item (.v this)
              := (.r this)
              :> (if-let [node (delete (.r this) item comparator)]
                   (RightyNode. (.v this) (.p this) node)
                   (LeafNode. (.v this) (.p this)))
              :< (throw+ {:not-found? true})))
  (rotate-left [this]
    (let [r   (.r this)
          rv  (value r)
          rp  (priority r)
          rl  (left r)
          rr  (right r)]
      (cond
       (and rl rr) (FullNode. rv rp (RightyNode. (.v this) (.p this) rl) rr)
       rr          (FullNode. rv rp (LeafNode. (.v this) (.p this)) rr)
       rl          (LeftyNode. rv rp (RightyNode. (.v this) (.p this) rl))
       :else       (LeftyNode. rv rp (LeafNode. (.v this) (.p this))))))
  (rotate-right [this] this)
  (retrieve [this item comparator]
    (cmp/case ^Comparator comparator item (.v this)
              :=  (.v this)
              :> (retrieve (.r this) item comparator)))
  (left [_] nil)
  (right [this] (.r this))
  (priority [this] (.p this))
  (value [this] (.v this)))

(extend-type FullNode
  INode
  (insert [this item priority comparator]
    (cmp/case ^Comparator comparator item (.v this)
              := (throw+ {:duplicate-key? true})
              :> (let [node (FullNode. (.v this) (.p this) (.l this) (insert (.r this) item priority comparator))]
                   (if (< priority (.p this))
                     (rotate-left node)
                     node))
              :< (let [node (FullNode. (.v this) (.p this) (insert (.l this) item priority comparator) (.r this))]
                   (if (< priority (.p this))
                     (rotate-right node)
                     node))))
  (delete [this item comparator]
    (cmp/case ^Comparator comparator item (.v this)
              := (merge (.l this) (.r this))
              :> (if-let [node (delete (.r this) item comparator)]
                   (FullNode. (.v this) (.p this) (.l this) node)
                   (LeftyNode. (.v this) (.p this) (.l this)))
              :< (if-let [node (delete (.l this) item comparator)]
                   (FullNode. (.v this) (.p this) node (.r this))
                   (RightyNode. (.v this) (.p this) (.r this)))))
  (rotate-left [this]
    (let [r   (.r this)
          rv  (value r)
          rp  (priority r)
          rl  (left r)
          rr  (right r)]
      (cond
       (and rl rr) (FullNode. rv rp (FullNode. (.v this) (.p this) (.l this) rl) rr)
       rr          (FullNode. rv rp (LeftyNode. (.v this) (.p this) (.l this)) rr)
       rl          (LeftyNode. rv rp (FullNode. (.v this) (.p this) (.l this) rl))
       :else       (LeftyNode. rv rp (LeftyNode. (.v this) (.p this) (.l this))))))
  (rotate-right [this]
    (let [l   (.l this)
          lv  (value l)
          lp  (priority l)
          ll  (left l)
          lr  (right l)]
      (cond
       (and ll lr) (FullNode. lv lp ll (FullNode. (.v this) (.p this) lr (.r this)))
       ll          (FullNode. lv lp ll (RightyNode. (.v this) (.p this) (.r this)))
       lr          (RightyNode. lv lp (FullNode. (.v this) (.p this) lr (.r this)))
       :else       (RightyNode. lv lp (RightyNode. (.v this) (.p this) (.r this))))))
  (retrieve [this item comparator]
    (cmp/case ^Comparator comparator item (.v this)
              :=  (.v this)
              :> (retrieve (.r this) item comparator)
              :< (retrieve (.l this) item comparator)))
  (left [this] (.l this))
  (right [this] (.r this))
  (priority [this] (.p this))
  (value [this] (.v this)))

;;-- Creation

(defmethod build-tree :polymorphic
  [type vals comparator & options]
  (when-let [coll (seq vals)]
    (let [[x & xs] coll
          root (LeafNode. x (rand))
          ins (fn [[^INode t c :as whole] v]
                (try+
                 [(insert t v (rand) comparator) (inc c)]
                 (catch :duplicate-key? _
                   whole)))]
      (reduce ins [root 1] xs))))

;;-- Pretty Printing

(extend-protocol PrintableTree
  LeafNode
  (print-tree [x] (prtree (str "LeafNode p" (.p x)) (.v x)))
  LeftyNode
  (print-tree [x] (prtree (str "LeftyNode p" (.p x)) (.v x) (.l x)))
  RightyNode
  (print-tree [x] (prtree (str "RightyNode p" (.p x)) (.v x) (.r x)))
  FullNode
  (print-tree [x] (prtree (str "FullNode p" (.p x)) (.v x) (.l x) (.r x))))
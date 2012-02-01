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

(deftype LeafNode
    [value ^double priority])

(deftype LeftyNode
    [value ^double priority ^data.tree.treap.core.INode left])

(deftype RightyNode
    [value ^double priority ^data.tree.treap.core.INode right])

(deftype FullNode
    [value ^double priority ^data.tree.treap.core.INode left
     ^data.tree.treap.core.INode right])

(defn- ^:static const-nil [_] nil)

(defmacro ^:private
  make-node
  [value priority left right]
  `(let [l# ~left
         r# ~right]
     (cond
      (and l# r#) (FullNode. ~value ~priority l# r#)
      l#          (LeftyNode. ~value ~priority l#)
      r#          (RightyNode. ~value ~priority r#)
      :else       (LeafNode. ~value ~priority))))

(defn 
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

;;-- Leaf Node INode Implementation
(defn- ^INode
  leaf-insert
  [^LeafNode node item priority ^Comparator comp]
  (let [val (.value node)
        pri (.priority node)]
    
    (cmp/case comp item val
              := (throw+ {:duplicate-key? true})
              :> (if (< priority pri)
                   (LeftyNode. item priority node)
                   (RightyNode. val pri (LeafNode. item priority)))
              :< (if (< priority pri)
                   (RightyNode. item priority node)
                   (LeftyNode. val pri (LeafNode. item priority))))))

(defn- ^INode
  leaf-delete
  [^LeafNode node item ^Comparator comp]
  (cmp/if-not= comp item (.value node)
               (throw+ {:not-found? true})))

(defn- 
  leaf-retrieve
  [^LeafNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/if= comp item val
             val)))

(extend LeafNode
  INode
  {:insert leaf-insert
   :delete leaf-delete
   :rotate-left identity
   :rotate-right identity
   :retrieve leaf-retrieve
   :left const-nil
   :right const-nil
   :value (fn [^LeafNode x] (.value x))
   :priority (fn [^LeafNode x] (.priority x))})

;;-- Lefty Node Implementation

(defn- ^INode
  lefty-insert
  [^LeftyNode node item priority ^Comparator comp]
  (let [val (.value node)
        pri (.priority node)
        l   (.left node)]
    (cmp/case comp item val
              := (throw+ {:duplicate-key? true})
              :< (let [n (LeftyNode. val pri (insert l item priority comp))]
                   (if (< priority pri)
                     (rotate-right n)
                     n))
              :> (if (< priority pri)
                   (LeftyNode. item priority node)
                   (FullNode. val pri l (LeafNode. item priority))))))

(defn- ^INode
  lefty-delete
  [^LeftyNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/case comp item val
              := (.left node)
              :< (let [nnode (delete (.left node) item comp)]
                   (if nnode
                     (LeftyNode. val (.priority node) nnode)
                     (LeafNode. val (.priority node))))
              :> (throw+ {:not-found? true}))))

(defn- ^INode
  lefty-rotate-right
  [^LeftyNode node]
  (let [val (.value node)
        pri (.priority node)
        l   (.left node)
        lv  (value l)
        lp  (priority l)
        ll  (left l)
        lr  (right l)]
    (cond
     (and ll lr) (FullNode. lv lp ll (LeftyNode. val pri lr))
     ll          (FullNode. lv lp ll (LeafNode. val pri))
     lr          (RightyNode. lv lp (LeftyNode. val pri lr))
     :else       (RightyNode. lv lp (LeafNode. val pri)))))

(defn- 
  lefty-retrieve
  [^LeftyNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/case comp item val
              := val
              :< (retrieve (.left node) item comp))))

(extend LeftyNode
  INode
  {:insert lefty-insert
   :delete lefty-delete
   :rotate-left identity
   :rotate-right lefty-rotate-right
   :retrieve lefty-retrieve
   :left (fn [^LeftyNode x] (.left x))
   :right const-nil
   :value (fn [^LeftyNode x] (.value x))
   :priority (fn [^LeftyNode x] (.priority x))})

;;-- Righty Node Implementation
(defn- ^INode
  righty-insert
  [^RightyNode node item priority ^Comparator comp]
  (let [val (.value node)
        pri (.priority node)
        r   (.right node)]
    (cmp/case comp item val
              := (throw+ {:duplicate-key? true})
              :> (let [n (RightyNode. val pri (insert r item priority comp))]
                   (if (< priority pri)
                     (rotate-left n)
                     n))
              :< (if (< priority pri)
                   (RightyNode. item priority node)
                   (FullNode. val pri (LeafNode. item priority) r)))))

(defn- ^INode
  righty-delete
  [^RightyNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/case comp item val
              := (.right node)
              :> (let [nnode (delete (.right node) item comp)]
                   (if nnode
                     (RightyNode. val (.priority node) nnode)
                     (LeafNode. val (.priority node))))
              :< (throw+ {:not-found? true}))))

(defn- ^INode
  righty-rotate-left
  [^RightyNode node]
  (let [val (.value node)
        pri (.priority node)
        r   (.right node)
        rv  (value r)
        rp  (priority r)
        rl  (left r)
        rr  (right r)]
    (cond
     (and rl rr) (FullNode. rv rp (RightyNode. val pri rl) rr)
     rr          (FullNode. rv rp (LeafNode. val pri) rr)
     rl          (LeftyNode. rv rp (RightyNode. val pri rl))
     :else       (LeftyNode. rv rp (LeafNode. val pri)))))

(defn-
  righty-retrieve
  [^RightyNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/case comp item val
              :=  val
              :> (retrieve (.right node) item comp))))

(extend RightyNode
  INode
  {:insert righty-insert
   :delete righty-delete
   :rotate-left righty-rotate-left
   :rotate-right identity
   :retrieve righty-retrieve
   :left const-nil
   :right (fn [^RightyNode x] (.right x))
   :value (fn [^RightyNode x] (.value x))
   :priority (fn [^RightyNode x] (.priority x))})

;;-- Full Node Implementation

(defn- ^INode
  full-insert
  [^FullNode node item priority ^Comparator comp]
  (let [val (.value node)
        pri (.priority node)
        l   (.left node)
        r   (.right node)]
    (cmp/case comp item val
              := (throw+ {:duplicate-key? true})
              :> (let [n (FullNode. val pri l (insert r item priority comp))]
                   (if (< priority pri)
                     (rotate-left n)
                     n))
              :< (let [n (FullNode. val pri (insert l item priority comp) r)]
                   (if (< priority pri)
                     (rotate-right n)
                     n)))))

(defn- ^INode
  full-delete
  [^FullNode node item ^Comparator comp]
  (let [val (.value node)
        l   (.left node)
        r   (.right node)]
    (cmp/case comp item val
              := (merge l r)
              :> (let [rnode (delete r item comp)]
                    (if rnode
                      (FullNode. val (.priority node) l rnode)
                      (LeftyNode. val (.priority node) l)))
              :< (let [lnode (delete l item comp)]
                    (if lnode
                      (FullNode. val (.priority node) lnode r)
                      (RightyNode. val (.priority node) r))))))

(defn- ^INode
  full-rotate-left
  [^FullNode node]
  (let [val (.value node)
        pri (.priority node)
        l   (.left node)
        r   (.right node)
        rv  (value r)
        rp  (priority r)
        rl  (left r)
        rr  (right r)]
    (cond
     (and rl rr) (FullNode. rv rp (FullNode. val pri l rl) rr)
     rr          (FullNode. rv rp (LeftyNode. val pri l) rr)
     rl          (LeftyNode. rv rp (FullNode. val pri l rl))
     :else       (LeftyNode. rv rp (LeftyNode. val pri l)))))

(defn- ^INode
  full-rotate-right
  [^FullNode node]
  (let [val (.value node)
        pri (.priority node)
        r   (.right node)
        l   (.left node)
        lv  (value l)
        lp  (priority l)
        ll  (left l)
        lr  (right l)]
    (cond
     (and ll lr) (FullNode. lv lp ll (FullNode. val pri lr r))
     ll          (FullNode. lv lp ll (RightyNode. val pri r))
     lr          (RightyNode. lv lp (FullNode. val pri lr r))
     :else       (RightyNode. lv lp (RightyNode. val pri r)))))

(defn-
  full-retrieve
  [^FullNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/case comp item val
              :=  val
              :> (retrieve (.right node) item comp)
              :< (retrieve (.left node) item comp))))

(extend FullNode
  INode
  {:insert full-insert
   :delete full-delete
   :rotate-left full-rotate-left
   :rotate-right full-rotate-right
   :retrieve full-retrieve
   :left (fn [^FullNode x] (.left x))
   :right (fn [^FullNode x] (.right x))
   :value (fn [^FullNode x] (.value x))
   :priority (fn [^FullNode x] (.priority x))})

;;-- Creation

(defn- ^:static build-tree
  "Returns a vector consisting of the tree and count of items"
  [^Comparator comparator vals]
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
  (print-tree [x] (prtree (str "LeafNode p" (.priority x)) (.value x)))
  LeftyNode
  (print-tree [x] (prtree (str "LeftyNode p" (.priority x)) (.value x) (.left x)))
  RightyNode
  (print-tree [x] (prtree (str "RightyNode p" (.priority x)) (.value x) (.right x)))
  FullNode
  (print-tree [x] (prtree (str "FullNode p" (.priority x)) (.value x) (.left x) (.right x))))
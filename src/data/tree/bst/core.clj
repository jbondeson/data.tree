(ns ^{:doc "Binary Search Tree Core"
      :author "Jeremy Bondeson"}
  data.tree.bst.core
  (:refer-clojure :exclude [comparator comp])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:use [data.tree.printable])
  (:require [data.compare :as cmp])
  (:import (java.util Comparator)))

(defprotocol INode
  (^data.tree.bst.core.INode
    insert [this item ^Comparator comp])
  (^data.tree.bst.core.INode
    delete [this item ^Comparator comp])
  (retrieve [this item ^Comparator comp])
  (^data.tree.bst.core.INode
    left [this])
  (^data.tree.bst.core.INode
    right [this])
  (value [this]))

(deftype LeafNode [value])
(deftype LeftyNode [value ^data.tree.bst.core.INode left])
(deftype RightyNode [value ^data.tree.bst.core.INode right])
(deftype FullNode [value ^data.tree.bst.core.INode left ^data.tree.bst.core.INode right])

(defn- ^:static const-nil [_] nil)

;;-- Leaf Node Implementation
(defn- ^data.tree.bst.core.INode
  leaf-insert
  [^LeafNode node item ^Comparator comp]
  (let [val (.value node)
        leaf (LeafNode. item)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  (throw+ {:duplicate-key? true})
       (= res -1) (LeftyNode. val leaf)
       :else      (RightyNode. val leaf)))))

(defn- ^data.tree.bst.core.INode
  leaf-delete
  [^LeafNode node item ^Comparator comp]
  (cmp/with-compare comp res item (.value node)
    (when (not (= res 0))
      (throw+ {:not-found? true}))))

(defn- 
  leaf-retrieve
  [^LeafNode node item ^Comparator comp]
  (cmp/with-compare comp res item (.value node)
    (when  (= res 0)
      (.value node))))

(extend LeafNode
  INode
  {:insert leaf-insert
   :delete leaf-delete
   :retrieve leaf-retrieve
   :left const-nil
   :right const-nil
   :value (fn [^LeafNode x] (.value x))})

;;-- Lefty Node Implementation

(defn- 
  ^data.tree.bst.core.INode lefty-insert
  [^LeftyNode node item ^Comparator comp]
  (let [val (.value node)
        l   (.left node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  (throw+ {:duplicate-key? true})
       (= res -1) (LeftyNode. val (insert l item comp))
       :else      (FullNode. val l (LeafNode. item))))))

(defn- 
  ^data.tree.bst.core.INode lefty-delete
  [^LeftyNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  (.left node)
       (= res -1) (let [nnode (delete (.left node) item comp)]
                    (if nnode
                      (LeftyNode. val nnode)
                      (LeafNode. val)))
       :else      (throw+ {:not-found? true})))))

(defn- 
  lefty-retrieve
  [^LeftyNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  val
       (= res -1) (retrieve (.left node) item comp)))))

(extend LeftyNode
  INode
  {:insert lefty-insert
   :delete lefty-delete
   :retrieve lefty-retrieve
   :left (fn [^LeftyNode x] (.left x))
   :right const-nil
   :value (fn [^LeftyNode x] (.value x))})

;;-- Righty Node Implementation
(defn- ^data.tree.bst.core.INode
  righty-insert
  [^RightyNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (RightyNode. val (insert (.right node) item comp))
       :else     (FullNode. val (LeafNode. item) (.right node))))))

(defn- ^data.tree.bst.core.INode
  righty-delete
  [^RightyNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0) (.right node)
       (= res 1) (let [nnode (delete (.right node) item comp)]
                   (if nnode
                     (RightyNode. val nnode)
                     (LeafNode. val)))
       :else      (throw+ {:not-found? true})))))

(defn-
  righty-retrieve
  [^RightyNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  val
       (= res 1) (retrieve (.right node) item comp)))))

(extend RightyNode
  INode
  {:insert righty-insert
   :delete righty-delete
   :retrieve righty-retrieve
   :left const-nil
   :right (fn [^RightyNode x] (.right x))
   :value (fn [^RightyNode x] (.value x))})

;;-- Full Node Implementation

(defn- ^data.tree.bst.core.INode
  full-insert
  [^FullNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (FullNode. val (.left node) (insert (.right node) item comp))
       :else     (FullNode. val (insert (.left node) item comp) (.right node))))))

(defn- ^data.tree.bst.core.INode
  full-delete
  [^FullNode node item ^Comparator comp]
  (let [val (.value node)
        l   (.left node)
        r   (.right node)]
    (cmp/with-compare comp res item val
      (cond
       (= res  1) (let [rnode (delete r item comp)]
                    (if rnode
                      (FullNode. val l rnode)
                      (LeftyNode. val l)))
       (= res -1) (let [lnode (delete l item comp)]
                    (if lnode
                      (FullNode. val lnode r)
                      (RightyNode. val r)))
       :else      (let [^data.tree.bst.core.INode
                        successor (loop [^data.tree.bst.core.INode nnode r]
                                    (let [smaller (.left nnode)]
                                      (if (nil? smaller)
                                        nnode
                                        (recur smaller))))
                        nval (value successor)]
                    (if (identical? r successor)
                      (if (nil? (right r))
                        (LeftyNode. nval l)
                        (FullNode. nval l (right r)))
                      (FullNode. nval l (delete r nval comp))))))))

(defn-
  full-retrieve
  [^FullNode node item ^Comparator comp]
  (let [val (.value node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  val
       (= res 1) (retrieve (.right node) item comp)
       :else     (retrieve (.left node) item comp)))))

(extend FullNode
  INode
  {:insert full-insert
   :delete full-delete
   :retrieve full-retrieve
   :left (fn [^FullNode x] (.left x))
   :right (fn [^FullNode x] (.right x))
   :value (fn [^FullNode x] (.value x))})

;;-- Creation

(defn- ^:static build-tree
  "Returns a vector consisting of the tree and count of items"
  [^Comparator comparator vals]
  (when-let [coll (seq vals)]
    (let [[x & xs] coll
          root (LeafNode. x)
          ins (fn [[^data.tree.bst.core.INode t c :as whole] v]
                (try+
                 [(insert t v comparator) (inc c)]
                 (catch :duplicate-key? _
                   whole)))
          ]
      (reduce ins [root 1] xs))))


;;-- Pretty Printing

(extend-protocol PrintableTree
  LeafNode   (print-tree [x] (prtree "LeafNode" (.value x)))
  LeftyNode  (print-tree [x] (prtree "LeftyNode" (.value x) (.left x)))
  RightyNode (print-tree [x] (prtree "RightyNode" (.value x) (.right x)))
  FullNode   (print-tree [x] (prtree "FullNode" (.value x) (.left x) (.right x))))

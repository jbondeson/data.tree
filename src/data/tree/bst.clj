(ns ^{:doc "Persistent Binary Search Tree"
      :author "Jeremy Bondeson"}
  data.tree.bst
  (:refer-clojure :exclude [comparator comp])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:require [data.compare :as cmp])
  (:use [data.ifn])
  (:use [data.tree.bst.core])
  (:use [data.tree.bst.seq :only [make-seq]])
  (:use [data.tree.bst.transient :only [transient-bst]])
  (:use data.tree.printable)
  (:import (clojure.lang Seqable Sequential ISeq IPersistentSet
                         IPersistentCollection Counted Sorted
                         Reversible IEditableCollection
                         IPersistentMap IFn IObj)
           (java.util Comparator)
           (data.tree.bst.core LeafNode)
           (data.tree.bst.seq Seq)))

(set! *warn-on-reflection* true)

;;=======  Tree Implementations =======;;

(declare make-bst)

(deftype&ifn EmptyBinarySearchTree [^IPersistentMap mdata
                                    ^Comparator comparator]
  ;; IFn definitions
  {:1 (invoke [_ __] nil)}
  Object
  (equals [_ x]
    (if (isa? (type x) EmptyBinarySearchTree)
      (let [^EmptyBinarySearchTree x x]
        (= comparator (.comparator x)))
      false))
  (hashCode [this] (hash (map identity this)))
  IObj
  (meta [_] mdata)
  (withMeta [_ mdata] (EmptyBinarySearchTree. mdata comparator))
  Sequential
  Seqable
  (seq [_] nil)
  Reversible
  (rseq [_] nil)
  IPersistentCollection
  (count [_] 0)
  (empty [this] this)
  (equiv [this x] (.equals this x))
  (cons [_ item] (make-bst mdata comparator (LeafNode. item) 1))
  IPersistentSet
  (disjoin [this _] this)
  (contains [_ __] false)
  (get [_ __] nil)
  Sorted
  (comparator [_] comparator)
  (entryKey [_ __] nil)
  (seqFrom [_ __ ___] nil)
  IEditableCollection
  (asTransient [this] (transient-bst mdata comparator nil 0)))

(deftype&ifn BinarySearchTree [^IPersistentMap mdata
                               ^Comparator comparator
                               ^data.tree.bst.core.INode tree
                               count]
  ;; IFn definitions
  {:1 (invoke [this item] (.get this item))}
  Object
  (equals [_ x]
    (if (isa? (type x) BinarySearchTree)
      (let [^BinarySearchTree x x]
        (and
         (= comparator (.comparator x))
         (= tree (.tree x))))
      false))
  (hashCode [this] (hash (map identity this)))
  IObj
  (meta [_] mdata)
  (withMeta [_ mdata] (BinarySearchTree. mdata comparator tree count))
  Sequential
  Seqable
  (seq [_] (make-seq tree true count))
  Reversible
  (rseq [_] (make-seq tree false count))
  IPersistentCollection
  (count [_] count)
  (empty [this] (EmptyBinarySearchTree. mdata comparator))
  (equiv [this x] (.equals this x))
  (cons [this item]
    (try+
     (BinarySearchTree. mdata comparator (insert tree item comparator) (inc count))
     (catch :duplicate-key? _
         this)))
  IPersistentSet
  (disjoin [this item]
    (try+
     (let [ntree (delete tree item comparator)]
       (if ntree
         (BinarySearchTree. mdata comparator ntree (dec count))
         (.empty this)))
     (catch :not-found? _
       this)))
  (contains [this x] (not (nil? (.get this x))))
  (get [_ x] (retrieve tree x comparator))
  Sorted
  (comparator [_] comparator)
  (entryKey [this x] (when (contains? this) x))
  (seqFrom [_ item asc]
    (loop [^data.tree.bst.core.INode node tree
           stack nil]
      (if (nil? node)
        (when stack (Seq. nil stack asc nil (Object.)))
        (cmp/with-compare comparator res item (value node)
          (cond
           (= res  0)           (Seq. nil (cons node stack) asc nil (Object.))
           (and asc (= res -1)) (recur (left node) (cons node stack))
           asc                  (recur (right node) stack)
           (= res 1)            (recur (right node) (cons node stack))
           :else                (recur (left node) stack))))))
  IEditableCollection
  (asTransient [this] (transient-bst mdata comparator tree count)))



(defn- make-bst
  ([mdata comparator]
     (EmptyBinarySearchTree. mdata comparator))
  ([mdata comparator tree count]
     (BinarySearchTree. mdata comparator tree count)))

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

(defn ^:static binary-search-tree
  "Returns a new binary search tree with supplied keys."
  ([]
     (EmptyBinarySearchTree. nil cmp/default))
  ([& keys]
     (let [[tree count] (build-tree cmp/default keys)]
       (BinarySearchTree. nil cmp/default tree count))))

(defn ^:static binary-search-tree-by
  "Returns a new binary search tree with supplied keys, using the supplied comparator."
  ([^Comparator comparator]
     (EmptyBinarySearchTree. nil comparator))
  ([^Comparator comparator & keys]
     (let [[tree count] (build-tree cmp/default keys)]
       (BinarySearchTree. nil comparator tree count))))

;;-- Pretty Printing

(extend-protocol PrintableTree
  EmptyBinarySearchTree  (print-tree [x] (prtree "BST : 0 empty"))
  BinarySearchTree       (print-tree [x] (prtree "BST :" (.count x) (.tree x))))
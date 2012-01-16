(ns ^{:doc "Transient implementation for Binary Search Trees"
      :author "Jeremy Bondeson"}
  data.tree.bst.transient
  (:refer-clojure :exclude [comparator comp])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:use [data.tree.bst.core])
  (:use [data.tree.printable])
  (:require [data.compare :as cmp])
  (:require [data.util.tref :as tref])
  (:import (java.util Comparator)
           (data.util EditContext ThreadBoundRef)
           (data.tree.bst.core LeafNode LeftyNode RightyNode FullNode)))

(set! *warn-on-reflection* true)

(defprotocol ITransientNode
  (^data.tree.bst.transient.ITransientNode
    delete! [this ^EditContext edit item  ^Comparator comp])
  (^data.tree.bst.transient.ITransientNode
   insert! [this ^EditContext edit item  ^Comparator comp])
  (^data.tree.bst.transient.ITransientNode
    insert+copy [this ^EditContext edit item ^Comparator comp])
  (^data.tree.bst.transient.ITransientNode
    delete+copy [this ^EditContext edit item ^Comparator comp]))

(defrecord TransientNode [value ^ThreadBoundRef left ^ThreadBoundRef right])

(defn-
  ^{:tag data.tree.bst.transient.ITransientNode
    :inline (fn [e v l r] `(TransientNode. ~v
                                          (tref/thread-bound-ref ~l ~e)
                                          (tref/thread-bound-ref ~r ~e)))
    :inline-arities #{4}}
  make-trans-node
  [^EditContext edit value left right]

(extend FullNode
  ITransientNode
  {:insert! full-insert+copy
   :delete! full-delete+copy
   :insert+copy full-insert+copy
   :delete+copy full-delete+copy})
  (TransientNode. value
                 (tref/thread-bound-ref left edit)
                 (tref/thread-bound-ref right edit)))

;;-- Leaf Node Implementation
(defn- ^data.tree.bst.transient.ITransientNode
  leaf-insert+copy
  [^LeafNode node ^EditContext edit item ^Comparator comp]
  (let [leaf (make-trans-node edit item nil nil)
        val (:value node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  (throw+ {:duplicate-key? true})
       (= res -1) (make-trans-node edit val leaf nil)
       :else      (make-trans-node edit val nil leaf)))))

(defn- ^data.tree.bst.transient.ITransientNode
  leaf-delete+copy
  [^LeafNode node ^EditContext edit item ^Comparator comp]
  (cmp/with-compare comp res item (:value node)
    (when (not (= res 0))
      (throw+ {:not-found? true}))))

(extend LeafNode
  ITransientNode
  {:insert! leaf-insert+copy
   :delete! leaf-delete+copy
   :insert+copy leaf-insert+copy
   :delete+copy leaf-delete+copy})

;;-- Lefty Node Implementation
(defn- ^data.tree.bst.transient.ITransientNode
  lefty-insert+copy
  [^LeftyNode node ^EditContext edit item ^Comparator comp]
  (let [val (:value node)
        l   (:left node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  (throw+ {:duplicate-key? true})
       (= res -1) (make-trans-node edit val (insert+copy l edit item comp) nil)
       :else      (make-trans-node edit val l (make-trans-node edit item nil nil))))))

(defn- ^data.tree.bst.transient.ITransientNode
  lefty-delete+copy
  [^LeftyNode node ^EditContext edit item ^Comparator comp]
  (let [val (:value node)
        l   (:left node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  l
       (= res -1) (make-trans-node edit val (delete+copy l edit item comp) nil)
       :else      (throw+ {:not-found? true})))))

(extend LeftyNode
  ITransientNode
  {:insert! lefty-insert+copy
   :delete! lefty-delete+copy
   :insert+copy lefty-insert+copy
   :delete+copy lefty-delete+copy})

;;-- Righty Node Implementation
(defn- ^data.tree.bst.transient.ITransientNode
  righty-insert+copy
  [^RightyNode node ^EditContext edit item ^Comparator comp]
  (let [val (:value node)
        r   (:right node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (make-trans-node edit val nil (insert+copy r edit item comp))
       :else     (make-trans-node edit val (make-trans-node edit item nil nil) r)))))

(defn- ^data.tree.bst.transient.ITransientNode
  righty-delete+copy
  [^RightyNode node ^EditContext edit item ^Comparator comp]
  (let [val (:value node)
        r   (:right node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0) r
       (= res 1) (make-trans-node edit val nil (delete+copy r edit item comp))    
       :else      (throw+ {:not-found? true})))))

(extend RightyNode
  ITransientNode
  {:insert! righty-insert+copy
   :delete! righty-delete+copy
   :insert+copy righty-insert+copy
   :delete+copy righty-delete+copy})

;;-- Full Node Implementation
(defn- ^data.tree.bst.transient.ITransientNode
  full-insert+copy
  [^FullNode node ^EditContext edit item ^Comparator comp]
  (let [val (:value node)
        l   (:left node)
        r   (:right node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (make-trans-node edit val l (insert+copy r edit item comp))
       :else     (make-trans-node edit val (insert+copy l edit item comp) r)))))

(defn- ^data.tree.bst.transient.ITransientNode
  full-delete+copy
  [^FullNode node ^EditContext edit item ^Comparator comp]
  (let [val (:value node)
        l   (:left node)
        r   (:right node)]
    (cmp/with-compare comp res item val
      (cond
       (= res  1) (make-trans-node edit val l (delete+copy r edit item comp))    
       (= res -1) (make-trans-node edit val (delete+copy l edit item comp) r)    
       :else      (let [^data.tree.bst.core.INode
                        successor (loop [^data.tree.bst.core.INode node r]
                                    (let [smaller (left node)]
                                      (if (nil? smaller)
                                        node
                                        (recur smaller))))
                        nval (value successor)]
                    (if (identical? r successor)                      
                      (make-trans-node edit nval l (right r))
                      (make-trans-node edit nval l (delete+copy r edit nval comp))))))))

(extend FullNode
  ITransientNode
  {:insert! full-insert+copy
   :delete! full-delete+copy
   :insert+copy full-insert+copy
   :delete+copy full-delete+copy})

;;-- Transient Node INode Implementation
(defn- ^data.tree.bst.core.INode
  trans-insert
  [^TransientNode node item ^Comparator comp]
  (let [val (:value node)
        ^data.tree.bst.core.INode l @(:left node)
        ^data.tree.bst.core.INode r @(:right node)]
    (cmp/with-compare comp res item val
        (cond
         (= res 0) (throw+ {:duplicate-key? true})
         (= res 1) (cond
                    (and l r) (FullNode. val l (insert r item comp))
                    r         (RightyNode. val (insert r item comp))
                    l         (FullNode. val l (LeafNode. item))
                    :else     (RightyNode. val (LeafNode. item)))
         :else     (cond
                    (and l r) (FullNode. val (insert l item comp) r)
                    l         (LeftyNode. val (insert l item comp))
                    r         (FullNode. val (LeafNode. item) r)
                    :else     (LeftyNode. val (LeafNode. item)))))))

(defn- ^data.tree.bst.core.INode
  trans-delete
  [^TransientNode node item ^Comparator comp]
  (let [val (:value node)
        ^data.tree.bst.core.INode l @(:left node)
        ^data.tree.bst.core.INode r @(:right node)]
    (cmp/with-compare comp res item val
        (cond
         (= res -1) (if l
                      (let [dnode (delete l item comp)]
                        (cond
                         (and dnode r) (FullNode. val dnode r)
                         dnode         (LeftyNode. val dnode)
                         r             (RightyNode. val r)
                         :else         (LeafNode. val)))
                      (throw+ {:not-found? true}))
         (= res  1) (if r
                      (let [dnode (delete r item comp)]
                        (cond
                         (and dnode l) (FullNode. val l dnode)
                         dnode         (RightyNode. val dnode)
                         l             (LeftyNode. val l)
                         :else         (LeafNode. val)))
                      (throw+ {:not-found? true}))
         :else      (if (and l r)
                      (let [^data.tree.bst.core.INode
                            successor (loop [^data.tree.bst.core.INode nnode r]
                                        (let [smaller (left nnode)]
                                          (if smaller
                                            (recur smaller)
                                            nnode)))
                            nval (value successor)]
                        (if (identical? r successor)
                          (if (right r)
                            (FullNode. nval l (right r))
                            (LeftyNode. nval l))
                          (FullNode. nval l (delete r nval comp))))
                      (or l r))))))

(defn- ^data.tree.bst.core.INode
  trans-retrieve
  [^TransientNode node item ^Comparator comp]
  (let [val (:value node)
        ^data.tree.bst.core.INode l @(:left node)
        ^data.tree.bst.core.INode r @(:right node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  val
       (and (= res -1) l) (retrieve l item comp)
       (and (= res  1) r) (retrieve r item comp)))))

(extend TransientNode
  INode
  {:insert trans-insert
   :delete trans-delete
   :retrieve trans-retrieve
   :left (fn [x] @(:left x))
   :right (fn [x] @(:right x))
   :value :value})

;;-- Transient Node ITransientNode Implementation
(defn- ^data.tree.bst.transient.ITransientNode
  trans-insert!
  [^TransientNode node ^EditContext edit item ^Comparator comp]
  (cmp/with-compare comp res item (:value node)
    (let [^ThreadBoundRef l (:left node)
          ^ThreadBoundRef r (:right node)
          ^data.tree.bst.core.INode lnode @l
          ^data.tree.bst.core.INode rnode @r]
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (do
                   (if rnode
                     (tref/set! r (insert! rnode edit item comp))
                     (tref/set! r (make-trans-node edit item nil nil)))
                   node)
       :else     (do
                   (if lnode
                     (tref/set! l  (insert! lnode edit item comp))
                     (tref/set! l (make-trans-node edit item nil nil)))
                   node)))))

(defn- ^data.tree.bst.transient.ITransientNode
  trans-delete!
  [^TransientNode node ^EditContext edit item ^Comparator comp]
  (cmp/with-compare comp res item (:value node)
      (let [^ThreadBoundRef l (:left node)
            ^ThreadBoundRef r (:right node)
            ^data.tree.bst.core.INode lnode @l
            ^data.tree.bst.core.INode rnode @r]
        (cond
         (= res  1) (do
                      (when rnode
                        (tref/set! r (delete! rnode edit item comp)))
                      (throw+ {:not-found? true}))
         (= res -1) (do
                      (when lnode
                        (tref/set! l (delete! lnode edit item comp)))
                      (throw+ {:not-found? true}))
         :else      (if (and lnode rnode)
                      (let [^data.tree.bst.core.INode
                            successor (loop [^data.tree.bst.core.INode node rnode]
                                        (let [smaller (left node)]
                                          (if smaller
                                            (recur smaller)
                                            node)))
                            nval (value successor)]
                        (if (identical? rnode successor)
                          (make-trans-node edit nval lnode (right rnode))
                          (make-trans-node edit nval lnode (delete! rnode edit val comp))))
                      (or lnode rnode))))))

(defn- ^data.tree.bst.transient.ITransientNode
  trans-insert+copy
  [^TransientNode node ^EditContext edit item ^Comparator comp]
  (let [val (:value node)
        ^data.tree.bst.core.INode l @(:left node)
        ^data.tree.bst.core.INode r @(:right node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (if r
                   (make-trans-node edit val l (insert+copy r edit item comp))
                   (make-trans-node edit val l (make-trans-node edit item nil nil)))
       :else     (if l
                   (make-trans-node edit val (insert+copy l edit item comp) r)
                   (make-trans-node edit val (make-trans-node edit item nil nil) r))))))

(defn- ^data.tree.bst.transient.ITransientNode
  trans-delete+copy
  [^TransientNode node ^EditContext edit item ^Comparator comp]
  (let [val (:value node)
        ^data.tree.bst.core.INode l @(:left node)
        ^data.tree.bst.core.INode r @(:right node)]
    (cmp/with-compare comp res item val
      (cond
       (= res  1) (if r
                    (make-trans-node edit val l (delete+copy r edit item comp))
                    (throw+ {:not-found? true}))
       (= res -1) (if l
                    (make-trans-node edit val (delete+copy l edit item comp) r)
                    (throw+ {:not-found? true}))
       :else      (if (and l r)
                    (let [^data.tree.bst.core.INode
                          successor (loop [^data.tree.bst.core.INode node r]
                                      (let [smaller (left node)]
                                        (if smaller
                                          (recur smaller)
                                          node)))
                          nval (value successor)]
                      (if (identical? r successor)
                        (make-trans-node edit nval l (right r))
                        (make-trans-node edit nval l (delete+copy r edit nval comp))))
                    (or l r))))))

(extend TransientNode
  ITransientNode
  {:insert! trans-insert+copy
   :delete! trans-delete+copy
   :insert+copy trans-insert+copy
   :delete+copy trans-delete+copy})

;;-- Printable Tree
(extend-protocol PrintableTree
  TransientNode (print-tree [x] (prtree "TransNode" (:value x) @(:left x) @(:right x))))
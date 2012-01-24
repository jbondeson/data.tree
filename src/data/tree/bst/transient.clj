(ns ^{:doc "Transient implementation for Binary Search Trees"
      :author "Jeremy Bondeson"}
  data.tree.bst.transient
  (:refer-clojure :exclude [comparator comp])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:use [data.tree.bst.core])
  (:use [data.tree.printable])
  (:use [data.transient])
  (:require [data.compare :as cmp])
  (:import (java.util Comparator)
           (java.util.concurrent.atomic AtomicReference)
           (data.tree.bst.core LeafNode LeftyNode RightyNode FullNode)
           (clojure.lang IEditableCollection Counted IPersistentMap
                         ITransientCollection ITransientSet)))

(set! *warn-on-reflection* true)

(defprotocol ITransientNode
  (^data.tree.bst.transient.ITransientNode
    delete! [this ^AtomicReference edit item  ^Comparator comp])
  (^data.tree.bst.transient.ITransientNode
    insert! [this ^AtomicReference edit item  ^Comparator comp]))

(defprotocol IEditableNode
  (^data.tree.bst.transient.IEditableNode
    edit&set-left! [^data.tree.bst.transient.IEditableNode this
                    ^AtomicReference edit
                    fn-left])
  (^data.tree.bst.transient.IEditableNode
    edit&set-right! [^data.tree.bst.transient.IEditableNode this
                     ^AtomicReference edit
                     fn-right]))

(declare make-trans-node)

;; All protocol implementations are declared inline in order to access
;; mutable properties.
(deftype TransientNode [value
                        #^{:tag data.tree.bst.transient.ITransientNode
                           :unsynchronized-mutable true} left
                        #^{:tag data.tree.bst.transient.ITransientNode
                           :unsynchronized-mutable true} right
                        ^AtomicReference edit]
  INode
  (insert [_ item comp]
    (cmp/with-compare ^Comparator comp res item value
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (cond
                  (and left right) (FullNode. value left (insert right item comp))
                  right            (RightyNode. value (insert right item comp))
                  left             (FullNode. value left (LeafNode. item))
                  :else            (RightyNode. value (LeafNode. item)))
       :else     (cond
                  (and left right) (FullNode. value (insert left item comp) right)
                  left             (LeftyNode. value (insert left item comp))
                  right            (FullNode. value (LeafNode. item) right)
                  :else            (LeftyNode. value (LeafNode. item))))))

  (delete [_ item comp]
    (cmp/with-compare ^Comparator comp res item value
      (cond
       (= res -1) (if left
                    (let [dnode (delete left item comp)]
                      (cond
                       (and dnode right) (FullNode. value dnode right)
                       dnode             (LeftyNode. value dnode)
                       right             (RightyNode. value right)
                       :else             (LeafNode. value)))
                    (throw+ {:not-found? true}))
       (= res  1) (if right
                    (let [dnode (delete right item comp)]
                      (cond
                       (and dnode left) (FullNode. value left dnode)
                       dnode            (RightyNode. value dnode)
                       left             (LeftyNode. value left)
                       :else            (LeafNode. value)))
                    (throw+ {:not-found? true}))
       :else      (if (and left right)
                    (let [^data.tree.bst.core.INode
                          successor (loop [^data.tree.bst.core.INode nnode right]
                                      (let [smaller (left nnode)]
                                        (if smaller
                                          (recur smaller)
                                          nnode)))
                          nval (value successor)]
                      (if (identical? right successor)
                        (if (right right)
                          (FullNode. nval left (right right))
                          (LeftyNode. nval left))
                        (FullNode. nval left (delete right nval comp))))
                    (or left right)))))

  (retrieve [_ item comp]
    (cmp/with-compare ^Comparator comp res item value
      (cond
       (= res 0)              value
       (and (= res -1) left)  (retrieve left item comp)
       (and (= res  1) right) (retrieve right item comp))))

  (left [_] left)
  (right [_] right)
  (value [_] value)
  
  ITransientNode
  (insert! [this edit' item comp]
    (let [ins-or-create (fn [x] (if x
                                 (insert! x edit' item comp)
                                 (make-trans-node edit' item nil nil)))]
      (cmp/with-compare ^Comparator comp res item value
        (cond
         (= res 0) (throw+ {:duplicate-key? true})
         (= res 1) (edit&set-right! this edit' ins-or-create)
         :else     (edit&set-left!  this edit' ins-or-create)))))

  (delete! [this edit' item comp]
    (let [del-fn (fn [x] (delete! x edit' item comp))]
      (cmp/with-compare ^Comparator comp res item value
        (cond
         (= res  1) (if right
                      (edit&set-right! this edit' del-fn)
                      (throw+ {:not-found? true}))
         (= res -1) (if left
                      (edit&set-left! this edit' del-fn)
                      (throw+ {:not-found? true}))
         :else      (if (and left right)
                      (let [^data.tree.bst.core.INode
                            head (loop [^data.tree.bst.core.INode node right]
                                   (let [smaller (left node)]
                                     (if smaller
                                       (recur smaller)
                                       node)))
                            headval (value head)
                            nright (if (identical? right head)
                                     (right right)
                                     (delete! (right right) edit' headval comp))]
                        (make-trans-node edit' headval left nright))
                      (or left right))))))
  
  IEditableNode
  (edit&set-left! [this edit' fn-left]
    (if (identical? edit edit') 
      (do
        (set! left (fn-left left))
        this)
      (make-trans-node edit' value (fn-left left) right)))
  
  (edit&set-right! [this edit' fn-right]
    (if (identical? edit edit')
      (do
        (set! right (fn-right right))
        this)
      (make-trans-node edit' value left (fn-right right)))))

(defn-
  ^{:tag data.tree.bst.transient.ITransientNode
    :inline (fn [e v l r] `(TransientNode. ~v ~l ~r ~e))
    :inline-arities #{4}}
  make-trans-node
  [^AtomicReference edit value left right]
  (TransientNode. value left right edit))


;;-- Leaf Node Implementation
(defn- ^data.tree.bst.transient.ITransientNode
  leaf-insert!
  [^LeafNode node ^AtomicReference edit item ^Comparator comp]
  (let [leaf (make-trans-node edit item nil nil)
        val (.value node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  (throw+ {:duplicate-key? true})
       (= res -1) (make-trans-node edit val leaf nil)
       :else      (make-trans-node edit val nil leaf)))))

(defn- ^data.tree.bst.transient.ITransientNode
  leaf-delete!
  [^LeafNode node ^AtomicReference edit item ^Comparator comp]
  (cmp/with-compare comp res item (.value node)
    (when (not (= res 0))
      (throw+ {:not-found? true}))))

(defn- ^data.tree.bst.transient.ITransientNode
  leaf-ensure-editable
  [^LeafNode node ^AtomicReference edit]
  (make-trans-node edit (.value node) nil nil))

(extend LeafNode
  ITransientNode
  {:insert! leaf-insert!
   :delete! leaf-delete!}
  ITransient
  {:ensure-editable leaf-ensure-editable})

;;-- Lefty Node Implementation
(defn- ^data.tree.bst.transient.ITransientNode
  lefty-insert!
  [^LeftyNode node ^AtomicReference edit item ^Comparator comp]
  (let [val (.value node)
        l   (.left node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  (throw+ {:duplicate-key? true})
       (= res -1) (make-trans-node edit val (insert! l edit item comp) nil)
       :else      (make-trans-node edit val l (make-trans-node edit item nil nil))))))

(defn- ^data.tree.bst.transient.ITransientNode
  lefty-delete!
  [^LeftyNode node ^AtomicReference edit item ^Comparator comp]
  (let [val (.value node)
        l   (.left node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0)  l
       (= res -1) (make-trans-node edit val (delete! l edit item comp) nil)
       :else      (throw+ {:not-found? true})))))

(defn- ^data.tree.bst.transient.ITransientNode
  lefty-ensure-editable
  [^LeftyNode node ^AtomicReference edit]
  (make-trans-node edit (.value node) (.left node) nil))

(extend LeftyNode
  ITransientNode
  {:insert! lefty-insert!
   :delete! lefty-delete!}
  ITransient
  {:ensure-editable lefty-ensure-editable})

;;-- Righty Node Implementation
(defn- ^data.tree.bst.transient.ITransientNode
  righty-insert!
  [^RightyNode node ^AtomicReference edit item ^Comparator comp]
  (let [val (.value node)
        r   (.right node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (make-trans-node edit val nil (insert! r edit item comp))
       :else     (make-trans-node edit val (make-trans-node edit item nil nil) r)))))

(defn- ^data.tree.bst.transient.ITransientNode
  righty-delete!
  [^RightyNode node ^AtomicReference edit item ^Comparator comp]
  (let [val (.value node)
        r   (.right node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0) r
       (= res 1) (make-trans-node edit val nil (delete! r edit item comp))    
       :else      (throw+ {:not-found? true})))))

(defn- ^data.tree.bst.transient.ITransientNode
  righty-ensure-editable
  [^RightyNode node ^AtomicReference edit]
  (make-trans-node edit (.value node) nil (.right node)))

(extend RightyNode
  ITransientNode
  {:insert! righty-insert!
   :delete! righty-delete!}
  ITransient
  {:ensure-editable righty-ensure-editable})

;;-- Full Node Implementation
(defn- ^data.tree.bst.transient.ITransientNode
  full-insert!
  [^FullNode node ^AtomicReference edit item ^Comparator comp]
  (let [val (.value node)
        l   (.left node)
        r   (.right node)]
    (cmp/with-compare comp res item val
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (make-trans-node edit val l (insert! r edit item comp))
       :else     (make-trans-node edit val (insert! l edit item comp) r)))))

(defn- ^data.tree.bst.transient.ITransientNode
  full-delete!
  [^FullNode node ^AtomicReference edit item ^Comparator comp]
  (let [val (.value node)
        l   (.left node)
        r   (.right node)]
    (cmp/with-compare comp res item val
      (cond
       (= res  1) (make-trans-node edit val l (delete! r edit item comp))    
       (= res -1) (make-trans-node edit val (delete! l edit item comp) r)    
       :else      (let [^data.tree.bst.core.INode
                        successor (loop [^data.tree.bst.core.INode node r]
                                    (let [smaller (left node)]
                                      (if (nil? smaller)
                                        node
                                        (recur smaller))))
                        nval (value successor)]
                    (if (identical? r successor)                      
                      (make-trans-node edit nval l (right r))
                      (make-trans-node edit nval l (delete! r edit nval comp))))))))

(defn- ^data.tree.bst.transient.ITransientNode
  full-ensure-editable
  [^FullNode node ^AtomicReference edit]
  (make-trans-node edit (.value node) (.left node) (.right node)))

(extend FullNode
  ITransientNode
  {:insert! full-insert!
   :delete! full-delete!}
  ITransient
  {:ensure-editable full-ensure-editable})


;;-- Transient Tree Implementation

;; This will avoid a circular dependency, because the three types all
;;  reference each other, short of throwing everything in one file,
;;  this is the only way to resolve that
(def ^:private make-bst
  (ns-resolve 'data.tree.bst 'make-bst))

(deftype TransientBinarySearchTree [^IPersistentMap mdata
                                    ^Comparator comparator
                                    ^AtomicReference edit
                                    #^{:tag data.tree.bst.transient.ITransientNode
                                       :unsynchronized-mutable true} tree
                                    #^{:unsynchronized-mutable true} cnt]
  Counted
  (count [this] cnt)
  ITransientCollection
  (persistent [_] 
    (.set edit nil)
    (if tree
      (make-bst mdata comparator tree cnt)
      (make-bst mdata comparator)))
  (conj [this item]
    (ensure-editable this edit)
    (try+
     (if tree
       (set! tree (insert! tree edit item comparator))
       (set! tree (make-trans-node edit item nil nil)))
     (set! cnt (inc cnt))
     this
     (catch :duplicate-key? _
       this)))
  ITransientSet
  (disjoin [this item]
    (ensure-editable this edit)
    (try+
     (when tree
       (set! tree (delete! tree edit item comparator))
       (set! (.cnt this) (dec cnt)))
     this
     (catch :not-found? _
       this)))
  (contains [this item] (not (nil? (.get this item))))
  (get [this item] (when tree
                     (retrieve tree item comparator)))
  ITransient
  (ensure-editable [this edit']
    (let [owner (.get ^AtomicReference edit')]
      (when (not owner)
        (throw (IllegalAccessError. "Transient used after persistent! call")))
      (when (not (identical? owner (Thread/currentThread)))
        (throw (IllegalAccessError. "Transient used by non-owner thread")))
      this)))


(defn transient-bst
  [^IPersistentMap mdata ^Comparator comparator ^data.tree.bst.core.INode tree count]
  (TransientBinarySearchTree. mdata comparator (edit-context) tree count))

(set! *warn-on-reflection* false)

;;-- Printable Tree
(extend-protocol PrintableTree
  TransientNode
  (print-tree [x] (prtree "TransNode" (.value x) (.left x) (.right x)))
  TransientBinarySearchTree
  (print-tree [x]
    (let [head (when (> (count x) 0)
                 (.tree (persistent! x)))]
          (prtree "TransBST :" (count x) head))))
(ns ^{:doc "Persistent Binary Search Tree"
      :author "Jeremy Bondeson"}
  data.tree.bst
  (:refer-clojure :exclude [comparator comp])
  (:require [data.tree.quickref :as qref])
  (:import (clojure.lang Seqable Sequential ISeq IPersistentSet
                         IPersistentCollection Counted Sorted
                         Reversible IEditableCollection)))

(set! *warn-on-reflection* true)

(defmacro with-comparator
  "Macro to automate the binding of a comparator result"
  ([^java.util.Comparator comparator result x y & body]
     `(let [~result (int (.compare ~comparator ~x ~y))]
        ~@body)))

(definterface INode
  (^data.tree.bst.INode insert [item ^java.util.Comparator comp])
  (^data.tree.bst.INode delete [item ^java.util.Comparator comp])
  (^data.tree.bst.INode doInsert [item ^java.util.Comparator comp])
  (^data.tree.bst.INode doDelete [item ^java.util.Comparator comp])
  (retrieve [item ^java.util.Comparator comp])
  (value [])
  (left [])
  (right []))

(declare make-leaf-node make-lefty-node make-righty-node make-full-node
         make-trans-node)

(deftype LeafNode [x]
  INode
  (insert [this item comp]
    (let [leaf (make-leaf-node item)]
      (with-comparator comp res item x
        (cond
         (= res 0)  this
         (= res -1) (make-lefty-node x leaf)
         :else      (make-righty-node x leaf)))))
  (delete [this item comp]
    (with-comparator comp res item x
      (if (= res 0)
        nil
        this)))
  (doInsert [this item comp]
    (let [leaf (make-trans-node item)]
      (with-comparator comp res item x
        (cond
         (= res 0)  leaf
         (= res -1) (make-trans-node x leaf nil)
         :else      (make-trans-node x nil leaf)))))
  (doDelete [this item comp]
    (with-comparator comp res item x
      (if (= res 0)
        nil
        this)))
  (retrieve [this item comp]
    (with-comparator comp res item x
      (if (= res 0)
        x
        nil)))
  (value [this] x)
  (left [this] nil)
  (right [this] nil))

(deftype LeftyNode [x ^INode l]
  INode
  (insert [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0)  this
       (= res -1) (make-lefty-node x (.insert l item comp))
       :else      (make-full-node x l (make-leaf-node item)))))
  (delete [this item comp]
     (with-comparator comp res item x
      (cond
       (= res 0)  l
       (= res -1) (let [node (.delete l item comp)]
                    (if node
                      (make-lefty-node x node)
                      (make-leaf-node x)))
       :else      this)))
  (doInsert [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0)  this
       (= res -1) (make-trans-node x (.doInsert l item comp) nil)
       :else      (make-trans-node x l (make-trans-node item)))))
  (doDelete [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0)  l
       (= res -1) (let [node (.doDelete l item comp)]
                    (if node
                      (make-trans-node x node nil)
                      (make-trans-node x)))
       :else      this)))
  (retrieve [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0)  x
       (= res -1) (.retrieve l item comp))))
  (value [_] x)
  (left [_] l)
  (right [_] nil))

(deftype RightyNode [x ^INode r]
  INode
  (insert [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0) this
       (= res 1) (make-righty-node x (.insert r item comp))
       :else     (make-full-node x (make-leaf-node item) r))))
  (delete [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0) r
       (= res 1) (let [node (.delete r item comp)]
                   (if node
                     (make-righty-node x node)
                     (make-leaf-node x)))
       :else      this)))
  (doInsert [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0) this
       (= res 1) (make-trans-node x nil (.doInsert r item comp))
       :else     (make-trans-node x (make-trans-node item) r))))
  (doDelete [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0) r
       (= res 1) (let [node (.doDelete r item comp)]
                   (if node
                     (make-trans-node x nil node)
                     (make-trans-node x)))
       :else      this))
    )
  (retrieve [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0)  x
       (= res 1) (.retrieve r item comp))))
  (value [_] x)
  (left [_] nil)
  (right [_] r))

(deftype FullNode [x ^INode l ^INode r]
  INode
  (insert [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0) this
       (= res 1) (make-full-node x l (.insert r item comp))
       :else     (make-full-node x (.insert l item comp) r))))
  (delete [this item comp]
    (with-comparator comp res item x
      (cond
       (= res  1) (let [rnode (.delete r item comp)]
                    (if rnode
                      (make-full-node x l rnode)
                      (make-lefty-node x l)))
       (= res -1) (let [lnode (.delete l item comp)]
                    (if lnode
                      (make-full-node x lnode r)
                      (make-righty-node x r)))
       :else      (let [^INode successor (loop [^INode node r]
                                           (let [smaller (.left node)]
                                             (if (nil? smaller)
                                               node
                                               (recur smaller))))
                        val (.value successor)]
                    (if (identical? r successor)
                      (if (nil? (.right r))
                        (make-lefty-node val l)
                        (make-full-node val l (.right r)))
                      (make-full-node val l (.delete r val comp)))))))
  (doInsert [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0) this
       (= res 1) (make-trans-node x l (.doInsert r item comp))
       :else     (make-trans-node x (.doInsert l item comp) r))))
  (doDelete [this item comp]
    (with-comparator comp res item x
      (cond
       (= res  1) (let [rnode (.doDelete r item comp)]
                    (if rnode
                      (make-trans-node x l rnode)
                      (make-trans-node x l nil)))
       (= res -1) (let [lnode (.doDelete l item comp)]
                    (if lnode
                      (make-trans-node x lnode r)
                      (make-trans-node x nil r)))
       :else      (let [^INode successor (loop [^INode node r]
                                           (let [smaller (.left node)]
                                             (if (nil? smaller)
                                               node
                                               (recur smaller))))
                        val (.value successor)]
                    (if (identical? r successor)
                      (if (nil? (.right r))
                        (make-trans-node val l nil)
                        (make-trans-node val l (.right r)))
                      (make-trans-node val l (.doDelete r val comp)))))))
  (retrieve [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0)  x
       (= res 1) (.retrieve r item comp)
       :else     (.retrieve l item comp))))
  (value [_] x)
  (left [_] l)
  (right [_] r))

;;(qref/def-create inode-ref INode)
;;(qref/def-deref inode-deref INode)
;;(qref/def-set inode-set! INode)

(defn- inode-ref ^"[Ldata.tree.bst.INode;" 
  [^INode value]
  (into-array INode [value]))

(defn- inode-deref ^INode
  [^"[Ldata.tree.bst.INode;" ref]
  (aget ^"[Ldata.tree.bst.INode;" ref (int 0)))

(defn- inode-set!
  [^"[Ldata.tree.bst.INode;" ref ^INode value]
  (aset ^"[Ldata.tree.bst.INode;" ref (int 0) value))

(deftype TransNode [x l r]
  INode
  (insert [this item comp]
    (let [^INode lnode (inode-deref l)
          ^INode rnode (inode-deref r)]
      (with-comparator comp res item x
        (cond
         (= res 0) this
         (= res 1) (cond
                    (and lnode rnode) (make-full-node x lnode (.insert rnode item comp))
                    rnode             (make-righty-node x (.insert rnode item comp))
                    lnode             (make-full-node x lnode (make-leaf-node item))
                    :else             (make-righty-node x (make-leaf-node item)))
         :else     (cond
                    (and lnode rnode) (make-full-node x (.insert lnode item comp) rnode)
                    lnode             (make-lefty-node x (.insert lnode item comp))
                    rnode             (make-full-node x (make-leaf-node item) rnode)
                    :else             (make-lefty-node x (make-lefty-node item)))))))

  (delete [this item comp]
    (let [^INode lnode (inode-deref l)
          ^INode rnode (inode-deref r)]
      (with-comparator comp res item x
        (cond
         (= res -1) (if lnode
                      (let [node (.delete lnode item comp)]
                        (cond
                         (and node rnode) (make-full-node x node rnode)
                         node             (make-lefty-node x node)
                         rnode            (make-righty-node x rnode)
                         :else            (make-leaf-node x)))
                      this)
         (= res  1) (if rnode
                      (let [node (.delete rnode item comp)]
                        (cond
                         (and node lnode) (make-full-node x lnode node)
                         node             (make-righty-node x node)
                         lnode            (make-lefty-node x lnode)
                         :else            (make-leaf-node x)))
                      this)
         :else      (if (and lnode rnode)
                      (let [^INode successor (loop [^INode node r]
                                               (let [smaller (.left node)]
                                                 (if smaller
                                                   (recur smaller)
                                                   node)))
                            val (.value successor)]
                        (if (identical? rnode successor)
                          (if (.right rnode)
                            (make-full-node val lnode (.right rnode))
                            (make-lefty-node val lnode))
                          (make-full-node val lnode (.delete rnode val comp))))
                      (or lnode rnode))))))

  (doInsert [this item comp]
    (with-comparator comp res item x
      (let [^INode rnode (inode-deref r)
            ^INode lnode (inode-deref l)]
        (cond
         (= res 0) this
         (= res 1) (do
                     (if rnode
                       (inode-set! r (.doInsert rnode item comp))
                       (inode-set! r (make-trans-node item)))
                     this)
         :else     (do
                     (if lnode
                       (inode-set! l  (.doInsert lnode item comp))
                       (inode-set! l (make-trans-node item)))
                     this)))))
  (doDelete [this item comp]
    (with-comparator comp res item x
      (let [^INode rnode (inode-deref r)
            ^INode lnode (inode-deref l)]
        (cond
         (= res  1) (do
                      (when rnode
                        (inode-set! r (.doDelete rnode item comp)))
                      this)
         (= res -1) (do
                      (when lnode
                        (inode-set! l (.doDelete lnode item comp)))
                      this)
         :else      (if (and lnode rnode)
                      (let [^INode successor (loop [^INode node rnode]
                                               (let [smaller (.left node)]
                                                 (if smaller
                                                   (recur smaller)
                                                   node)))
                            val (.value successor)]
                        (if (identical? rnode successor)
                          (make-trans-node val lnode (.right rnode))
                          (make-trans-node val lnode (.doDelete rnode val comp))))
                      (or lnode rnode))))))
  
  (retrieve [this item comp]
    (let [^INode lnode (inode-deref l)
          ^INode rnode (inode-deref r)]
      (with-comparator comp res item x
        (cond
         (= res 0)  x
         (and (= res -1) lnode) (.retrieve lnode item comp)
         (and (= res  1) rnode) (.retrieve rnode item comp)))))
  (value [_] x)
  (left [_] (inode-deref l))
  (right [_] (inode-deref r)))

(defn- make-leaf-node [item]
  (LeafNode. item))

(defn- make-lefty-node [item ^INode left]
  (LeftyNode. item left))

(defn- make-righty-node [item ^INode right]
  (RightyNode. item right))

(defn- make-full-node [item ^INode left ^INode right]
  (FullNode. item left right))

(defn- make-trans-node
  ([item]
     (TransNode. item (inode-ref nil) (inode-ref nil)))
  ([item ^INode left ^INode right]
     (TransNode. item (inode-ref left) (inode-ref right))))


;;=======  Seq Implementation   =======;;

(defn- seq-equals
  "Determines whether two sequences expand equally"
  [a b]
  (boolean
   (when (or (sequential? b) (instance? java.util.List b))
     (loop [a (seq a), b (seq b)]
       (when (= (nil? a) (nil? b))
         (or (nil? a)
             (when (= (first a) (first b))
               (recur (next a) (next b)))))))))

(defn ^{:private true :static true}
  memoize-by-id
  "Memoizes based upon a supplied object rather than the whole argument list"
  [f]
  (let [mem (atom {})]
    (fn [id & args]
      (if-let [e (find @mem id)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc id ret)
          ret)))))

(def ^{:doc "Generates a hash code for a sequence based upon a unique-id."
       :private true
       :static true}
  seq-hash
  (memoize-by-id
   (fn [coll]
     (reduce (fn [h x] (unchecked-add-int
                       (unchecked-multiply-int h 31)
                       (hash x)))
             1 coll))))

(def ^{:doc "Generates a count for a sequence based upon a unique-id."
       :private true
       :static true}
  seq-count
  (memoize-by-id
   (fn [coll]
     (reduce (fn [x _] (inc x)) 0 coll))))

(defn- ^:static push-stack
  [^INode node ^clojure.lang.ISeq stack asc]
  (let [f (if asc
            (fn [^INode x] (.left x))
            (fn [^INode x] (.right x)))]
    (loop [^INode t node
           ^clojure.lang.ISeq s stack]
      (if (nil? t)
        s
        (recur (f t) (cons t s))))))

(deftype Seq [^clojure.lang.IPersistentMap mdata ^clojure.lang.ISeq stack asc cnt id]
  Object
  (equals [this x] (seq-equals this x))
  (hashCode [this] (seq-hash id this))
  clojure.lang.IObj
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
                (Seq. mdata s asc (dec cnt) (Object.)))))
  IPersistentCollection
  (count [this] (or cnt (seq-count id this)))
  (empty [_] (list))
  (equiv [this x] (.equals this x))
  (cons [this item] (cons item this)))

(defn- make-seq
  ([^INode tree asc]
     (Seq. nil (push-stack tree nil asc) asc nil (Object.)))
  ([^INode tree asc cnt]
     (Seq. nil (push-stack tree nil asc) asc cnt (Object.)))
  ([mdata ^INode tree asc cnt]
     (Seq. mdata (push-stack tree nil asc) asc cnt (Object.)))
  ([mdata ^INode tree asc cnt id]
     (Seq. mdata (push-stack tree nil asc) asc cnt id)))

;;=======  Tree Implementations =======;;

(declare make-bst)

(deftype EmptyBinarySearchTree [^clojure.lang.IPersistentMap mdata
                                ^java.util.Comparator comparator]
  Object
  (equals [_ x]
    (if (isa? (type x) EmptyBinarySearchTree)
      (let [^EmptyBinarySearchTree x x]
        (= comparator (.comparator x)))
      false))
  (hashCode [this] (hash (map identity this)))
  clojure.lang.IObj
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
  (cons [_ item] (make-bst mdata comparator (make-leaf-node item) 1))
  IPersistentSet
  (disjoin [this _] this)
  (contains [_ __] false)
  (get [_ __] nil)
  Sorted
  (comparator [_] comparator)
  (entryKey [_ __] nil)
  (seqFrom [_ __ ___] nil)
  IEditableCollection
  (asTransient [this] nil))

(deftype BinarySearchTree [^clojure.lang.IPersistentMap mdata
                           ^java.util.Comparator comparator
                           ^INode tree
                           ^java.lang.Long count]
  Object
  (equals [_ x]
    (if (isa? (type x) BinarySearchTree)
      (let [^BinarySearchTree x x]
        (and
         (= comparator (.comparator x))
         (= tree (.tree x))))
      false))
  (hashCode [this] (hash (map identity this)))
  clojure.lang.IObj
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
  (cons [_ item]
    (BinarySearchTree. mdata comparator (.insert tree item comparator) (inc count)))
  IPersistentSet
  (disjoin [this item]
    (BinarySearchTree. mdata comparator (.delete tree item comparator) (inc count)))
  (contains [this x] (not (nil? (.get this x))))
  (get [_ x] (.retrieve tree x comparator))
  Sorted
  (comparator [_] comparator)
  (entryKey [this x] (when (contains? this) x))
  (seqFrom [_ item asc]
    (loop [^INode node tree
           stack nil]
      (if (nil? node)
        (when stack (make-seq stack asc))
        (with-comparator comparator res item (.value node)
          (cond
           (= res  0)           (make-seq (cons node stack) asc)
           (and asc (= res -1)) (recur (.left node) (cons node stack))
           asc                  (recur (.right node) stack)
           (= res 1)            (recur (.right node) (cons node stack))
           :else                (recur (.left node) stack))))))
  IEditableCollection
  (asTransient [this] nil))



(defn- make-bst [mdata comparator tree count]
  (BinarySearchTree. mdata comparator tree count))

(def ^:private def-comp
  clojure.lang.RT/DEFAULT_COMPARATOR)

(defn- ^:static build-tree
  "Returns a vector consisting of the tree and count of items"
  [^java.util.Comparator comparator vals]
  (when-let [coll (seq vals)]
    (let [[x & xs] coll
          root (make-leaf-node x)
          ins (fn [[^INode t c] v] [(.insert t v comparator) (inc c)])]
      (reduce ins [root 1] xs))))

(defn ^:static binary-search-tree
  "Returns a new binary search tree with supplied keys."
  ([]
     (EmptyBinarySearchTree. nil def-comp))
  ([& keys]
     (let [[tree count] (build-tree def-comp keys)]
       (BinarySearchTree. nil def-comp tree count))))

(defn ^:static binary-search-tree-by
  "Returns a new binary search tree with supplied keys, using the supplied comparator."
  ([^java.util.Comparator comparator]
     (EmptyBinarySearchTree. nil comparator))
  ([^java.util.Comparator comparator & keys]
     (let [[tree count] (build-tree def-comp keys)]
       (BinarySearchTree. nil comparator tree count))))

;;-- Transient Binary Search Tree


;;-- Pretty Printing
(defprotocol PrintableTree
  (print-tree [tree]))

(defn- p [t & xs]
  (print "<")
  (print t)
  (doseq [x xs]
    (print " ")
    (print-tree x))
  (print ">"))

(extend-protocol PrintableTree
  EmptyBinarySearchTree  (print-tree [x] (p "BST : 0 empty"))
  BinarySearchTree       (print-tree [x] (p "BST :" (.count x) (.tree x)))
  LeafNode               (print-tree [x] (p "LeafNode" (.value x)))
  LeftyNode              (print-tree [x] (p "LeftyNode" (.value x) (.left x)))
  RightyNode             (print-tree [x] (p "RightyNode" (.value x) (.right x)))
  FullNode               (print-tree [x] (p "FullNode" (.value x) (.left x) (.right x)))
  TransNode              (print-tree [x] (p "TransNode" (.value x) (.left x) (.right x)))
  Object                 (print-tree [x] (pr x))
  nil                    (print-tree [x] (print "nil")))
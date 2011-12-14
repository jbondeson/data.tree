0(ns ^{:doc "Persistent Binary Search Tree"
      :author "Jeremy Bondeson"}
  data.tree.bst
  (:refer-clojure :exclude [comparator comp])
  (:import (clojure.lang Seqable Sequential ISeq IPersistentSet
                         IPersistentCollection Counted)))

(defmacro with-comparator
  "Macro to automate the binding of a comparator result"
  ([^java.util.Comparator comparator result x y & body]
     `(let [~result (. ~comparator (compare ~x ~y))]
        ~@body)))

(defprotocol Node
  (insert [^data.tree.bst.Node node item ^java.util.Comparator comp])
  (delete [^data.tree.bst.Node node item ^java.util.Comparator comp])
  (retrieve [^data.tree.bst.Node node item ^java.util.Comparator comp])
  (value [^data.tree.bst.Node node])
  (left [^data.tree.bst.Node node])
  (right [^data.tree.bst.Node node]))

(declare make-leaf-node make-lefty-node make-righty-node make-full-node)

(deftype LeafNode [x]
  Node
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
  (retrieve [this item comp]
    (with-comparator comp res item x
      (if (= res 0)
        x
        nil)))
  (value [this] x)
  (left [this] nil)
  (right [this] nil))

(deftype LeftyNode [x ^data.tree.bst.Node l]
  Node
  (insert [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0)  this
       (= res -1) (make-lefty-node x (insert l item comp))
       :else      (make-full-node x l (make-leaf-node item)))))
  (delete [this item comp]
     (with-comparator comp res item x
      (cond
       (= res 0)  l
       (= res -1) (make-lefty-node x (delete l item comp))
       :else      this)))
  (retrieve [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0)  x
       (= res -1) (retrieve l item comp))))
  (value [_] x)
  (left [_] l)
  (right [_] nil))

(deftype RightyNode [x ^data.tree.bst.Node r]
  Node
  (insert [this item  comp]
    (with-comparator comp res item x
      (cond
       (= res 0) this
       (= res 1) (make-righty-node x (insert r item comp))
       :else     (make-full-node x (make-leaf-node item) r))))
  (delete [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0) r
       (= res 1) (make-righty-node x (delete r item comp))
       :else      this)))
  (retrieve [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0)  x
       (= res 1) (retrieve r item comp))))
  (value [_] x)
  (left [_] nil)
  (right [_] r))

(deftype FullNode [x ^data.tree.bst.Node l ^data.tree.bst.Node r]
  Node
  (insert [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0) this
       (= res 1) (make-full-node x l (insert r item comp))
       :else     (make-full-node x (insert l item comp) r))))
  (delete [this item comp]
    (with-comparator comp res item x
      (cond
       (= res  1) (let [rnode (delete r item comp)]
                    (if (nil? rnode)
                      (make-lefty-node x l)
                      (make-full-node x l rnode)))
       (= res -1) (let [lnode (delete l item comp)]
                    (if (nil? lnode)
                      (make-righty-node x r)
                      (make-full-node x lnode r)))
       :else      (let [successor (loop [node (right r)]
                                    (let [smaller (left node)]
                                      (if (nil? smaller)
                                        node
                                        (recur smaller))))
                        val (value successor)]
                    (if (identical? r successor)
                      (if (nil? (right r))
                        (make-lefty-node val l)
                        (make-full-node val l (right r)))
                      (make-full-node val l (delete r val successor)))))))
  (retrieve [this item comp]
    (with-comparator comp res item x
      (cond
       (= res 0)  x
       (= res 1) (retrieve r item comp)
       :else     (retrieve l item comp))))
  (value [_] x)
  (left [_] l)
  (right [_] r))


(defn- make-leaf-node [item]
  (LeafNode. item))

(defn- make-lefty-node [item left]
  (LeftyNode. item left))

(defn- make-righty-node [item right]
  (RightyNode. item right))

(defn- make-full-node [item left right]
  (FullNode. item left right))


;;=======  Tree Implementations =======;;

(declare make-empty-bst make-bst)

(defn- seq-equals [a b]
  (boolean
   (when (or (sequential? b) (instance? java.util.List b))
     (loop [a (seq a), b (seq b)]
       (when (= (nil? a) (nil? b))
         (or
          (nil? a)
          (when (= (first a) (first b))
            (recur (next a) (next b)))))))))

(deftype EmptyBinarySearchTree [^clojure.lang.IPersistentMap mdata
                                ^java.util.Comparator comparator]
  Object
  (equals [_ x]
    (if (isa? (type x) EmptyBinarySearchTree)
      (= comparator (.comparator x))
      false))
  (hashCode [this] (hash (map identity this)))
  clojure.lang.IObj
  (meta [_] mdata)
  (withMeta [_ mdata] (EmptyBinarySearchTree. mdata comparator))
  Sequential
  Seqable
  (seq [_] nil)
  ISeq
  (first [_] nil)
  (more [this] this)
  (next [_] nil)
  IPersistentCollection
  (count [_] 0)
  (empty [this] this)
  (equiv [this x] (.equals this x))
  (cons [_ item] (make-bst mdata comparator (make-leaf-node item) 1))
  IPersistentSet
  (disjoin [this _] this)
  (contains [this _] false)
  (get [this _] nil))

(deftype BinarySearchTree [^clojure.lang.IPersistentMap mdata
                           ^java.util.Comparator comparator
                           ^data.tree.bst.Node tree
                           ^java.lang.Long count]
  Object
  (equals [_ x]
    (if (isa? (type x) BinarySearchTree)
      (and
       (= comparator (.comparator x))
       (= tree (.tree x)))
      false))
  (hashCode [this] (hash (map identity this)))
  clojure.lang.IObj
  (meta [_] mdata)
  (withMeta [_ mdata] (BinarySearchTree. mdata comparator tree count))
  Sequential
  Seqable
  (seq [_] nil)
  ISeq
  (first [_] nil)
  (more [this] this)
  (next [_] nil)
  IPersistentCollection
  (count [_] count)
  (empty [this] (EmptyBinarySearchTree. mdata comparator))
  (equiv [this x] (.equals this x))
  (cons [_ item]
    (BinarySearchTree. mdata comparator (insert tree item comparator) (inc count)))
  IPersistentSet
  (disjoin [this item]
    (BinarySearchTree. mdata comparator (remove tree item comparator) (inc count)))
  (contains [this x] (not (nil? (.get this x))))
  (get [_ x] (retrieve tree x comparator)))


(def ^:private def-comp
  clojure.lang.RT/DEFAULT_COMPARATOR)

(defn- ^:static make-empty-bst
  ([]
     (EmptyBinarySearchTree. nil def-comp))
  ([^java.util.Comparator comparator]
     (EmptyBinarySearchTree. nil comparator)))

(defn- ^:static make-bst
  ([^clojure.lang.IPersistentMap mdata
    ^data.tree.bst.Node tree
    ^java.lang.Long count]
     (BinarySearchTree. mdata def-comp tree count))
  ([^clojure.lang.IPersistentMap mdata
    ^java.util.Comparator comparator
    ^data.tree.bst.Node tree
    ^java.lang.Long count]
      (BinarySearchTree. mdata comparator tree count)))

(defn- ^:static build-tree
  [^java.util.Comparator comparator vals]
  (when-let [coll (seq vals)]
    (let [[x & xs] coll
          root (make-leaf-node x)
          ins (fn [[t c] v] [(insert t v comparator) (inc c)])
          [tree count] (reduce ins [root 1] xs)]
      (make-bst nil tree count))))

(defn ^:static binary-search-tree
  ([]
     (make-empty-bst))
  ([& vals]
     (build-tree def-comp vals)))

(defn ^:static binary-search-tree-by
  ([^java.util.Comparator comparator]
     (make-empty-bst comparator))
  ([^java.util.Comparator comparator & vals]
     (build-tree comparator vals)))

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
  LeafNode               (print-tree [x] (p "LeafNode" (value x)))
  LeftyNode              (print-tree [x] (p "LeftyNode" (value x) (left x)))
  RightyNode             (print-tree [x] (p "RightyNode" (value x) (right x)))
  FullNode               (print-tree [x] (p "FullNode" (value x) (left x) (right x)))
  Object                 (print-tree [x] (pr x))
  nil                    (print-tree [x] (print "nil")))
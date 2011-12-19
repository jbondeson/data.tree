0(ns ^{:doc "Persistent Binary Search Tree"
      :author "Jeremy Bondeson"}
  data.tree.bst
  (:refer-clojure :exclude [comparator comp])
  (:import (clojure.lang Seqable Sequential ISeq IPersistentSet
                         IPersistentCollection Counted Sorted
                         Reversible)))

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
       :else      (let [successor (loop [node r]
                                    (let [smaller (left node)]
                                      (if (nil? smaller)
                                        node
                                        (recur smaller))))
                        val (value successor)]
                    (if (identical? r successor)
                      (if (nil? (right r))
                        (make-lefty-node val l)
                        (make-full-node val l (right r)))
                      (make-full-node val l (delete r val comp)))))))
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

(defn memoize-by-id
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
  [^data.tree.bst.Node node ^clojure.lang.ISeq stack asc]
  (let [f (if asc
            (fn [x] (left x))
            (fn [x] (right x)))]
    (loop [t node
           s stack]
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
  (first [_] (value (first stack)))
  (more [this] (or (next this) (list)))
  (next [_] (let [f (if asc
                      (fn [x] (right x))
                      (fn [x] (left x)))
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
  ([tree asc]
     (Seq. nil (push-stack tree nil asc) asc nil (Object.)))
  ([tree asc cnt]
     (Seq. nil (push-stack tree nil asc) asc cnt (Object.)))
  ([mdata tree asc cnt]
     (Seq. mdata (push-stack tree nil asc) asc cnt (Object.)))
  ([mdata tree asc cnt id]
     (Seq. mdata (push-stack tree nil asc) asc cnt id)))

;;=======  Tree Implementations =======;;

(declare make-empty-bst make-bst)

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
  (seq [_] (make-seq tree true count))
  ISeq
  (first [_] (value tree))
  (more [this] this)
  (next [_] nil)
  Reversible
  (rseq [_] (make-seq tree false count))
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
  (get [_ x] (retrieve tree x comparator))
  Sorted
  (comparator [_] comparator)
  (entryKey [this x] (when (contains? this) x))
  (seqFrom [_ item asc]
    (loop [node tree
           stack nil]
      (if (nil? node)
        (when stack (make-seq stack asc))
        (with-comparator comparator res item (value node)
          (cond
           (= res  0)           (make-seq (cons node stack) asc)
           (and asc (= res -1)) (recur (left node) (cons node stack))
           asc                  (recur (right node) stack)
           (= res 1)            (recur (right node) (cons node stack))
           :else                (recur (left node) stack)))))))


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
  "Returns a vector consisting of the tree and count of items"
  [^java.util.Comparator comparator vals]
  (when-let [coll (seq vals)]
    (let [[x & xs] coll
          root (make-leaf-node x)
          ins (fn [[t c] v] [(insert t v comparator) (inc c)])]
      (reduce ins [root 1] xs))))

(defn ^:static binary-search-tree
  ([]
     (make-empty-bst))
  ([& vals]
     (let [[tree count] (build-tree def-comp vals)]
       (make-bst nil tree count))))

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
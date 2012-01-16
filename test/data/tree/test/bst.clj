(ns data.tree.test.bst
  (:use [data.tree.bst])
  (:use [clojure.test])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:require [clojurecheck.core :as cc])
  (:require [data.compare :as cmp])
  (:require [data.util.tref :as tref])
  (:use [data.tree.bst.transient])
  (:use data.tree.bst.core)
  (:import (data.tree.bst EmptyBinarySearchTree BinarySearchTree)))

;; Bring in some private methods from data.tree.bst
(def ^:private build
  (ns-resolve 'data.tree.bst 'build-tree)) 

(def ^:private make-trans-node
  (ns-resolve 'data.tree.bst.transient 'make-trans-node))

(defn build-def
  [& args]
  (build cmp/default args))

(defn build-trans-def
  [& args]
  (when-let [coll (seq args)]
    (let [[x & xs] coll
          edit (tref/edit-context)
          root (make-trans-node edit x nil nil)
          ins (fn [[^data.tree.bst.core.INode t cnt] v]
                [(insert! t edit v cmp/default) (inc cnt)])]
      (dosync
       (reduce ins [root 1] xs)))))

(def test-trees
  {:leaf        '(50)
   :s-lefty     '(50 25)
   :m-lefty     '(50 25 20)
   :l-lefty     '(50 25 20 15 10 5)
   :s-righty    '(50 75)
   :m-righty    '(50 75 80)
   :l-righty    '(50 75 80 85 90 95)
   :s-full      '(50 25 75)
   :l-full      '(50 25 75 15 35 85 65)
   })

(defn- mmap
  [f coll]
  (into {} (map (fn [[k v]] [k (f v)]) coll)))

(def trees
  (mmap (fn [v] (first (apply build-def v))) test-trees))

(def bsts
  (mmap (partial apply binary-search-tree) test-trees))

(defn make-transients
  []
  (mmap (fn [v] (first (apply build-trans-def v))) test-trees))

(def transients
  (make-transients))

(declare identical-structure? ins-def del-def ret-def
         ins!-def del!-def get-head)

;; Empty Tests
(deftest one-empty-bst
  (is (= (binary-search-tree) (binary-search-tree))))

(deftest empty-contract
  (let [e (binary-search-tree)]
    (is (= 0 (count e)))
    (is (nil? (first e)))
    (is (nil? (next e)))
    (is (not (contains? e 0)))
    (is (not (contains? e nil)))
    (is (nil? (get e 0)))
    (is (nil? (get e nil)))
    (is (identical? e (empty e)))))

;;  Binary Search Tree Tests
(defmacro ^:private tree-prop
  [msg tree set items & body]
  `(cc/property
    ~msg
    [~items (cc/list (cc/int :lower 0 :upper 32767))]
    (let [~tree (apply binary-search-tree ~items)
          ~set (apply sorted-set ~items)]
      ~@body)))

(deftest binary-search-tree-properties
  (tree-prop
   "ascending sequence"
   tree set list
   (is (= (seq set) (seq tree))))
  (tree-prop
   "descending sequence"
   tree set list
   (is (= (seq (reverse set)) (rseq tree))))
  (tree-prop
   "count"
   tree set list
   (is (= (count set) (count tree))))
  (tree-prop
   "seq from"
   tree set list
   (let [from (if (seq list)
                (nth list (int (/ (count list) 2)))
                nil)]
     (is
      (= (seq (.seqFrom set from true))
         (seq (.seqFrom tree from true))))))  
  (tree-prop
   "member tests"
   tree set items
   (is (every? #(= (contains? set %) (contains? tree %)) items))))

;;  Operation Tests
(defmacro insertion-tests
  [name ins-fn acc-fn tree]
  `(deftest ~name
     (are [t x r] (identical-structure? (~acc-fn (~ins-fn (t ~tree) x)) r)
          :leaf      10   '(50 10  nil)
          :leaf      60   '(50 nil 60 )
          :leaf      50   50
          :s-lefty   75   '(50 25 75)
          :s-lefty   20   '(50 (25 20 nil) nil)
          :s-lefty   50   '(50 25 nil)
          :s-righty  25   '(50 25 75)
          :s-righty  80   '(50 nil (75 nil 80))
          :s-righty  50   '(50 nil 75)
          :s-full    20   '(50 (25 20 nil) 75)
          :s-full    80   '(50 25 (75 nil 80)))))

(defmacro deletion-tests
  [name del-fn acc-fn tree]
  `(deftest ~name
     (are [t x r] (identical-structure? (~acc-fn (~del-fn (t ~tree) x)) r)
          :leaf      50   nil
          :leaf      99   50
          :leaf      40   50
          :s-lefty   50   25
          :s-lefty   25   50
          :m-lefty   50  '(25 20 nil)
          :m-lefty   25  '(50 20 nil)
          :m-lefty   20  '(50 25 nil)
          :s-righty  50   75
          :s-righty  75   50
          :m-righty  50  '(75 nil 80)
          :m-righty  75  '(50 nil 80)
          :m-righty  80  '(50 nil 75))))

(defmacro retrieval-tests
  [name ret-fn tree]
  `(deftest ~name
     (are [t x r] (= (~ret-fn (t ~tree) x) r)
          :leaf      50  50
          :leaf      99  nil
          :leaf      40  nil
          :l-lefty   50  50
          :l-lefty   99  nil
          :l-lefty   25  25
          :l-lefty   5   5
          :l-lefty   19  nil
          :l-righty  50  50
          :l-righty  40  nil
          :l-righty  75  75
          :l-righty  95  95
          :l-righty  89  nil
          :l-full    50  50
          :l-full    25  25
          :l-full    75  75
          :l-full    15  15
          :l-full    85  85
          :l-full    79  nil
          :l-full    19  nil)))

(insertion-tests transient-insert!  ins!-def identity (make-transients))
(insertion-tests transient-insert   ins-def  identity transients)
(insertion-tests persistent-insert  ins-def  identity trees)
(insertion-tests persistent-insert! ins!-def identity trees)
(insertion-tests bst-insert         conj     get-head bsts)

(deletion-tests transient-delete!  del!-def identity (make-transients))
(deletion-tests transient-delete   del-def  identity transients)
(deletion-tests persistent-delete  del-def  identity trees)
(deletion-tests persistent-delete! del!-def identity trees)
(deletion-tests bst-delete         disj     get-head bsts)

(retrieval-tests transient-retrieve  ret-def transients)
(retrieval-tests persistent-retrieve ret-def trees)
(retrieval-tests bst-retrieve        get     bsts)

;;==== Helper Functions ====
(defn get-head
  [tree]
  (cond
   (isa? (type tree) EmptyBinarySearchTree) nil
   (isa? (type tree) BinarySearchTree)      (.tree tree)
   :else                                    tree))

;; Alias node functions to take the default comparator
(defn ins-def [^data.tree.bst.core.INode tree item]
  (try+
   (insert tree item cmp/default)
   (catch :duplicate-key? _
     tree)))
(defn del-def [^data.tree.bst.core.INode tree item]
  (try+
   (delete tree item cmp/default)
   (catch :not-found? _
     tree)))
(defn ret-def [^data.tree.bst.core.INode tree item]
  (retrieve tree item cmp/default))
(defn ins!-def [^data.tree.bst.core.INode tree item]
  (try+
   (insert! tree (tref/edit-context) item cmp/default)
   (catch :duplicate-key? _
     tree)))
(defn del!-def [^data.tree.bst.core.INode tree item]
  (try+
   (delete! tree (tref/edit-context) item cmp/default)
   (catch :not-found? _
     tree)))

(defn flatten-tree [^data.tree.bst.core.INode tree]
  (loop [res '[]
         node tree
         queue '()]
    (cond
     (and (nil? node) (empty? queue)) res
     (nil? node) (recur res (first queue) (rest queue))
     :else (recur (conj res (value node))
                  (left node)
                  (cons (right node) queue)))))

(defn tree-structure [^data.tree.bst.core.INode tree]
  (when tree
    (let [l (left tree)
          r (right tree)
          v (value tree)]
      (if (and (nil? l) (nil? r))
        v
        (list v (tree-structure l) (tree-structure r))))))

(defn identical-traversal? [^data.tree.bst.core.INode tree coll]
  (= (flatten-tree tree) coll))

(defn identical-structure? [^data.tree.bst.core.INode tree structure]
  (= (tree-structure tree) structure))

(defn- make-queue
  [& args]
  (reduce conj clojure.lang.PersistentQueue/EMPTY args))

(defn- sane-round
  ([n]
     (int (.setScale (bigdec n) 0 java.math.RoundingMode/HALF_EVEN))))

(defn balanced-seq
  [count]
  (loop [coll '[]
         queue (make-queue [0 (dec count)])]
    (if (empty? queue)
      coll
      (let [head (peek queue)
            nqueue (pop queue) 
            [lo hi] head
            val (+ lo (sane-round (/ (- hi lo) 2)))
            ncoll (conj coll val)
            vlo [lo (dec val)]
            vhi [(inc val) hi]]        
        (cond
         (and (not= lo val) (not= val hi)) (recur ncoll (conj nqueue vlo vhi))
         (not= lo val)                     (recur ncoll (conj nqueue vlo))
         (not= val hi)                     (recur ncoll (conj nqueue vhi))
         :else                             (recur ncoll nqueue))))))


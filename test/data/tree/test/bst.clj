(ns data.tree.test.bst
  (:use [data.tree.bst])
  (:use [clojure.test])
  (:require [data.tree.quickref :as qref])
  (:import (data.tree.bst EmptyBinarySearchTree BinarySearchTree
                          LeafNode LeftyNode RightyNode FullNode
                          TransNode
                          INode 
                          )))

(def ^:private c
  clojure.lang.RT/DEFAULT_COMPARATOR)

;; Bring in some private methods from data.tree.bst
(def ^:private build
  (ns-resolve 'data.tree.bst 'build-tree)) 

(defn build-def
  [& args]
  (build c args))

(defn build-trans-def
  [& args]
  (when-let [coll (seq args)]
    (let [[x & xs] coll
          root (TransNode. x (qref/ref INode nil) (qref/ref INode nil))
          ins (fn [[^INode t cnt] v] [(.doInsert t v c) (inc cnt)])]
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

(def transients
  (mmap (fn [v] (first (apply build-trans-def v))) test-trees))

(declare identical-structure? ins-def del-def ret-def
         doins-def dodel-def)

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


;;  Node Tests
(deftest node-insertion
  (are [t x r] (identical-structure? (ins-def (t trees) x) r)
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
       :s-full    80   '(50 25 (75 nil 80))))

(deftest node-deletion
  (are [t x r] (identical-structure? (del-def (t trees) x) r)
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
       :m-righty  80  '(50 nil 75)
       :s-full    50  '(75 25 nil)
       :s-full    25  '(50 nil 75)
       :s-full    75  '(50 25 nil)
       :l-full    50  '(65 (25 15 35) (75 nil 85))))

(deftest node-retrieve
  (are [t x r] (= (ret-def (t trees) x) r)
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
       :l-full    19  nil))


;;==== Helper Functions ====

;; Alias node functions to take the default comparator
(defn ins-def [^INode tree item] (.insert tree item c))
(defn del-def [^INode tree item] (.delete tree item c))
(defn ret-def [^INode tree item] (.retrieve tree item c))
(defn doins-def [^INode tree item] (.doInsert tree item c))
(defn dodel-def [^INode tree item] (.doDelete tree item c))

(defn flatten-tree [^INode tree]
  (loop [res '[]
         node tree
         queue '()]
    (cond
     (and (nil? node) (empty? queue)) res
     (nil? node) (recur res (first queue) (rest queue))
     :else (recur (conj res (.value node))
                  (.left node)
                  (cons (.right node) queue)))))

(defn tree-structure [^INode tree]
  (when tree
    (let [l (.left tree)
          r (.right tree)
          v (.value tree)]
      (if (and (nil? l) (nil? r))
        v
        (list v (tree-structure l) (tree-structure r))))))

(defn identical-traversal? [^INode tree coll]
  (= (flatten-tree tree) coll))

(defn identical-structure? [^INode tree structure]
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

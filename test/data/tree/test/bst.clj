(ns data.tree.test.bst
  (:use [data.tree.bst])
  (:use [clojure.test])
  (:import (data.tree.bst EmptyBinarySearchTree BinarySearchTree
                          LeafNode LeftyNode RightyNode FullNode
                          )))
(def ^:private c
  clojure.lang.RT/DEFAULT_COMPARATOR)

(declare identical-structure? )

;; Empty Tests
(deftest one-empty-bst
  (is (= (binary-search-tree) (binary-search-tree))))

(deftest empty-contract
  (let [e (bst)]
    (is (= 0 (count e)))
    (is (nil? (first e)))
    (is (nil? (next e)))
    (is (not (contains? e 0)))
    (is (not (contains? e nil)))
    (is (nil? (get e 0)))
    (is (nil? (get e nil)))
    (is (identical? e (empty e)))))

;; Insert tests
(deftest node-insertion
  (is (identical-structure?
       (insert (LeafNode. 50) 10 c)
       '(50 10 nil)))
  (is (identical-structure?
       (insert (LeafNode. 50) 60 c)
       '(50 nil 60)))
  (is (identical-structure?
       (insert (LeftyNode. 50 (LeafNode. 10)) 60 c)
       '(50 10 60)))
  (is (identical-structure?
       (insert (RightyNode. 50 (LeafNode. 100)) 0 c)
       '(50 0 100)))
  (is (identical-structure?
       (insert (LeftyNode. 50 (LeafNode. 10)) 0 c)
       '(50 (10 0 nil) nil)))
  (is (identical-structure?
       (insert (RightyNode. 50 (LeafNode. 100)) 200 c)
       '(50 nil (100 nil 200))))
  )

;;==== Helper Functions ===

(defn flatten-tree [tree]
  (loop [res '[]
         node tree
         queue '()]
    (cond
     (and (nil? node) (empty? queue)) res
     (nil? node) (recur res (first queue) (rest queue))
     :else (recur (conj res (value node))
                  (left node)
                  (cons (right node) queue)))))
(defn tree-structure [tree]
  (when tree
    (let [l (left tree)
          r (right tree)
          v (value tree)]
      (if (and (nil? l) (nil? r))
        v
        (list v (tree-structure l) (tree-structure r))))))

(defn identical-traversal? [tree coll]
  (= (flatten-tree tree) coll))

(defn identical-structure? [tree structure]
  (= (tree-structure tree) structure))
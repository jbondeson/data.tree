(ns ^{:doc "Tests for the treap data structure"
      :author "Jeremy Bondeson"}
  data.tree.test.treap
  (:use [data.tree.treap])
  (:use [data.tree.treap.core])
  (:use [clojure.test])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:require [clojurecheck.core :as cc])
  (:require [data.compare :as cmp])
  (:import (data.tree.treap.core LeafNode LeftyNode RightyNode FullNode))
  )

(def ^:private build-node-tree
  (ns-resolve 'data.tree.treap.core 'build-tree))

(defn build-node-tree-def
  [vals]
  (first (build-node-tree cmp/default vals)))

(defmacro ^:private tree-test
  [tree test-fn]
  `(when ~tree
     (loop [q# (conj (clojure.lang.PersistentQueue/EMPTY) ~tree)]
       (when-not (empty? q#)
         (let [head# (peek q#)
               next# (pop q#)
               r#    (right head#)
               l#    (left head#)]
           (~test-fn head#)
           (cond
            (and l# r#) (recur (conj next# l# r#))
            l#          (recur (conj next# l#))
            r#          (recur (conj next# r#))
            :else       (recur next#))))))
  )

(defmacro ^:private left<?
  [node]
  `(if-let [l# (left ~node)]
     (= -1 (.compare cmp/default (value l#) (value ~node)))
     true))

(defmacro ^:private right>?
  [node]
  `(if-let [r# (right ~node)]
     (= 1 (.compare cmp/default (value r#) (value ~node)))
     true))

(defmacro ^:private priority<?
  [node]
  `(let [r# (right ~node)
         l# (left ~node)
         rp# (if r# (priority r#) (Double/MAX_VALUE))
         lp# (if l# (priority l#) (Double/MAX_VALUE))
         p#   (priority ~node)]
     (and (<= p# rp#) (<= p# lp#))))

(defmacro ^:private node-contains?
  [node item]
  `(when (retrieve ~node ~item cmp/default) true))

(defmacro ^:private node-tree-prop
  [msg tree set items & body]
  `(cc/property
    ~msg
    [~items (cc/list (cc/int :lower 0 :upper 32767))]
    (let [~tree (build-node-tree-def ~items)
          ~set (apply sorted-set ~items)]
      ~@body)))

(deftest node-tree-properties
  (node-tree-prop
   "search tree structure"
   tree set list
   (tree-test tree #(is (and (left<? %) (right>? %)))))
  
  (node-tree-prop
     "priority min-heap"
     tree set list
     (tree-test tree #(is (priority<? %))))
  
  (node-tree-prop
   "membership tests"
   tree set list
   (is (every? #(= (contains? set %) (node-contains? tree %)) list)))
  )
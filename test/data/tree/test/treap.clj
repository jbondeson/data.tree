(ns ^{:doc "Tests for the treap data structure"
      :author "Jeremy Bondeson"}
  data.tree.test.treap
  (:refer-clojure :exclude [merge])
  (:use [data.tree.treap])
  (:use [data.tree.treap.core])
  (:use [data.tree.treap.polymorphic])
  (:use [clojure.test])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:require [clojurecheck.core :as cc])
  (:require [data.compare :as cmp])
  (:import (data.tree.treap.polymorphic LeafNode LeftyNode RightyNode FullNode)))

(defmacro tree-test
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
            :else       (recur next#)))))))

(defmacro left<?
  [node]
  `(if-let [l# (left ~node)]
     (= -1 (.compare cmp/default (value l#) (value ~node)))
     true))

(defmacro right>?
  [node]
  `(if-let [r# (right ~node)]
     (= 1 (.compare cmp/default (value r#) (value ~node)))
     true))

(defmacro priority<?
  [node]
  `(let [r# (right ~node)
         l# (left ~node)
         rp# (if r# (priority r#) (Double/MAX_VALUE))
         lp# (if l# (priority l#) (Double/MAX_VALUE))
         p#   (priority ~node)]
     (and (<= p# rp#) (<= p# lp#))))

(defmacro node-contains?
  [node item]
  `(when (retrieve ~node ~item cmp/default) true))

(defmacro node-tree-prop
  [fn-build msg tree set items & body]
  `(cc/property
    ~msg
    [~items (cc/list (cc/int :lower 0 :upper 32767))]
    (let [~tree (~fn-build ~items)
          ~set (apply sorted-set ~items)]
      ~@body)))

(defmacro node-tree-prop-tests
  [name fn-build]
  `(deftest ~name ;;node-tree-properties
     (node-tree-prop ~fn-build
      "search tree structure"
      tree# set# list#
      (tree-test tree# #(is (and (left<? %) (right>? %)))))
     
     (node-tree-prop ~fn-build
      "priority min-heap"
      tree# set# list#
      (tree-test tree# #(is (priority<? %))))
     
     (node-tree-prop ~fn-build
      "membership tests"
      tree# set# list#
      (is (every? #(= (contains? set# %) (node-contains? tree# %)) list#)))))
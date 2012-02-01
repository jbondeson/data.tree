(ns ^{:doc "Comparison helpers"
      :author "Jeremy Bondeson"}
  data.compare
  (:refer-clojure :exclude [comparator comp case])
  (:import (java.util Comparator)))

(set! *warn-on-reflection* true)

(defmacro
  with-compare
  "Macro to automate the binding of a comparator result"
  ([^java.util.Comparator comparator result lhs rhs & body]
     `(let [^int ~result (int (.compare ~comparator ~lhs ~rhs))]
        ~@body)))


(defmacro
  case
  [^Comparator comparator lhs rhs & options]
  (let [res (gensym)
        opts (apply hash-map options)]
    `(let [^int ~res (.compare ~comparator ~lhs ~rhs)]
       (clojure.core/cond
        ~@(if (:= opts) (list (list '= 0 res) (:= opts)))
        ~@(if (:> opts) (list (list '= 1 res) (:> opts)))
        ~@(if (:< opts) (list (list '= -1 res) (:< opts)))))))

(defmacro
  if*
  ([^Comparator comparator test lhs rhs then]
     `(if* ~comparator ~test ~lhs ~rhs ~then nil))
  ([^Comparator comparator test lhs rhs then else]
     `(if (= ~test (.compare ~comparator ~lhs ~rhs))
        ~then
        ~else)))

(defmacro
  if=
  ([^Comparator comparator lhs rhs then]
     `(if* ~comparator 0 ~lhs ~rhs ~then))
  ([^Comparator comparator lhs rhs then else]
     `(if* ~comparator 0 ~lhs ~rhs ~then ~else)))

(defmacro
  if-not=
  ([^Comparator comparator lhs rhs then]
     `(if* ~comparator 0 ~lhs ~rhs nil ~then))
  ([^Comparator comparator lhs rhs then else]
     `(if* ~comparator 0 ~lhs ~rhs ~else ~then)))

(defmacro
  if<
  ([^Comparator comparator lhs rhs then]
     `(if* ~comparator -1 ~lhs ~rhs ~then))
  ([^Comparator comparator lhs rhs then else]
     `(if* ~comparator -1 ~lhs ~rhs ~then ~else)))

(defmacro
  if>
  ([^Comparator comparator lhs rhs then]
     `(if* ~comparator 1 ~lhs ~rhs ~then))
  ([^Comparator comparator lhs rhs then else]
     `(if* ~comparator 1 ~lhs ~rhs ~then ~else)))


(def ^{:doc "Default comparator"
       :static true}
  default
  clojure.lang.RT/DEFAULT_COMPARATOR)
(ns data.tree.quickref
  (:refer-clojure :exclude [set! ref deref])
  (:import (java.lang.reflect Array)))

(defn ref
  "Creates and returns a quick (non-stm controlled) reference.
  NOTE: These object should never leak outside their scope."
  [^Class type val]
  (into-array type [val]))

(defn deref
  "Dereferences a quick reference and returns the current value."
  [qref]
  (aget qref (int 0)))

(defn set!
  "Updates the value of a quick reference."
  [qref val]
  (aset qref (int 0) val))


(defn- resolve-type ^Class
  [value]
  (if (symbol? value)
    (resolve value)
    (Class/forName (str value))))

(defn- array-class ^Class
  [^Class type]
  (class (into-array type '())))


(defmacro def-create
  [name type]
  (let [^Class typeclass (resolve-type type)
        ^Class aclass    (array-class typeclass)]
    (let [value (gensym)
          array (gensym)]
      `(def ~(vary-meta name assoc
                        :tag aclass
                        :arglists (list 'quote '([value]))
                        :static true
                        :private true)
         (fn [~(with-meta value {:tag typeclass})]
           (into-array ~typeclass [~value])
           )))))

(defmacro def-deref
  [name type]
  (let [^Class typeclass (resolve-type type)
        ^Class aclass    (array-class typeclass)]
    (let [qref (gensym)]
      `(def ~(vary-meta name assoc
                        :tag typeclass
                        :arglists (list 'quote '([ref]))
                        :static true
                        :private true
                        ;;:inline (fn [a] `(aget ~a (int 0)))
                        ;;:inline-arities #{1}
                        )
         (fn [~(with-meta qref {:tag aclass})]
           (aget ~qref (int 0)))))))

(defmacro def-set
  [name type]
  (let [^Class typeclass (resolve-type type)
        ^Class aclass    (array-class typeclass)]
    (let [qref (gensym)
          value (gensym)]
      `(def ~(vary-meta name assoc
                        :tag aclass
                        :arglists (list 'quote '([ref value]))
                        :static true
                        :private true
                        ;;:inline (fn [a v] `(aset ~a (int 0) ~v))
                        ;;:inline-arities #{2}
                        )
         (fn [~(with-meta qref {:tag aclass})
             ~(with-meta value {:tag typeclass})]
           (aset ~qref (int 0) ~value))))))

(comment
  (defmacro def-ref
    [name type]
    (let [^Class typeclass (resolve-type type)
          ^Class aclass    (array-class typeclass)]
      `(def ~(vary-meta name assoc
                        :tag aclass
                        :arglists (list 'quote '([value]))
                        :static true
                        :inline (fn [t v] `(into-array ~t [~v]))
                        :inline-arities #{2})
         (fn [value#] (into-array ~typeclass [value#])))))

  (defmacro def-create
  [name type]
  (let [^Class typeclass (resolve-type type)
        ^Class aclass    (array-class typeclass)]
    (let [value (gensym)
          array (gensym)]
      `(def ~(vary-meta name assoc
                        :tag aclass
                        :arglists (list 'quote '([value]))
                        :static true
                        :private true)
         (fn [~(with-meta value {:tag typeclass})]
           (let [~(with-meta array {:tag aclass})
                 (Array/newInstance ~typeclass (int 1))]
             (aset ~array (int 0) ~value)
             ~array)
           )))))
  )
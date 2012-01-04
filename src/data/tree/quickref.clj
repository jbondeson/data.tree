(ns data.tree.quickref
  (:refer-clojure :exclude [set! ref deref]))

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

(defmacro def-ref
  [name type]
  (let [^Class typeclass (resolve-type type)
        ^Class aclass    (array-class typeclass)]
    `(def ~(vary-meta name assoc
                      :tag aclass
                      :arglists (list 'quote '([value]))
                      :static true)
       (fn [value#] (into-array ~typeclass [value#])))))

(defmacro def-deref
  [name type]
  (let [^Class typeclass (resolve-type type)
        ^Class aclass    (array-class typeclass)]
    (let [qref (gensym)
          qget (gensym)]
      `(let [~qget (vary-meta aget assoc :tag ~typeclass)]
         (def ~(vary-meta name assoc
                          :tag typeclass
                          :arglists (list 'quote '([ref]))
                          :static true)
           (fn [~(with-meta qref {:tag aclass})]
             (~qget ~qref (int 0))))))))

(defmacro def-set
  [name type]
  (let [^Class typeclass (resolve-type type)
        ^Class aclass    (array-class typeclass)]
    (let [qref (gensym)
          value (gensym)]
      `(def ~(vary-meta name assoc
                        :tag aclass
                        :arglists (list 'quote '([ref value]))
                        :static true)
         (fn [~(with-meta qref {:tag aclass})
             ~(with-meta value {:tag typeclass})]
           (aset ~qref (int 0) ~value))))))

(comment
  (defn do-stuff2 ^"[Ldata.tree.bst.INode;"
    [array access-times set-times]
    (do
      (dotimes [i access-times]
        (aget ^"[Ldata.tree.bst.INode;"
              array (int 0))
        (dotimes [j set-times]
          (aset ^"[Ldata.tree.bst.INode;" array (int 0) nil)))
      nil))

  (defn do-stuff
    [creation-times access-times set-times]
    (do
      (dotimes [i creation-times]
        (buh-ref nil))
      (let [iref (buh-ref (LeafNode. 1))]
        (dotimes [j access-times]
          (buh-deref iref))
        (dotimes [k set-times]
          (buh-set! iref nil)))
      nil))
  )
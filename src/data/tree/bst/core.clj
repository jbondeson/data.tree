(ns ^{:doc "Persistent Binary Search Tree Core"
      :author "Jeremy Bondeson"}
  data.tree.bst.core
  (:refer-clojure :exclude [comparator comp])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:use [data.tree.printable])
  (:require [data.compare :as cmp])
  (:require [data.util.tref :as tref])
  (:import (data.util EditContext))
  )

(definterface INode
  (^data.tree.bst.core.INode insert [item ^java.util.Comparator comp])
  (^data.tree.bst.core.INode delete [item ^java.util.Comparator comp])
  (^data.tree.bst.core.INode doInsert [^data.util.EditContext edit item ^java.util.Comparator comp])
  (^data.tree.bst.core.INode doDelete [^data.util.EditContext edit item ^java.util.Comparator comp])
  (^data.tree.bst.core.INode doInsertCopy [^data.util.EditContext edit item ^java.util.Comparator comp])
  (^data.tree.bst.core.INode doDeleteCopy [^data.util.EditContext edit item ^java.util.Comparator comp])
  (retrieve [item ^java.util.Comparator comp])
  (value [])
  (left [])
  (right []))

(declare make-leaf-node make-lefty-node make-righty-node make-full-node
         make-trans-node)

(deftype LeafNode [x]
  INode
  (insert [this item comp]
    (let [leaf (make-leaf-node item)]
      (cmp/with-compare comp res item x
        (cond
         (= res 0)  (throw+ {:duplicate-key? true})
         (= res -1) (make-lefty-node x leaf)
         :else      (make-righty-node x leaf)))))
  (delete [this item comp]
    (cmp/with-compare comp res item x
      (if (= res 0)
        nil
        (throw+ {:not-found? true}))))
  (doInsert [this edit item comp] (.doInsertCopy this edit item comp))
  (doDelete [this edit item comp] (.doDeleteCopy this edit item comp))
  (doInsertCopy [this edit item comp]
    (let [leaf (make-trans-node edit item nil nil)]
      (cmp/with-compare comp res item x
        (cond
         (= res 0)  (throw+ {:duplicate-key? true})
         (= res -1) (make-trans-node edit x leaf nil)
         :else      (make-trans-node edit x nil leaf)))))
  (doDeleteCopy [this edit item comp]
    (cmp/with-compare comp res item x
      (if (= res 0)
        (throw+ {:not-found? true})
        this)))
  (retrieve [this item comp]
    (cmp/with-compare comp res item x
      (if (= res 0)
        x
        nil)))
  (value [this] x)
  (left [this] nil)
  (right [this] nil))

(deftype LeftyNode [x ^INode l]
  INode
  (insert [this item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res 0)  (throw+ {:duplicate-key? true})
       (= res -1) (make-lefty-node x (.insert l item comp))
       :else      (make-full-node x l (make-leaf-node item)))))
  (delete [this item comp]
     (cmp/with-compare comp res item x
      (cond
       (= res 0)  l
       (= res -1) (let [node (.delete l item comp)]
                    (if node
                      (make-lefty-node x node)
                      (make-leaf-node x)))
       :else      (throw+ {:not-found? true}))))
  (doInsert [this edit item comp] (.doInsertCopy this edit item comp))
  (doDelete [this edit item comp] (.doDeleteCopy this edit item comp))
  (doInsertCopy [this edit item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res 0)  (throw+ {:duplicate-key? true})
       (= res -1) (make-trans-node edit x (.doInsertCopy l edit item comp) nil)
       :else      (make-trans-node edit x l (make-trans-node edit item nil nil)))))
  (doDeleteCopy [this edit item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res 0)  l
       (= res -1) (let [node (.doDeleteCopy l edit item comp)]
                    (if node
                      (make-trans-node edit x node nil)
                      (make-trans-node edit x nil nil)))
       :else      (throw+ {:not-found? true}))))
  (retrieve [this item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res 0)  x
       (= res -1) (.retrieve l item comp))))
  (value [_] x)
  (left [_] l)
  (right [_] nil))

(deftype RightyNode [x ^INode r]
  INode
  (insert [this item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (make-righty-node x (.insert r item comp))
       :else     (make-full-node x (make-leaf-node item) r))))
  (delete [this item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res 0) r
       (= res 1) (let [node (.delete r item comp)]
                   (if node
                     (make-righty-node x node)
                     (make-leaf-node x)))
       :else      (throw+ {:not-found? true}))))
  (doInsert [this edit item comp] (.doInsertCopy this edit item comp))
  (doDelete [this edit item comp] (.doDeleteCopy this edit item comp))
  (doInsertCopy [this edit item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (make-trans-node edit x nil (.doInsertCopy r edit item comp))
       :else     (make-trans-node edit x (make-trans-node edit item nil nil) r))))
  (doDeleteCopy [this edit item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res 0) r
       (= res 1) (let [node (.doDeleteCopy r edit item comp)]
                   (if node
                     (make-trans-node edit x nil node)
                     (make-trans-node edit x nil nil)))
       :else      (throw+ {:not-found? true})))
    )
  (retrieve [this item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res 0)  x
       (= res 1) (.retrieve r item comp))))
  (value [_] x)
  (left [_] nil)
  (right [_] r))

(deftype FullNode [x ^INode l ^INode r]
  INode
  (insert [this item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (make-full-node x l (.insert r item comp))
       :else     (make-full-node x (.insert l item comp) r))))
  (delete [this item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res  1) (let [rnode (.delete r item comp)]
                    (if rnode
                      (make-full-node x l rnode)
                      (make-lefty-node x l)))
       (= res -1) (let [lnode (.delete l item comp)]
                    (if lnode
                      (make-full-node x lnode r)
                      (make-righty-node x r)))
       :else      (let [^INode successor (loop [^INode node r]
                                           (let [smaller (.left node)]
                                             (if (nil? smaller)
                                               node
                                               (recur smaller))))
                        val (.value successor)]
                    (if (identical? r successor)
                      (if (nil? (.right r))
                        (make-lefty-node val l)
                        (make-full-node val l (.right r)))
                      (make-full-node val l (.delete r val comp)))))))
  (doInsert [this edit item comp] (.doInsertCopy this edit item comp))
  (doDelete [this edit item comp] (.doDeleteCopy this edit item comp))
  (doInsertCopy [this edit item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res 0) (throw+ {:duplicate-key? true})
       (= res 1) (make-trans-node edit x l (.doInsertCopy r edit item comp))
       :else     (make-trans-node edit x (.doInsertCopy l edit item comp) r))))
  (doDeleteCopy [this edit item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res  1) (let [rnode (.doDeleteCopy r edit item comp)]
                    (if rnode
                      (make-trans-node edit x l rnode)
                      (make-trans-node edit x l nil)))
       (= res -1) (let [lnode (.doDeleteCopy l edit item comp)]
                    (if lnode
                      (make-trans-node edit x lnode r)
                      (make-trans-node edit x nil r)))
       :else      (let [^INode successor (loop [^INode node r]
                                           (let [smaller (.left node)]
                                             (if (nil? smaller)
                                               node
                                               (recur smaller))))
                        val (.value successor)]
                    (if (identical? r successor)
                      (if (nil? (.right r))
                        (make-trans-node edit val l nil)
                        (make-trans-node edit val l (.right r)))
                      (make-trans-node edit val l (.doDeleteCopy r edit val comp)))))))
  (retrieve [this item comp]
    (cmp/with-compare comp res item x
      (cond
       (= res 0)  x
       (= res 1) (.retrieve r item comp)
       :else     (.retrieve l item comp))))
  (value [_] x)
  (left [_] l)
  (right [_] r))

(deftype TransNode [x l r]
  INode
  (insert [this item comp]
    (let [^INode lnode @l
          ^INode rnode @r]
      (cmp/with-compare comp res item x
        (cond
         (= res 0) (throw+ {:duplicate-key? true})
         (= res 1) (cond
                    (and lnode rnode) (make-full-node x lnode (.insert rnode item comp))
                    rnode             (make-righty-node x (.insert rnode item comp))
                    lnode             (make-full-node x lnode (make-leaf-node item))
                    :else             (make-righty-node x (make-leaf-node item)))
         :else     (cond
                    (and lnode rnode) (make-full-node x (.insert lnode item comp) rnode)
                    lnode             (make-lefty-node x (.insert lnode item comp))
                    rnode             (make-full-node x (make-leaf-node item) rnode)
                    :else             (make-lefty-node x (make-leaf-node item)))))))

  (delete [this item comp]
    (let [^INode lnode @l
          ^INode rnode @r]
      (cmp/with-compare comp res item x
        (cond
         (= res -1) (if lnode
                      (let [node (.delete lnode item comp)]
                        (cond
                         (and node rnode) (make-full-node x node rnode)
                         node             (make-lefty-node x node)
                         rnode            (make-righty-node x rnode)
                         :else            (make-leaf-node x)))
                      (throw+ {:not-found? true}))
         (= res  1) (if rnode
                      (let [node (.delete rnode item comp)]
                        (cond
                         (and node lnode) (make-full-node x lnode node)
                         node             (make-righty-node x node)
                         lnode            (make-lefty-node x lnode)
                         :else            (make-leaf-node x)))
                      (throw+ {:not-found? true}))
         :else      (if (and lnode rnode)
                      (let [^INode successor (loop [^INode node r]
                                               (let [smaller (.left node)]
                                                 (if smaller
                                                   (recur smaller)
                                                   node)))
                            val (.value successor)]
                        (if (identical? rnode successor)
                          (if (.right rnode)
                            (make-full-node val lnode (.right rnode))
                            (make-lefty-node val lnode))
                          (make-full-node val lnode (.delete rnode val comp))))
                      (or lnode rnode))))))
  (doInsert [this edit item comp]
    (cmp/with-compare comp res item x
      (let [^INode rnode @r
            ^INode lnode @l]
        (cond
         (= res 0) (throw+ {:duplicate-key? true})
         (= res 1) (do
                     (if rnode
                       (tref/set! r (.doInsert rnode edit item comp))
                       (tref/set! r (make-trans-node edit item nil nil)))
                     this)
         :else     (do
                     (if lnode
                       (tref/set! l  (.doInsert lnode edit item comp))
                       (tref/set! l (make-trans-node edit item nil nil)))
                     this)))))
  (doDelete [this edit item comp]
    (cmp/with-compare comp res item x
      (let [^INode rnode @r
            ^INode lnode @l]
        (cond
         (= res  1) (do
                      (when rnode
                        (tref/set! r (.doDelete rnode edit item comp)))
                      (throw+ {:not-found? true}))
         (= res -1) (do
                      (when lnode
                        (tref/set! l (.doDelete lnode edit item comp)))
                      (throw+ {:not-found? true}))
         :else      (if (and lnode rnode)
                      (let [^INode successor (loop [^INode node rnode]
                                               (let [smaller (.left node)]
                                                 (if smaller
                                                   (recur smaller)
                                                   node)))
                            val (.value successor)]
                        (if (identical? rnode successor)
                          (make-trans-node edit val lnode (.right rnode))
                          (make-trans-node edit val lnode (.doDelete rnode edit val comp))))
                      (or lnode rnode))))))
  (doInsertCopy [this edit item comp]
    (cmp/with-compare comp res item x
      (let [^INode rnode @r
            ^INode lnode @l]
        (cond
         (= res 0) (throw+ {:duplicate-key? true})
         (= res 1) (if rnode
                     (make-trans-node edit val lnode (.doInsertCopy rnode edit item comp))
                     (make-trans-node edit val lnode (make-trans-node edit item)))
                     
         :else     (if lnode
                     (make-trans-node edit val (.doInsertCopy lnode edit item comp) rnode)
                     (make-trans-node edit val (make-trans-node edit item) rnode))))))
  
  (doDeleteCopy [this edit item comp]
    (cmp/with-compare comp res item x
      (let [^INode rnode @r
            ^INode lnode @l]
        (cond
         (= res  1) (if rnode
                      (make-trans-node edit val lnode (.doDeleteCopy rnode edit item comp))
                      (throw+ {:not-found? true}))
                      
         (= res -1) (if lnode
                      (make-trans-node edit val (.doDeleteCopy lnode edit item comp) rnode)
                      (throw+ {:not-found? true}))
                      
         :else      (if (and lnode rnode)
                      (let [^INode successor (loop [^INode node rnode]
                                               (let [smaller (.left node)]
                                                 (if smaller
                                                   (recur smaller)
                                                   node)))
                            val (.value successor)]
                        (if (identical? rnode successor)
                          (make-trans-node edit val lnode (.right rnode))
                          (make-trans-node edit val lnode (.doDeleteCopy rnode edit val comp))))
                      (or lnode rnode))))))
  
  (retrieve [this item comp]
    (let [^INode lnode @l
          ^INode rnode @r]
      (cmp/with-compare comp res item x
        (cond
         (= res 0)  x
         (and (= res -1) lnode) (.retrieve lnode item comp)
         (and (= res  1) rnode) (.retrieve rnode item comp)))))
  (value [_] x)
  (left [_] @l)
  (right [_] @r))

(defn make-leaf-node ^INode
  [item]
  (LeafNode. item))

(defn make-lefty-node ^INode
  [item ^INode left]
  (LeftyNode. item left))

(defn make-righty-node ^INode
  [item ^INode right]
  (RightyNode. item right))

(defn make-full-node ^INode
  [item ^INode left ^INode right]
  (FullNode. item left right))

(defn make-trans-node ^INode
  ^{:inline (fn [e i l r] `(TransNode. i
                                      (tref/thread-bound-ref l e)
                                      (tref/thread-bound-ref r e)))
    :inline-arities #{4}}
  [^EditContext edit item ^INode left ^INode right]
  (TransNode. item (tref/thread-bound-ref left edit) (tref/thread-bound-ref right edit)))

;;-- Printable Tree
(extend-protocol PrintableTree
  LeafNode   (print-tree [x] (prtree "LeafNode" (.value x)))
  LeftyNode  (print-tree [x] (prtree "LeftyNode" (.value x) (.left x)))
  RightyNode (print-tree [x] (prtree "RightyNode" (.value x) (.right x)))
  FullNode   (print-tree [x] (prtree "FullNode" (.value x) (.left x) (.right x)))
  TransNode  (print-tree [x] (prtree "TransNode" (.value x) (.left x) (.right x)))

  )
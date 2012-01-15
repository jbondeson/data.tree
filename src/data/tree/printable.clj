(ns ^{:doc "Printable Tree Protocol"
      :author "Jeremy Bondeson"}
  data.tree.printable)

(defprotocol PrintableTree
  (print-tree [tree]))

(defn
  prtree
  "Print a tree to *out*"
  [t & xs]
  (print "<")
  (print t)
  (doseq [x xs]
    (print " ")
    (print-tree x))
  (print ">"))

(extend-protocol PrintableTree
  Object (print-tree [x] (prtree x))
  nil    (print-tree [x] (print "nil")))
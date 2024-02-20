(defpackage #:tinymath-core
  (:use #:cl)
  (:export #:constant
           #:ooperator
           #:binary-op
           #:unary-op
           #:sym
           #:btree
           #:wire!))

(defpackage #:tinymath-view
  (:use #:cl)
  (:export #:view-tree))

(defpackage #:tinymath
  (:use #:cl #:tinymath-core
             #:tinymath-view))
(in-package #:tinymath)

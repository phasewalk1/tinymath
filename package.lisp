(defpackage #:tinymath-core
  (:use #:cl)
  (:export #:constant
           #:make-constant
           #:constant-value
           #:operator
           #:make-operator
           #:operator-op
           #:operator-precedence
           #:operator-is-left-assoc
           #:binary-op
           #:make-binary-op
           #:binary-op-operator
           #:binary-op-left
           #:binary-op-right
           #:unary-op
           #:make-unary-op
           #:unary-op-operator
           #:unary-op-operand
           #:sym
           #:sym-name
           #:tinytree
           #:wire!
           #:make-operation
           #:simplify))

(defpackage #:tinymath-view
  (:use #:cl)
  (:export #:view-tree))

(defpackage #:tinymath
  (:use #:cl #:tinymath-core
             #:tinymath-view))
(in-package #:tinymath)

(defpackage #:tinymath-view
  (:use #:cl)
  (:import-from #:tinymath-core #:btree-data 
                                #:btree-p
                                #:btree-left
                                #:btree-right
                                #:constant-p
                                #:constant-value
                                #:sym-p
                                #:sym-name
                                #:operator-p
                                #:operator-name
                                #:binary-op-p
                                #:binary-op-left
                                #:binary-op-right
                                #:unary-op-p
                                #:unary-op-operand)
  (:export #:view-tree))
(in-package #:tinymath-view)

(defun view-constant (k)
  (format t "~a" (constant-value k)))

(defun view-sym (s)
  (sym-name s))


(defpackage #:tinymath-simplify
  (:use #:cl #:tinymath-core))
(in-package #:tinymath-simplify)

(defmacro rule! (pattern result)
  `(push (cons ,pattern ,result) *simplification-rules*))

(defvar *simplification-rules* nil)

(rule! ((* x 1) | (* 1 x)) x)
(rule! ((* x 0) | (+ 0 x)) x)

(defun apply-rule (tree rule))

(defun simplify (tree)
  (dolist (rule *simplification-rules* tree)
    (let ((result (apply-rule tree rule)))
      (when result
        (return (simplify result))))))

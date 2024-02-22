(defpackage #:tinymath-core
  (:use :cl))
(in-package #:tinymath-core)

;; We use binary trees to represent algebraic expressions in such a way
;; that closely models the underlying data structure Lisp uses for lists.
;; 
;; In our model, the nodes in the tree can be of the following types:
;;   - constant:  a numeric value
;;   - sym:       a symbol, can be variable or point to a function (e.g., 'x' or 'sin')
;;   - operator:  builtin operator (e.g., '+', '*', etc.)

;; --------- Binary Operators ------------------------------------------------------
;;
;; The following is an example of a computation tree for the expression 
;;   2x + 3   
;;      or   
;; (+ (* 2 x) 3)
;;
;;       +                +              
;;      / \              / \
;;     *   3    ---->   2x  3    ---->    2x + 3
;;    / \
;;   2   x
;;
;; We call operations such as addition, multiplication, division, etc., binary operations
;; because they act on two inputs (left and right). 
;;     
;; --------- Unary Operators -------------------------------------------------------
;;
;; To include unary operations like trigonometric functions or logarithms within our binary tree structure,
;; we adapt the representation to accommodate these operations. Unary operations inherently require only one operand,
;; and we represent them in our binary tree by assigning the operand to one branch (typically the left for consistency),
;; and using the other branch (right) to hold a placeholder or `nil`, indicating the absence of a second operand.
;;
;; For example, the expression:
;;     sin(x) + 3
;;         or 
;;     (+ (sin x) 3)
;;
;; Can be represented as:
;;
;;       +                
;;      / \              
;;    sin  3     
;;    /
;;   x   
;;
;; Here, the `sin` node represents the unary operation, with `x` as its operand (left child), and an implicit `nil` on 
;; the right to maintain binary structure. This approach allows us to uniformly handle both unary and binary operations 
;; within the same binary tree structure, facilitating parsing, manipulation, and evaluation of algebraic expressions.

;; Constant values
(defclass constant ()
  ((value :initarg :value :accessor value-acc)))

;; Symbols are either variable or point to some function (or operator)
(defclass sym ()
  ((name :initarg :name :accessor sym-name)
   (value :initarg :value :initform nil :accessor sym-value)))

;; Computation tree
(defclass tinytree ()
  ((root :initarg :root :accessor root)
   (left :initarg :left :accessor left)
   (right :initarg :right :accessor right)))

;; Wire a computation tree from an arbitrary lisp expression
(defun wire! (expr)
  (cond
    ((numberp expr) (make-instance 'constant :value expr))
    ((quoted-sym? expr) (make-instance 'sym :name (cadr expr)))
    ((symbolp expr) (make-instance 'sym :name expr))
    ((listp expr)
     (let* ((op (car expr))
            (args (cdr expr))
            (left (first args))
            (right (second args)))
       (make-instance 'tinytree
                      :root op
                      :left (and left (wire! left))
                      :right (and right (wire! right)))))
    (t (error "Unsupported expression"))))


;; (wire! '(+ (* 2 'x) 3))
;; (wire! '(+ (sin 'x) 3))

(defun make-operation (op &rest args)
  (reduce (lambda (a b) (make-instance 'tinytree :root op :left a :right b)) args))

(defun simplify (tree)
  (cond
    ((typep tree 'constant) tree)
    ((typep tree 'sym) tree)
    ((typep tree 'tinytree)
     (let ((left-simplified (simplify (left tree)))
           (right-simplified (simplify (right tree))))
       (cond
         ((and (eq (root tree) '*)
               (typep left-simplified 'constant)
               (= (value-acc left-simplified) 1)) 
          right-simplified)
         ((and (eq (root tree) '+)
               (or (and (typep left-simplified 'constant) (= (value-acc left-simplified) 0))
                   (and (typep right-simplified 'constant) (= (value-acc right-simplified) 0))))
          (if (and (typep left-simplified 'constant) (= (value-acc left-simplified) 0))
            right-simplified
            left-simplified))
         (t (make-instance 'tinytree :root (root tree) :left left-simplified :right right-simplified)))))
    (t tree)))

(defun quoted-sym? (expr)
  (and (listp expr) (eq (car expr) 'quote)))

(defun precedence (op)
  "Returns the precedence level for the given operator."
  (case op
    ;; Higher precedence
    ('exp 10)   ; Exponentiation
    ('sin 9)    ; Trigonometric functions, etc.
    ('cos 9)
    ('tan 9)
    ('log 9)    ; Logarithmic
    ('* 7)      ; Multiplication
    ('/ 7)      ; Division
    ('+ 5)      ; Addition
    ('- 5)      ; Subtraction
    ;; Lower precedence
    ('= 3)      ; Equality
    ('and 2)    ; Logical AND
    ('or 1)     ; Logical OR
    ;; Default case if the operator is not recognized
    (t 0)))

(defun is-left-assoc? (op)
  (not (eq op 'exp)))

(defun print-tree (node &optional (level 0))
  "Prints the tree structure starting from NODE at the given LEVEL."
  (when node
    (let ((indent (make-string (* 2 level) :initial-element #\Space)))
      (cond
       ;; If the node is a constant, print its value
       ((typep node 'constant)
        (format t "~&~A[Constant: ~A]" indent (value-acc node)))
       ;; If the node is a symbol, print its name
       ((typep node 'sym)
        (format t "~&~A[Symbol: ~A]" indent (name node)))
       ;; If the node is the root of a subtree (an operator or function)
       ((typep node 'tinytree)
        (format t "~&~A[Operator: ~A]" indent (root node))
        ;; Recursively print the left and right subtrees
        (print-tree (left node) (1+ level))
        (print-tree (right node) (1+ level)))
       (t
        (format t "~&~A[Unknown node type]" indent))))))



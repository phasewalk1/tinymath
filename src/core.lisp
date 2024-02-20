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

(defstruct constant
  value)

(defstruct operator
  name
  precedence
  is-left-assoc)

(defstruct binary-op
  operator
  left
  right)

(defstruct unary-op
  operator
  operand)

(defstruct sym
  name)

(defstruct tree
  (data nil)
  (left nil)
  (right nil))

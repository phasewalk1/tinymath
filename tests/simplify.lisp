(ql:quickload "tinymath")

;; x * 1
(defparameter *1x* 
  (tinymath-core:make-operation 
    '* (make-instance 'tinymath-core:constant :value 1)
       (make-instance 'tinymath-core:sym :name 'x)))

;; call simplify on the tree and store it
(defparameter *1x-simpl*
  (tinymath-core:simplify *1x*))

(defun test-mult-by-1 ()
  (typecase *1x-simpl*
    ;; x * 1 simplifies to x
    (tinymath-core:sym (eq (tinymath-core:sym-name *1x-simpl*) 'x))
    (t nil)))

(let ((result (test-mult-by-1)))
  (cond
    ((equal result t) (format t "<simplify> test-mult-by-1 passed~%"))
    ((equal result nil) (format t "<simplify> test-mult-by-1 failed!~%"))))

;; x + 0
(defparameter *x+0*
  (tinymath-core:make-operation 
    '+ (make-instance 'tinymath-core:sym :name 'x)
       (make-instance 'tinymath-core:constant :value 0)))

(defparameter *x+0-simpl*
  (tinymath-core:simplify *x+0*))

(defun test-add-0 ()
  (typecase *x+0-simpl*
    (tinymath-core:sym (eq (tinymath-core:sym-name *x+0-simpl*) 'x))
    (t nil)))

(let ((result (test-add-0)))
  (cond
    ((equal result t) (format t "<simplify> test-add-by-0 passed~%"))
    ((equal result nil) (format t "<simplify> test-add-by-0 failed!~%"))))

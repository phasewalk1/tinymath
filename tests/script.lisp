(ql:quickload "tinymath")

(tinymath-view:view-tree 
  (tinymath-core:wire! '(+ (* x 2) (* 3 y))))

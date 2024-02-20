(asdf:defsystem #:tinymath
  :serial t
  :depends-on (#:serapeum #:optima)
  :components ((:file "package")               ; Defines all packages
               (:module "src"
                        :serial t
                        :components ((:file "core")
                                     (:file "view"))))
  :description "A tiny symbolic mathematics engine"
  :author "phasewalk1"
  :licence "MIT"
  :version "0.1.0")


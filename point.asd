(in-package :asdf-user)
(defsystem "point"
  :description "Generic protocol to deal with 2d and 3d points in various formats."
  :version "0.0.1"
  :licence "LGPL"
  :author "Johannes Martinez Calzada"
  :components ((:file "package")
               (:file "macros")
               (:file "struct")
               (:file "protocol-required")
               (:file "protocol-optional")
               (:file "utility")
               (:file "documentation")))

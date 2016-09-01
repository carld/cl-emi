;;;; cl-emi.asd

(asdf:defsystem :cl-emi
  :description "Electricity Market Information API Library for Common Lisp"
  :author "Carl Douglas <carl.douglas@gmail.com>"
  :license "MIT"
  :depends-on (:drakma
               :yason)
  :serial t
  :components ((:file "package")
               (:file "cl-emi")))


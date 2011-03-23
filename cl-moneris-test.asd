;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem :cl-moneris-test
  :name "cl-moneris-test"
  :description "Tests for the cl-moneris library."
  :license "Public Domain"
  :components
  ((:module :test
            :serial t
            :components ((:file "package")
                         (:file "moneris-test"))))
  :depends-on (:cl-moneris :eos))

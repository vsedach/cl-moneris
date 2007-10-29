;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :moneris
  :serial t
  :version "0.0"
  :components ((:file "packages")
	       (:file "moneris"))
  :depends-on (:s-xml
	       :cl-ppcre
	       :drakma))

(asdf:defsystem :moneris-test
  :serial t
  :version "0.0"
  :components ((:file "packages")
	       (:file "moneris-test"))
  :depends-on (:moneris))

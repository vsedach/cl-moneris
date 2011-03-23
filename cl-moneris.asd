;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem :cl-moneris
  :name "cl-moneris"
  :description "An interface to the Moneris payment processing service (HTTP)."
  :maintainer "Vladimir Sedach <vsedach@gmail.com>"
  :license "ISC"
  :serial t
  :components ((:file "package")
               (:file "response-codes")
	       (:file "moneris"))
  :depends-on (:s-xml :drakma))

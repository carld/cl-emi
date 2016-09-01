;;;; package.lisp

(defpackage :cl-emi
  (:use :cl :drakma :yason)
  (:export *key*
	   :icp/search
	   :icp/single
	   :icp/list
	   :rtp/get
	   :emit-filter
	   ))



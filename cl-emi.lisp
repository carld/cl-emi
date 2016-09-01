;;;; cl-emi.lisp

(in-package #:cl-emi)

;;(setf drakma:*header-stream* *standard-output*)

;(eval-when (:compile-toplevel :execute :load-toplevel)
(defparameter *key* nil)

(defun make-keyword (name)
  "Intern `name' into the keyword package"
  (values (intern (string-upcase name) "KEYWORD")))

(defun stringify-param (var)
  "Writes `var' as a string unless it already is a string"
  (unless (stringp var) (write-to-string var)))

(defun *default-headers* ()
  "Returns an association list of headers"
  `(("Ocp-Apim-Subscription-Key" . ,*key*)))

(defun api-request (uri &optional params headers (method :GET))
  "make an HTTP request to `uri' with the supplied `params'"
  (format t "~a" params)
  (let ((stream
	 (drakma:http-request
	  uri
	  :want-stream t
	  :method method
	  :additional-headers headers
	  :parameters params )))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (yason:parse stream :object-as :plist
		 :object-key-fn #'make-keyword
		 )))

(defmacro defapi (path params)
  `(api-request
    ,(format nil "https://emi.azure-api.net/~a" path)
    ,params
    (*default-headers*)))

; https://emi.azure-api.net/ICPConnectionData/search/[?unitOrNumber][&streetOrPropertyName][&suburbOrTown][&region]

(defun icp/search (&optional unit-or-number street-or-property-name suburb-or-town region )
  (defapi "ICPConnectionData/search" (remove-if (lambda (x) (null (cdr x)))
			      `(("unitOrNumber" . ,unit-or-number)
				("streetOrPropertyName" . ,street-or-property-name)
				("suburbOrTown" . ,suburb-or-town)
				("region" . ,region)))))

;; https://emi.azure-api.net/ICPConnectionData/single/?id={ICP}

;; https://emi.azure-api.net/ICPConnectionData/list/?ids={list_of_ICPs}

(defun icp/single (icp)
  (defapi "ICPConnectionData/single" `(("id" . ,icp))))

(defun icp/list (icp-list)
  (defapi "ICPConnectionData/list" `(("ids" . ,(format nil "~{~a~^,~}" icp-list)))))

;; ICP Status
;;Code that represents the energisation and connection status of the ICP
;;999—new; 000—ready; 001—inactive; 002—active; or 003—decommissioned.

;; Real Time Prices
;; https://emi.azure-api.net/rtp/[?$filter]
;; $filter=interval_datetime gt datetime'2015-04-01T01:00' and interval_datetime lt datetime'2015-04-30T01:00'
;; https://emi.azure-api.net/rtp/

;; interval_datetime gt datetime'2015-04-01T01:00' and interval_datetime lt datetime'2015-04-30T01:00'
(defun rtp/get (odata-filter)
  "Retrieve Real Time Prices"
  (defapi "rtp/" `(("$filter" . ,odata-filter))))

(defun rtp/post (name callback-url)
  "Register a url for Real Time Price updates"
  (defapi "rtp/" `(("name" . ,name)
		   ("url"  . ,callback-url))))

(defun rtp/delete (callback-url)
  "Unregister a url from Real Time Price updates"
  (defapi "rtp/" callback-url))

;; Real Time Dispatch
;; https://emi.azure-api.net/rtd/[?$filter]


;; ODATA filter interpreter
; (and (gt interval_datetime (datetime '2015-04-01T01:00')) (lt interval_datetime (datetime '2015-04-30T01:00')))
; interval_datetime gt datetime'2015-04-01T01:00' and interval_datetime lt datetime'2015-04-30T01:00'
;"interval_datetime gt datetime 2015-04-01T01:00 and interval_datetime lt datetime 2015-04-30T01:00"
;interval_datetime gt datetime'2015-04-01T01:00' and interval_datetime lt datetime'2015-04-30T01:00'
(defun emit-filter (exp)
  ;(print exp *standard-output*)
  (cond
    ((stringp exp) exp)
    ((symbolp exp) (string-downcase (string exp)))
    ((funcp (first exp)) (format nil "~a'~a'" (emit-filter (first exp)) (second exp)))
    ((operatorp (first exp)) (format nil "~{~A~^ ~}" (list (emit-filter (second exp)) (emit-filter (first exp)) (emit-filter (third exp)))))
    ((consp exp) (format nil "~{~A~^ ~}" (mapcar #'emit-filter exp)))
    (t (error "bad expression: ~a" exp))))

(defun funcp (sym)
  (member sym '(:datetime)))

(defun operatorp (sym)
  (member sym '(:and :gt :lt)))


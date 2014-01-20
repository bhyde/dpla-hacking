(in-package #:cl-dpla)

(defparameter *api-key* nil)
(defparameter *api-user-email* nil)
(defvar *api-url-template* nil)
(defvar *last-query*)
(defvar *last-query-result*)

(defun get-or-load-api-info ()
  (cond
    (*api-key* 
     ; It's all good.
     )
    ((probe-file "api-key.lisp")
     (load "api-key.lisp")
     (assert (stringp *api-key*))
     (assert (stringp *api-user-email*))
     (format t "~&API key set for ~S" *api-user-email*)
     (setf *api-url-template*
           (format nil "http://api.dp.la/v2/~~a?api_key=~a~~@{~~^&~~a=~~a~~}" *api-key*)))
    (t
     (error "Need and api key, see api-key-example.lisp.")))
  nil)


(defun request (type &rest parameters)
  (get-or-load-api-info)
  (setf *last-query* (list* type parameters))
  (setf *last-query-result*
        (cl-json:decode-json-from-string
         (coerce
          (loop
             with request-url = (apply #'format nil *api-url-template* *last-query*)
             with raw-result = (drakma:http-request request-url)
             for i across raw-result
             collect (code-char i)) 
          'string))))

(in-package #:cl-dpla)

(defparameter *api-key* nil)
(defparameter *api-user-email* nil)
(defvar *api-url-template* nil)
(defvar *last-query*)
(defvar *last-query-url*)
(defvar *last-query-result*)

(defun assure-api-info-loaded ()
  (cond
    (*api-key*
     ; It's all good.
     )
    ((asdf:system-relative-pathname "cl-dpla" "api-key.lisp")
     (let ((*package* #.*package*))
       (load (asdf:system-relative-pathname "cl-dpla" "api-key.lisp")))
     (assert (stringp *api-key*))
     (assert (stringp *api-user-email*))
     ; (format t "~&API key set for ~S" *api-user-email*)
     (setf *api-url-template*
           (format nil "http://api.dp.la/v2/~~{~~a~~^/~~}?api_key=~a~~@{~~^&~~a=~~a~~}" *api-key*)))
    (t
     (error "Api-key.lisp not found, see api-key-example.lisp for guide.")))
  nil)

#+nil ;; not used yet.
(defparameter *known-query-parameters*
    '(:q :api-key
      :page :page-size
      :id :@id :source-resource.id :source-resource.contributor 
      :source-resource.date.begin :source-resource.date.end 
      :source-resource.extent :source-resource.language.name
      :source-resource.language.iso-639 :source-resource.format
      :source-resource.state-located-in.name
      :source-resource.state-located-in.iso-3166-2 :source-resource.spatial.name
      :source-resource.spatial.country :source-resource.spatial.region
      :source-resource.spatial.county :source-resource.spatial.state
      :source-resource.spatial.city :source-resource.spatial.iso-3166-2
      :source-resource.spatial.coordinates :source-resource.subject.@id
      :source-resource.subject.type :source-resource.subject.name
      :source-resource.temporal.begin :source-resource.temporal.end
      :source-resource.title :source-resource.type
      :has-view.@id :has-view.format :is-part-of.@id :is-part-of.name
      :is-shown-at :object :provider.@id :provider.name :*facetable :*fields
      :source-resource.contributor :source-resource.date.begin 
      :source-resource.date.end :source-resource.language.name
      :source-resource.language.iso-639 :source-resource.format
      :source-resource.state-located-in.name :source-resource.state-located-in.iso-3166-2
      :source-resource.spatial.name :source-resource.spatial.country
      :source-resource.spatial.region :source-resource.spatial.county
      :source-resource.spatial.state :source-resource.spatial.city
      :source-resource.spatial.iso-3166-2 :source-resource.spatial.coordinates
      :source-resource.subject.@id :source-resource.subject.name
      :source-resource.temporal.begin :source-resource.temporal.end
      :source-resource.type :has-view.@id :has-view.format :is-part-of.@id
      :is-part-of.name :is-shown-at :object :provider.@id :provider.name))

(defun to-query-arg (arg)
  (drakma:url-encode
   (etypecase arg
     (fixnum (format nil "~D" arg))
     (symbol (json:lisp-to-camel-case (symbol-name arg)))
     (string arg))
   :ascii))

(defun request (object-id &rest parameters)
  (assure-api-info-loaded)
  (setf *last-query* (list* (if (listp object-id) object-id (list object-id)) parameters))
  (setf *last-query-result*
        (cl-json:decode-json-from-string
         (coerce
          (loop
             with query-as-strings = (mapcar #'to-query-arg (rest *last-query*))
             with request-url = (setf *last-query-url* 
                                      (apply #'format nil *api-url-template* (first *last-query*) query-as-strings))
             with raw-result = (drakma:http-request request-url)
             for i across raw-result
             collect (code-char i)) 
          'string))))

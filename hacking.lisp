(in-package #:common-lisp-user)

(defpackage #:dpla-hacking
  (:use #:common-lisp #:cl-dpla))

(in-package #:dpla-hacking)

(defun doc-properties ()
  (loop 
     with properties = () finally (return properties)
     for doc in (rest (assoc :docs *last-query-result*))
     do (loop for (property . nil) in doc
             do (pushnew property properties))))

(defun original-record-properties ()
  (loop 
     with properties = () finally (return properties)
     for doc in (rest (assoc :docs *last-query-result*))
     as original-record = (rest (assoc :original-record doc))
     do (loop for (property . nil) in original-record
             do (pushnew property properties))))

(defun print-doc-fields (&rest keys)
  (loop 
     for doc in (rest (assoc :docs *last-query-result*))
     do
       (loop initially (format t "~&")
            for key in keys
            do (format t " ~S" (rest (assoc key doc))))))

(defclass property-description ()
  ((name :reader name :initarg :name)
   (parent :reader parent :initarg :parent)
   (children :accessor children :initform nil)
   (type-guess :accessor type-guess :initform nil)))

(defmethod print-object ((x property-description) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream "~s" (name x))))

(defun hex-char-p (c)
  (digit-char-p c 16))

(defun looks-like-an-alist-p (x)
  (and (listp x)
       (loop 
          for e in x
          always (and (listp e)
                      (car e)
                      (keywordp (car e))))))

(defun guess-type-of-alist-element (alist-element)
  (typecase alist-element
    (integer
     'integer)
    (list
     (let ((list-of-lists? (every #'listp alist-element)))
     (cond
       ((null alist-element)
        'null)
       ((and list-of-lists?
             (looks-like-an-alist-p alist-element))
        'alist)
       (list-of-lists?
        (cond
          ((every #'(lambda (x) (eq 'alist (guess-type-of-alist-element x)))
                 alist-element)
           'list-of-alist)
          (t
           'list-of-lists)))
       (t 'list))))
    (string
     (cond
       ((string= "" alist-element)
        'empty-string)
       ((every #'digit-char-p alist-element)
        'decimal-string)
       ((every #'hex-char-p alist-element)
        'hex-string)
       (t 'string)))
    (t
     (let ((type (type-of alist-element)))
       (if (symbolp type) type (car type))))))

(defvar *depth* 0)

(defun map-over-property-description (pd function)
  (funcall function pd)
  (let ((*depth* (+ 1 *depth*)))
    (loop 
       for c in (children pd)
       do (map-over-property-description c function))))

(defparameter *root-of-item-property-tree*
    (make-instance 'property-description :parent nil :name :items))

(defparameter *root-of-collection-property-tree*
    (make-instance 'property-description :parent nil :name :collections))

(defun update-type-guess (property-description value)
  (let ((guess (guess-type-of-alist-element value)))
    (cond
      ((null (type-guess property-description))
       (setf (type-guess property-description) guess))
      ((eq guess (type-guess property-description))
       )
      ((listp (type-guess property-description))
       (pushnew guess (cdr (type-guess property-description))))
      (t
       (setf (type-guess property-description)
             `(or ,guess ,(type-guess property-description)))))))

(defun add-property-descriptions (property-tree-root alist)
  (loop 
     for (property-name . value) in alist
     as existing-description? 
       = (find property-name (children property-tree-root) :key #'name)
     unless existing-description? do
       (setf existing-description? (make-instance 'property-description
                                                      :parent property-tree-root
                                                      :name property-name))
       (push existing-description? (children property-tree-root))
     do
       (update-type-guess existing-description? value)
       (case (guess-type-of-alist-element value)
           (alist
            (add-property-descriptions existing-description? value))
           (list-of-alist
            ;; insert something here.
            (loop
               for x in value
               do (add-property-descriptions existing-description? x))))))

 (defun show-property-tree (property-tree-root)
   (labels ((show-path-to (x)
              (when (parent x)
                (show-path-to (parent x)))
              (format t "~s " (name x)))
            (show-node (x)
              (format t "~&")
              (show-path-to x)
              (format t " -- ~S" (type-guess x))))
     (map-over-property-description property-tree-root #'show-node)))
 

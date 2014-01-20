;; -*- common-lisp -*-

(defsystem "cl-dpla"
    :depends-on ("drakma" "cl-json")
    :components ((:file "packages")
                 (:file "api")))
    
              

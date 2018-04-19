;;;; http-client.asd

(asdf:defsystem #:myhttp
  :description "Describe http-client here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-ppcre
               #:drakma
               #:cl-html-parse
               #:hunchentoot)
  :serial t
  :components ((:file "package")
               (:file "http-client")
               (:file "http-server")
               ))


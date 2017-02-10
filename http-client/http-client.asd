;;;; http-client.asd

(asdf:defsystem #:http-client
  :description "Describe http-client here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-ppcre
               #:drakma
               #:cl-html-parse)
  :serial t
  :components ((:file "package")
               (:file "http-client")))


;;;; elf-parser.asd

(asdf:defsystem #:elf-parser
  :description "Describe elf-parser here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:metabang-bind
               #:com.gigamonkeys.binary-data
               #:elf)
  :serial t
  :components ((:file "package")
               (:file "dwarf")
               (:file "elf-parser")
               (:file "test")))


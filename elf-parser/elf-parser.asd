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
               (:file "shell")
               (:file "dwarf")
               (:file "elf-parser")
               (:file "test")
               (:file "regex-parser")
               (:file "keilc251")
               (:file "nm")
               (:file "printk")
               )
  )


;;;; package.lisp

(defpackage #:elf-parser
  (:use   #:cl
          #:com.gigamonkeys.binary-data
          #:alexandria)
  (:export :read-elf
           :show-debug-symbols
           :show-memory-layout
           :show-static-symbols))


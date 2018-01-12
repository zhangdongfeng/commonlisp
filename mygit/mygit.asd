#|
  This file is a part of mygit project.
|#

(in-package :cl-user)
(defpackage mygit-asd
  (:use :cl :asdf))
(in-package :mygit-asd)

(defsystem mygit
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:alexandria
               :flexi-streams
               :cl-fad
               :cl-ppcre
               :trivial-shell
               :drakma
               :yason)
  :components ((:module "src"
                        :components
                        ((:file "mygit")
                         (:file "shell")
                         (:file "regex")
                         (:file "manifest")
                         (:file "gerrit")
                         )))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op mygit-test))))

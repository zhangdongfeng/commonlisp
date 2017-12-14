#|
  This file is a part of git project.
|#

(in-package :cl-user)
(defpackage git-asd
  (:use :cl :asdf))
(in-package :git-asd)

(defsystem git
  :version "0.1"
  :author ""
  :license ""
  :depends-on ( :alexandria :babel :babel-streams :cl-ppcre :trivial-shell )
  :components ((:module "src"
                        :components
                        ((:file "git"))))
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
  :in-order-to ((test-op (test-op git-test))))

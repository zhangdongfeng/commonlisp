(in-package :cl-user)
(defpackage mygit-test
  (:use :cl
        :mygit
        :prove))
(in-package :mygit-test)

;; NOTE: To run this test file, execute `(asdf:test-system :mygit)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)

(in-package :cl-user)
(defpackage git-test
  (:use :cl
        :git
        :prove))
(in-package :git-test)

;; NOTE: To run this test file, execute `(asdf:test-system :git)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)

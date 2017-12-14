#|
  This file is a part of git project.
|#

(in-package :cl-user)
(defpackage git-test-asd
  (:use :cl :asdf))
(in-package :git-test-asd)

(defsystem git-test
  :author ""
  :license ""
  :depends-on (:git
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "git"))))
  :description "Test system for git"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

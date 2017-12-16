#|
  This file is a part of mygit project.
|#

(in-package :cl-user)
(defpackage mygit-test-asd
  (:use :cl :asdf))
(in-package :mygit-test-asd)

(defsystem mygit-test
  :author ""
  :license ""
  :depends-on (:mygit
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "mygit"))))
  :description "Test system for mygit"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

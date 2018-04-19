(in-package #:myhttp)

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))

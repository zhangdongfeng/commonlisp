;;;; http-client.lisp

(in-package #:http-client)




(defun remove-excess-whitespace (str)
  (cl-ppcre:regex-replace-all "\\s+" str " "))


(defun slurp-stream (stream)
  "Quickly slurps the stream's contents into an array with fill pointer."
  (declare (stream stream))
  (with-output-to-string (string)
    (let ((buffer (make-array 4096 :element-type 'character)))
      (loop for bytes = (read-sequence buffer stream)
         do (write-sequence buffer string :start 0 :end bytes)
         while (= bytes 4096)))))


(defun slurp-file (filespec &rest open-args)
  "Quickly slurps the stream's contents into an array with fill pointer."
  (with-open-stream (stream (apply #'open filespec open-args))
    (with-output-to-string (string)
      (let ((buffer (make-array 4096 :element-type 'character)))
        (loop for bytes = (read-sequence buffer stream)
           do (write-sequence buffer string :start 0 :end bytes)
           while (= bytes 4096))))))


(defun file-at-once (filespec &rest open-args)
  (with-open-stream (stream (apply #'open filespec
                                   open-args))
    (let* ((buffer
            (make-array (file-length stream)
                        :element-type
                        (stream-element-type stream)
                        :fill-pointer t))
           (position (read-sequence buffer stream)))
      (setf (fill-pointer buffer) position)
      buffer)))


(let ((cookie-jar (make-instance 'drakma:cookie-jar)))
  (drakma:http-request "http://www.phpsecurepages.com/test/test.php"
                       :method :post
                       :parameters '(("entered_login" . "test")
                                     ("entered_password" . "test"))
                       :cookie-jar cookie-jar)
  (drakma:http-request "http://www.phpsecurepages.com/test/test2.php"
                       :cookie-jar cookie-jar)
  (drakma:cookie-jar-cookies cookie-jar))


(setf drakma:*header-stream* *standard-output*)

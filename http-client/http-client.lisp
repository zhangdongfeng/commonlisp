;;;; http-client.lisp

(in-package #:http-client)


(use-package (list :drakma :plump))
;;; "http-client" goes here. Hacks and glory await!

(defun remove-newlines (str)
  (remove-if (lambda (ch) (or (eql ch #\return)
                         (eql ch #\linefeed))) str))

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
           ￼￼                        (position (read-sequence buffer stream)))
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


(drakma:http-request "http://www.w3school.com.cn/tiy/v.asp"
                     :method :post
                     :external-format-out :gb2312
                     :parameters `(("code" . ,(slurp-file #p "~/quicklisp/local-projects/http-client/tmp.html"))))



(setf drakma:*header-stream* *standard-output*)

(drakma:http-request "http://www.baidu.com"
                     :method :post
                     :external-format-out :utf8
                     :parameters '(("wd" . "东风")))

(drakma:http-request "http://www.w3school.com.cn/tiy/v.asp"
                     :method :post
                     :parameters '(("wd" . "dongfeng")))

(setf *h*  (plump:parse (drakma:http-request "http://www.w3school.com.cn/tiy/t.asp?f=html_form_submit")))
(mapc #'serialize (plump:get-elements-by-tag-name *h* "input"))


(plump:parse "<html>
<body>

<h1>My First Heading</h1>

<p>My first paragraph.</p>

</body>
</html>")

(plump:serialize *)


(defparameter *h* (plump:parse "<html>
<body>

<h1>My First Heading</h1>

<p>My first paragraph.</p>

</body>
</html>"))


(drakma:http-request "http://www.w3school.com.cn/tiy/v.asp"
                     :method :post
                     :content-type "multipart/form-data"
                     :external-format-out :gb2312
                     :close nil
                     :parameters `(("code" . ,(slurp-file #p "~/quicklisp/local-projects/http-client/tmp.html"))))


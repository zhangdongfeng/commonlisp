(in-package :mygit)

(define-condition api-error (error)
  ((http-status :initarg :http-status
                :reader error-http-status)
   (http-headers :initarg :http-headers
                 :reader error-http-headers)
   (response :initarg :response
             :reader error-response))
  (:report (lambda (c stream)
             (format stream "gerrit API error, HTTP status code ~A~%~A~@[~%~A~]"
                     (error-http-status c)
                     (error-http-headers c)
                     (error-response c)))))

(defun keyword-to-gerrit-keyword (keyword)
  (string-downcase (string keyword)))

(defun gerrit-keyword-to-keyword (string)
  (let ((*package* (find-package :keyword)))
    (read-from-string string)))

(defun plist-to-http-parameters (plist)
  (loop
     for (key value) on plist by #'cddr
     collect (cons (keyword-to-gerrit-keyword key) value)))

(defun plist-to-hash-table (plist)
  (loop
     with hash-table = (make-hash-table :test #'equal)
     for (key value) on plist by #'cddr
     do (setf (gethash (keyword-to-gerrit-keyword key) hash-table) value)
     finally (return hash-table)))

#+ (or)
(defun api-command (url &key body (method :get) (username *username*) (password *password*) parameters)
  (multiple-value-bind
        (body status-code headers)
      (drakma:http-request (format nil (concatenate 'string *host* "~A" )  url )
                           :method method
                           :parameters (plist-to-http-parameters parameters)
                           :basic-authorization (when username (list username password))
                           :content-type "application/json"
                           :force-binary t
                           :content (when body
                                      (with-output-to-string (s)
                                        (yason:encode (plist-to-hash-table body) s))))
    (let* ((yason:*parse-object-as* :plist)
           (yason:*parse-object-key-fn* #'gerrit-keyword-to-keyword)
           (response (when body
                       (yason:parse (flex:octets-to-string body :external-format :utf-8)))))
      (if (< status-code 300)
          (values response headers)
          (error 'api-error
                 :http-status status-code
                 :http-headers headers
                 :response response)))))

(defun api-command (url &key body (method :get) (username *username*) (password *password*) parameters)
  (multiple-value-bind
        (body status-code headers)
      (drakma:http-request (format nil (concatenate 'string *host* "~A" )  url )
                           :method method
                           :parameters (plist-to-http-parameters parameters)
                           :basic-authorization (list "zhangdf" "javateam1!")
                           :content-type "application/json"
                           :force-binary t
                           :content (when body
                                      (with-output-to-string (s)
                                        (yason:encode (plist-to-hash-table body) s))))
    (let* ((yason:*parse-object-as* :plist)
           (yason:*parse-object-key-fn* #'gerrit-keyword-to-keyword)
           (response (flex:octets-to-string body :external-format :utf-8)))
      (if (< status-code 300)
          (values response headers)
          (error 'api-error
                 :http-status status-code
                 :http-headers headers
                 :response response)))))

(defmacro booleanize-parameters (plist &rest keys)
  ;; unhygienic
  `(setf ,plist (let (result)
                  (alexandria:doplist (key value ,plist (nreverse result))
                    (push key result)
                    (push (if (member key ',keys)
                              (if value
                                  "true"
                                  "false")
                              value)
                          result)))))

(defmacro define-gerrit-command (name parameters &body body)
  ;; unhygienic
  `(prog1
       (defun ,name (&rest parameters &key ,@parameters)
         (declare (ignorable parameters ,@(loop for parameter in parameters
                                             collect (if (listp parameter) (first parameter) parameter))))
         ,@body)
     (export ',name)))
(defparameter *username* "zhangdf"
  "Username to use for API calls")
(defparameter *password* "javateam1!"
  "Password to use for API calls")
(defparameter *host* "http://192.168.4.4:8080")

(define-gerrit-command list-repositories (org)
  (api-command "/projects/"
               :method :get)  )

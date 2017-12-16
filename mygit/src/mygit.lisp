(in-package :cl-user)
(defpackage mygit
  (:use :cl :alexandria :babel :babel-streams :cl-ppcre))
(in-package :mygit)

;; blah blah blah.

(defgeneric file-to-string-as-lines (pathname)
  (:documentation ""))

(defmethod file-to-string-as-lines ((pathname pathname))
  (with-open-file (stream pathname :direction :input)
    (file-to-string-as-lines stream)))

(defmethod file-to-string-as-lines ((stream stream))
  (with-output-to-string (s)
    (loop for line = (read-line stream nil :eof nil)
       until (eq line :eof)
       do
         (princ line s)
         (terpri s))))

(defparameter *bourne-compatible-shell* "/bin/sh"
  "The path to a Bourne compatible command shell in
physical pathname notation.")

(defvar *shell-search-paths* '("/usr/bin/" "/usr/local/bin/"))

(defun find-command-ending-in-string (command)
  (let ((checking? t))
    (loop for ch across command
       for i from 0 do
         (cond ((and checking? (char= ch #\Space))
                (return i))
               ((char= ch #\\)
                (setf checking? nil))
               (t
                (setf checking? t))))))

(defmethod shell ((command t) &key input)
  "Synchronously execute `command` using a Bourne-compatible shell,
returns (values output error-output exit-status).

The `command` can be a full path to a shell executable binary
or just its name. In the later case, the variable `*shell-search-paths*`
will be used to find the executable.

Depending on the implementation, the variable `*bourne-compatible-shell*`
may be used to find a shell to use in executing `command`."
  (let* ((pos-/ (position #\/ command))
         (pos-space (find-command-ending-in-string command))
         (binary (subseq command 0 (or pos-space)))
         (args (and pos-space (subseq command pos-space))))
    (when (or (not pos-/)
              (and pos-/ pos-space)
              (and pos-space
                   (< pos-/ pos-space)))
      ;; no slash in the command portion, try to find the command with
      ;; our path
      (setf binary
            (or (loop for path in *shell-search-paths* do
                     (let ((full-binary (make-pathname :name binary
                                                       :defaults path)))
                       (when (and (probe-file full-binary)
                                  (directory-pathname-p full-binary))
                         (return full-binary))))
                binary)))
    (multiple-value-bind (output error status)
        (%shell-command (format nil "~a~@[ ~a~]" binary args) input)
      (values output error status))))

(defun directory-pathname-p (pathname)
  "Does `pathname` syntactically  represent a directory?

A directory-pathname is a pathname _without_ a filename. The three
ways that the filename components can be missing are for it to be `nil`,
`:unspecific` or the empty string.
"
  (flet ((check-one (x)
           (not (null (member x '(nil :unspecific "")
                              :test 'equal)))))
    (and (check-one (pathname-name pathname))
         (check-one (pathname-type pathname)))))

(defun %shell-command (command input #+(or) output)
  (let* ((process (sb-ext:run-program
                   *bourne-compatible-shell*
                   (list "-c" command)
                   :wait nil :input input
                   :external-format :utf8
                   :output :stream
                   :error :stream ))
         (output-thread (sb-thread:make-thread
                         #'(lambda ()
                             (file-to-string-as-lines
                              (sb-impl::process-output process)))))
         (error-thread (sb-thread:make-thread
                        #'(lambda ()
                            (file-to-string-as-lines
                             (sb-impl::process-error process))))))
    (let ((error-code
           (sb-impl::process-exit-code (sb-impl::process-wait process)))
          (output-string (sb-thread:join-thread output-thread))
          (error-string (sb-thread:join-thread error-thread)))
      (close (sb-impl::process-output process))
      (close (sb-impl::process-error process))
      (values output-string error-string error-code))))

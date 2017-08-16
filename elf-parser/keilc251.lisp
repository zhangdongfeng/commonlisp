(in-package :elf-parser)
(defparameter *blank-line-regex*  "^\\x0d*$")
(defparameter *line-wrap*  "\\x0d*\\x0a>> ")
(defparameter *invoke-line* "\\bINVOKED BY:")

(defun lines-to-string (lines)
  (let ((str (make-array '(0) :element-type 'base-char
                         :fill-pointer 0 :adjustable t)))
    (with-output-to-string  (s str)
      (loop for line in lines
         do (write-line line  s)))
    str))

(defun string-to-lines  (str)
  (with-input-from-string (s str)
    (loop for line = (read-line s  nil nil)
       while  line  collect line)))

(defun pre-process (path)
  (flet ((read-file-into-lines (path)
           (with-open-file (f path )
             (loop for line = (read-line f  nil nil)
                while  line  collect line)))
         (remove-tricky-lines (lines)
           (remove-if #' (lambda (line)
                           (or (and (scan "L251 LINKER" line)
                                    (scan "\\bPAGE\\b" line))
                               (scan *blank-line-regex* line)
                               (scan "^\\s+-\\S+\\s*$" line)
                               (scan "---      \\*\\*GAP\\*\\*" line )
                               (scan "\\*SFR\\* " line )))
                         lines))
         (remove-line-warp (str)
           (regex-replace-all *line-wrap* str ""  )))
    (let ((proc (compose #'string-to-lines
                         #'remove-line-warp
                         #'lines-to-string
                         #'remove-tricky-lines
                         #'read-file-into-lines)))
      (funcall proc path))))

(defparameter *input-modules * nil)
(defun parse-lines (map-lines)
  (flet ((parse-linker-invoke-line (lines)
           (when (scan *invoke-line* (car lines))
             (progn (cddr lines))
             ))
         (parse-input-modules (lines)
           (let ((modules (nthcdr  4  lines)))
             (loop for m on modules
                when (scan "^  " (car m))
                do (format t (car m))
                else return m)))
         (parse-memory-class (lines)
           (let ((modules (nthcdr  3  lines)))
             (loop for m on modules
                when (scan "^\\d{6}H\\.?\\d?\\b" (car m) )
                do (format t  (car m))
                else when (scan "^\\s+\\d{6}H" (car m))
                do (format t (car m))
                else return m))  )
         (parse-memory-map-module (lines)
           (let ((modules (nthcdr  3  lines)))
             (loop for m on modules
                when (scan "^[0-9A-F]{6}H\\.?\\d?\\b" (car m) )
                do (format t  (car m))
                else return m))  )
         (parse-module-symbols (lines)
           (let ((modules (nthcdr  3  lines)))
             (loop for m on modules
                when (scan "^\\s+[0-9A-F]{8}H\\b" (car m) )
                do (format t  (car m))
                else return m))  )
         (parse-module-symbols-table (lines)
           (let ((modules (nthcdr  3  lines)))
             (loop for m on modules
                unless (scan "^      \\S+" (car m) )
                return m))))
    (let ((proc (compose
                 #'parse-module-symbols-table
                 #'parse-module-symbols
                 #'parse-memory-map-module
                 #'parse-memory-class
                 #'parse-input-modules
                 #'parse-linker-invoke-line)))
      (funcall proc map-lines))))

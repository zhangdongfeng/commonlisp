(in-package :elf-parser)

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

(defun remove-regex (str regex)
  (multiple-value-bind (s  e  r1 r2)
      (scan regex  str)
    (if e
        (values (subseq str e) t)
        (values  str nil))))

(defun read-remove-regex-from-string-rcu (regex string collector)
  (pprint regex)
  (pprint (subseq string 0 10))
  (let ((s1 (remove-regex string "^\\s+" )))
    (multiple-value-bind (m r)
        (scan-to-strings regex  s1)
      (if m
          (read-remove-regex-from-string
           regex
           (remove-regex s1 regex)
           #'(lambda (result str)
               (funcall collector (cons m result) str)))
          (funcall collector nil string)))))

(defun read-remove-regex-from-string (regex string)
  (pprint regex)
  (pprint (subseq string 0 (min 20 (length string))))
  (let ((s1 (remove-regex string "^\\s*" )))
    (multiple-value-bind (m r)
        (scan-to-strings regex  s1)
      (pprint m)
      (format t "~%")
      (if m
          (values m  (remove-regex s1 regex))
          (values nil string)))))

(defun mk-list (ele)
  (if (listp ele)
      ele
      (cons ele nil)))

(defun parse-regex-spec (spec  str)
  (if (null spec)
      (values nil str)
      (let ((regex (car spec)))
        (if (eql regex 'repeat)
            (multiple-value-bind (r  s2)
                (parse-regex-spec (cdr spec) str)
              (multiple-value-bind (s3 comma)
                  (remove-regex s2 "^\\s*,")
                (if comma
                    (multiple-value-bind (r1 s4)
                        (parse-regex-spec spec  s3)
                      (values (cons r (mk-list r1)) s4))
                    (values r s3))))
            (if (listp regex)
                (multiple-value-bind (r s2)
                    (parse-regex-spec regex
                                      (remove-regex str "^\\s*\\(\\s*" ))
                  (let ((s3 (remove-regex s2 "^\\s*\\)\\s*" )))
                    (multiple-value-bind (r1 s4)
                        (parse-regex-spec (cdr spec) s3)
                      (values (cons r r1) s4))))
                (multiple-value-bind (r s2)
                    (read-remove-regex-from-string  regex str)
                  (multiple-value-bind (r1 s3)
                      (parse-regex-spec (cdr spec) s2)
                    (if r1 (values (cons r r1) s3)
                        (values r  s3)))))))))

(defun  parse-regex-spec-by-str (tag spec  string)
  (multiple-value-bind (str found)
      (remove-regex  string  tag)
    (if found
        (parse-regex-spec  spec str))))


(defun parse-regex-spec-by-line (tag spec string)
  (multiple-value-bind (str found)
      (remove-regex  string  tag)
    (if found
        (let*  ((lines (split "\\x0d\\x0a" str)))
          (remove-if #'null (loop for l in lines
                               collect (parse-regex-spec spec l)))))))

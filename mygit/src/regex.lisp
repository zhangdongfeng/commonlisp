(in-package :mygit)

(defun mk-list (ele)
  (if (listp ele)  ele (cons ele nil)))

(defun flatten-list (lst)
  (if (listp lst)  (car lst) lst))

(defparameter *debug* nil)

(defun parse-regex-spec (spec  str)
  "parse string defined by spec, regex will be matched one by one
 non group regex will be omit,  and  grouped keyrex will be collect into result list
spec:`(regex  (keyrex) ...)
result:
'(result-list, str-left)"
  (flet ((parse-regex-from-string (regex string)
           (flet ((remove-regex (str regex)
                    (multiple-value-bind (s  e  r1 r2)
                        (scan regex  str)
                      (if e  (subseq str e)  str))))
             (when *debug*
               (pprint regex)
               (pprint (subseq string 0 (min 20 (length string)))))
             (let ((s1 (remove-regex string "^\\s*" )))
               (multiple-value-bind (m r)
                   (scan-to-strings (concatenate 'string "^"  regex)   s1)
                 (when *debug*
                   (format t "~% result")
                   (pprint m )
                   (pprint r ))
                 (if (> (length r) 0)
                     (values (aref r 0)
                             (remove-regex s1 regex))
                     (values nil (remove-regex s1 regex))))))))
    (cond
      ((null spec) (values nil str))
      (t  (multiple-value-bind (r s2)
              (parse-regex-from-string  (car spec) str)
            (multiple-value-bind (r1 s3)
                (parse-regex-spec (cdr spec) s2)
              (values (cons r r1) s3)))))))


#+(or)
(defun extract-by-marker (start-regex end-regex string)
  (let ((start (scan start-regex string)))
    (if start
        (let* ((ofs (length (scan-to-strings start-regex string)))
               (end (scan end-regex (subseq string (+ start ofs)))))
          (if end
              (let ((real-end (+ start  end ofs)))
                (values (subseq string start real-end )
                        (subseq string real-end)))
              (values (subseq string start)  nil))))))

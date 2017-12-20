(in-package :cl-user)
(defpackage mygit
  (:use :cl :alexandria :flexi-streams  :cl-ppcre))
(in-package :mygit)

;; blah blah blah.

;;(cl-fad:list-directory #p "/Volumes/tmp/tmp/AOSP")

(make-pathname :directory '(:absolute "public" "games")
               :name "chess" :type "db")

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

(defun mk-list (ele)
  (if (listp ele)  ele (cons ele nil)))

(defun flatten-list (lst)
  (if (listp lst)  (car lst) lst))

(defparameter *debug* nil)
(defun parse-regex-spec (spec  str)
  "parse string defined by spec, regex will be matched one by one
 non group regex will be omit,  and  grouped keyrex will be collect into result list
spec:
`(regex  (keyrex) ...)
result:
'(result-list, str-left)"
  (flet ((read-remove-regex-from-string (regex string)
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
               (if r
                   (values (aref r 0)
                           (remove-regex s1 regex))
                   (values nil (remove-regex s1 regex)))))))
    (cond
      ((null spec) (values nil str))
      (t  (multiple-value-bind (r s2)
              (read-remove-regex-from-string  (car spec) str)
            (multiple-value-bind (r1 s3)
                (parse-regex-spec (cdr spec) s2)
              (values (cons r r1) s3)))))))

(defun list-have-elements (seq)
  (reduce  #'(lambda(x y)  (or x y))   seq ))

(defun have-null-elements (seq)
  (not (reduce
        #'(lambda(x y)  (and x y))   seq )))

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

(defun read-file-into-lines (path)
  (with-open-file (f path )
    (loop for line = (read-line f  nil nil)
       while  line  collect line)))

(defparameter %name% "([0-9a-zA-Z/-_.]+)")

(defun parse-manifest (path spec func)
  (let (prjs)
    (setq prjs (mapcar (curry #'parse-regex-spec spec)
                       (read-file-into-lines path)))
    (setq prjs (mapcar func prjs))
    (setq prjs (remove-if-not #'list-have-elements prjs))
    (stable-sort prjs #'string< :key #'car)))

(defun get-mainfest-diff (aosp-xml owl-xml)
  (set-difference (parse-manifest aosp-xml
                                  `( ,(concatenate 'string  "<project path=\"" %name%  "\"" )
                                      ,(concatenate 'string "name=\"" %name%  "\""))
                                  #'(lambda (prj) (list (cadr prj) (car prj))))
                  (parse-manifest owl-xml
                                  `( ,(concatenate 'string  "<project name=\"" %name%  "\"" )
                                      ,(concatenate 'string "path=\"" %name%  "\""))
                                  #'(lambda (prj)  prj))

                  :key #'(lambda (x)
                           (file-namestring (car x))) :test #'string= ))

(get-mainfest-diff #p "/Users/zhangdongfeng/Downloads/manifest.xml"
                   #p "/Users/zhangdongfeng/Downloads/manifest(1).xml")

#+ (or)
(set-difference (parse-manifest aosp-xml
                                `( ,(concatenate 'string  "<project path=\"" %name%  "\"" )
                                    ,(concatenate 'string "name=\"" %name%  "\""))
                                #'(lambda (prj) (list (cadr prj) (car prj))))
                (parse-manifest owl-xml
                                `( ,(concatenate 'string  "<project name=\"" %name%  "\"" )
                                    ,(concatenate 'string "path=\"" %name%  "\""))
                                #'(lambda (prj)  prj))

                :key #'(lambda (x)
                         (file-namestring (car x))) :test #'string= )

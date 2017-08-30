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

(defparameter *lines* nil)

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
      (setq *lines* (funcall proc path))
      nil)))

(defparameter *file* #p "/Users/zhangdongfeng/Downloads/airaha/AB1520S_SVN72747_Headset_OBJ/output/AB1520S/Release_Flash/BTStereoHeadset_AB1520S_FlashLinkRom.MAP")

#+or
(with-open-file (f #p "/Users/zhangdongfeng/Downloads/airaha/AB1520S_SVN72747_Headset_OBJ/output/AB1520S/Release_Flash/BTStereoHeadset_AB1520S_FlashLinkRom.MAP1"  :direction :output :if-exists :overwrite :if-does-not-exist :create)
  (loop for line in *lines*
     do (write-line line  f)))

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
  (pprint (subseq string 0 10))
  (let ((s1 (remove-regex string "^\\s*" )))
    (multiple-value-bind (m r)
        (scan-to-strings regex  s1)
      (if m
          (values m  (remove-regex s1 regex))
          (values nil string)))))

(defun  parse-regex-spec (spec  str)
  (if (null spec)
      (values nil str)
      (let ((regex (car spec))
            (s1 (remove-regex str "^\\s*\\(\\s*" )))
        (if (eql regex 'repeat)
            (multiple-value-bind (r  s2)
                (parse-regex-spec (cdr spec) s1)
              (pprint r)
              (multiple-value-bind (s3 comma)
                  (remove-regex s2 "^\\s*,")
                (if comma
                    (multiple-value-bind (r1 s4)
                        (parse-regex-spec spec  s3)
                      (values (cons r r1) s4))
                    (values r s3))))
            (if (listp regex)
                (multiple-value-bind (r s2)
                    (parse-regex-spec regex s1)
                  (let ((s3 (remove-regex s2 "^\\s*\\)\\s*" )))
                    (multiple-value-bind (r1 s4)
                        (parse-regex-spec (cdr spec) s3)
                      (values (cons r r1) s4))))
                (multiple-value-bind (r s2)
                    (read-remove-regex-from-string  regex s1)
                  (multiple-value-bind (r1 s3)
                      (parse-regex-spec (cdr spec) s2)
                    (pprint r)
                    (if r1 (values (cons r r1) s3)
                        (values r  s3)))))))))

(defparameter *merge-publics* nil)
(defparameter %merge-publics% "\\bMERGEPUBLICS CLASSES\\b")
(defparameter %seg-name% "^\\b[0-9A-Z_]+\\b")
(defparameter %data-addr% "^\\bD?:?0X[0-9A-F]{2,8}-D?:?0X[0-9A-F]{2,8}\\b")
(defparameter  %merge-publics-spec%
  `(,%merge-publics% (repeat ,%seg-name% (repeat ,%data-addr%))))

(defparameter %overlay% "\\bOVERLAY\\b")
(defparameter %overlay-name% "^\\b[0-9A-Z_*]+\\s+!")
(defparameter %overlay-symbol-name% "^\\b[0-9A-Z_*?]+")
(defparameter  %overlay-spec%
  `(,%overlay% (repeat ,%overlay-name% (repeat ,%overlay-symbol-name%))))

(defun parse-linker-invoke-line (lines)

  )

(defparameter *invoke-str* nil)
(defparameter *module-str* nil)
(defparameter *memory-str* nil)
(defparameter *memory-map-str* nil)
(defparameter *symbol-str* nil)
(defparameter *symbol-table-str* nil)
(defun parse-lines (map-lines)
  (flet ((collect-linker-invoke-line (lines)
           (when (scan *invoke-line* (car lines))
             (progn
               (setq *invoke-str* (cadr lines))
               (parse-linker-invoke-line (cadr lines))
               (cddr lines))))
         (collect-input-modules (lines)
           (let ((modules (nthcdr  4  lines)))
             (loop with result = nil
                for m on modules
                when (scan "^  " (car m))
                do (push (car  m) result)
                else  do
                  (let ((lines (nreverse result)))
                    (setq *module-str* (lines-to-string lines)))
                and return m)))
         (collect-memory-class (lines)
           (let ((modules (nthcdr  3  lines)))
             (loop
                with result = nil
                for m on modules
                when (scan "^\\d{6}H\\.?\\d?\\b" (car m) )
                do (push (car  m) result)
                else when (scan "^\\s+\\d{6}H" (car m))
                do (format t (car m))
                else do (let ((lines (nreverse result)))
                          (setq *memory-str* (lines-to-string lines)))
                and return m)))
         (collect-memory-map-module (lines)
           (let ((modules (nthcdr  3  lines)))
             (loop
                with result = nil
                for m on modules
                when (scan "^[0-9A-F]{6}H\\.?\\d?\\b" (car m) )
                do (push (car  m) result)
                else do (let ((lines (nreverse result)))
                          (setq *memory-map-str* (lines-to-string lines)))
                and return m))  )
         (collect-module-symbols (lines)
           (let ((modules (nthcdr  3  lines)))
             (loop
                with result = nil
                for m on modules
                when (scan "^\\s+[0-9A-F]{8}H\\b" (car m) )
                do (push (car  m) result)
                else do (let ((lines (nreverse result)))
                          (setq *symbol-str* (lines-to-string lines)))
                and return m)))
         (collect-module-symbols-table (lines)
           (let ((modules (nthcdr  3  lines)))
             (loop for m on modules
                unless (scan "^      \\S+" (car m) )
                return m))))
    (let ((proc (compose
                 #'collect-module-symbols-table
                 #'collect-module-symbols
                 #'collect-memory-map-module
                 #'collect-memory-class
                 #'collect-input-modules
                 #'collect-linker-invoke-line)))
      (funcall proc map-lines))))

(in-package :elf-parser)

(defun nm-pre-process (path)
  (flet ((read-file-into-lines (path)
           (with-open-file (f path )
             (loop for line = (read-line f  nil nil)
                while  line  collect line)))
         (remove-tricky-lines (lines)
           (remove-if #' (lambda (line)
                           (or
                            (scan *blank-line-regex* line)
                            (scan "^00000000 N __ARM_grp" line)
                            (scan "---      \\*\\*GAP\\*\\*" line )
                            (scan "\\*SFR\\* " line )))
                         lines)))
    (let ((proc (compose #'string-to-lines
                         #'lines-to-string
                         #'remove-tricky-lines
                         #'read-file-into-lines)))
      (setq *lines* (funcall proc path)))))

(defparameter *undefine-symbols* nil)
(defparameter *define-symbols* nil)

(defun nm-all-defined-symbos (modules)
  (flet ((nm-parse-sym (str)
           (let ((syms (remove-if #'have-null-elements
                                  (parse-regex-spec-by-line
                                   (concatenate 'string %obj-file-name% ":")
                                   `("\\S+" "B|R|D|T|t"  ,%symbol-name% )  str))))
             (mapcar #'caddr syms))))
    (flatten (mapcar #'nm-parse-sym modules))))

(defun nm-all-undefined-symbos (modules)
  (flet ((nm-parse-sym (str)
           (let ((syms (remove-if #'have-null-elements
                                  (parse-regex-spec-by-line
                                   (concatenate 'string %obj-file-name% ":")
                                   `( "U"  ,%symbol-name% )  str))))
             (mapcar #'cadr syms))))
    (flatten (mapcar #'nm-parse-sym modules))))

(defun nm-collect-module (sym-tbl)
  (loop with result = nil
     with str =  sym-tbl
     with res-str = nil
     do (progn
          (multiple-value-bind (res  rest-str)
              (extract-by-marker
               (concatenate 'string %obj-file-name% ":")
               (concatenate 'string %obj-file-name% ":")
               str)
            (push res  result)
            (setq str rest-str
                  res-str res)))
     when (not res-str)
     return (nreverse result)))

(defun read-file-into-lines (path)
  (with-open-file (f path )
    (loop for line = (read-line f  nil nil)
       while  line  collect line)))

(defun nm-parse (path)
  (let* ((lines (nm-pre-process path))
         (modules (nm-collect-module (lines-to-string lines))))
    (setq
     *undefine-symbols* (nm-all-undefined-symbos modules)
     *define-symbols* (nm-all-defined-symbos modules))
    (delete-duplicates (stable-sort
                        (remove-if #'(lambda (s)
                                       (find s *define-symbols* :test #'string=))
                                   *undefine-symbols*)
                        #'string< )
                       :test #'string=)))
#+or
(with-open-file (f #p "/Users/zhangdongfeng/Downloads/00005078-SDK-patch_RTL8189FTV_v02.6_v12728_SDIO/release-v02.6/component/common/drivers/wlan/realtek/wlan_lib/cortex-m/mdk/wlan_nm1.lst"  :direction :output :if-exists :overwrite :if-does-not-exist :create)
  (loop for line in *lines*
     do (write-line line  f)))

(defclass obj-symbol ()
  ((obj-name  :initarg :obj-name
              :accessor  obj-name)
   (t-sym  :initarg :t-sym
           :accessor  t-sym)
   (u-sym :initarg :u-sym
          :accessor u-sym)))

(defun parse-obj-symbol-from-string  (str)
  (declare (optimize debug))
  (let ((obj-name-regex "(\\S+\\.o)")
        (t-regex "[0-9a-fA-F]+ T (\\S+)")
        (u-regex "\\s+U (\\S+)")
        (obj-name nil)
        (t-syms nil)
        (u-syms nil))
    (dolist (l (string-to-lines str))
      (progn
        (register-groups-bind  (name) (obj-name-regex l)
          (if name (setq obj-name name)))
        (register-groups-bind (sym) (t-regex l)
          (if sym (push (cons sym obj-name)  t-syms)))
        (register-groups-bind (sym) (u-regex l)
          (if sym (push (cons sym obj-name)  u-syms)))))
    (if obj-name
        (make-instance
         'obj-symbol :obj-name obj-name :t-sym t-syms :u-sym u-syms))))

(defun resolve-syms (obj  t-syms)
  (declare (optimize debug))
  (let ((u-syms (u-sym obj)))
    (loop for usym in u-syms
       collect (let ((found (find (car usym) t-syms
                                  :test #'string= :key #'car)))
                 (cons usym found)))))

(defun analyse-module-dependency (obj-syms)
  (declare (optimize debug))
  (let ((t-syms (get-all-t-syms obj-syms)))
    (loop for obj in obj-syms
       collect (cons (obj-name obj) (resolve-syms obj t-syms)))))


(defun get-all-t-syms (obj-syms)
  (reduce #'append (mapcar #'t-sym obj-syms)))

(defun get-nm-from-cmdline (cmd)
  (let* ((lines (string-to-lines cmd))
         (lines (remove-if  #'(lambda(l)(scan "built-in\\.o" l))
                            lines)))
    (trivial-shell:shell-command
     (concatenate 'string "arm-none-eabi-nm "
                  (reduce #'(lambda (a b) (concatenate 'string a " " b))
                          lines )))))

(defun get-obj-syms-from-nm (nm)
  (let* ((obj-nm (split (create-scanner "^\\s*$" :multi-line-mode t) nm))
         (objs (mapcar #'parse-obj-symbol-from-string obj-nm)))
    objs))

(defun merge-objs (objs)
  (let* ((usym (reduce #'append (loop for i in objs collect (u-sym i))))
         (tsym (reduce #'append (loop for i in objs collect  (t-sym i))))
         (usym-new (remove-if #'(lambda (s)
                                  (find (car s) tsym :test #'string= :key #'car))
                              usym)))
    (make-instance 'obj-symbol :obj-name (directory-namestring (obj-name (car objs)))
                   :t-sym tsym :u-sym usym-new)))

(defun get-component-from-objs (objs)
  (let* ((non-empty-objs (remove-if-not #'(lambda (o) (or (t-sym o) (u-sym o))) objs))
         (paths (mapcar #'(lambda (f) (directory-namestring (obj-name f)))
                        non-empty-objs))
         (com-name (remove-duplicates paths :test #'string=))
         (components (mapcar #'(lambda (name)
                                 (remove-if-not
                                  #'(lambda (obj)
                                      (let ((obj-dir (directory-namestring (obj-name obj))))
                                        (string=  name obj-dir)))
                                  non-empty-objs))
                             com-name)))
    (mapcar #'merge-objs components)))

(defun get-dependency-count (d)
  (let* ((usyms (remove-if #'cdr (cdr d)))
         (syms (remove-if-not #'cdr (cdr d)))
         (syms (remove-duplicates syms :key #'(lambda (s)
                                                (directory-namestring (cddr s)))
                                  :test #'string=)))
    (list (length syms) (length usyms))))

(defun get-component-dependency  (path)
  (let* ((cmd (read-file-into-string path ))
         (objs (get-obj-syms-from-nm (get-nm-from-cmdline cmd)))
         (components (get-component-from-objs objs))
         (dependency (analyse-module-dependency components)))
    (sort dependency  #'> :key #'(lambda (d)
                                   (car (get-dependency-count d))))))

(defun dump-dependency (d)
  (let* ((c-name (car d))
         (syms (remove-if-not #'cdr (cdr d))))
    (format t  "~a~%" c-name)
    (dolist (s syms)
      (let* ((sym (car s))
             (mod-dir (reverse (pathname-directory (cddr s))))
             (mod-name (if (cadr mod-dir) (concatenate 'string  (cadr mod-dir) "/" (car mod-dir))  (car mod-dir)))
             (namestring (directory-namestring (cddr s))))
        (format t "~a   ~a~%" (car sym) namestring)))))


(defun show-component-dependency (dep)
  (let ((dep (stable-sort dep #'string> :key #'car)))
    (dolist (d dep)
      (let* ((c-name (car d))
             (syms (remove-if-not #'cdr (cdr d)))
             (syms (remove-duplicates syms :key #'(lambda (s)
                                                    (directory-namestring (cddr s)))
                                      :test #'string=)))
        (format t  "~a ~d ~d ~%" c-name (car (get-dependency-count d))
                (cadr (get-dependency-count d)))
        (dolist (s syms)
          (let* ((sym (car s))
                 (mod-dir (reverse (pathname-directory (cddr s))))
                 (mod-name (if (cadr mod-dir) (concatenate 'string  (cadr mod-dir) "/" (car mod-dir))  (car mod-dir))))
            (format t "~a~%"  mod-name)))))))

#+or
(defparameter *d* (get-component-dependency "/Users/zhangdongfeng/Downloads/ATS350B/cmd.txt"))




;;(defparameter *t-syms* (mapcan #'t-sym (remove-if-not #'t-sym *m*)))

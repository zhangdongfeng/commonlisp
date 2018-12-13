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

(defun get-nm-from-cmdline (cmd)
  (let* ((lines (string-to-lines cmd))
         (lines (remove-if  #'(lambda(l)(scan "built-in\\.o" l))
                            lines)))
    (trivial-shell:shell-command
     (concatenate 'string *nm-cmd*
                  (reduce #'(lambda (a b) (concatenate 'string a " " b))
                          lines )))))

(defun get-obj-syms-from-nm (nm)
  (let* ((obj-nm (split (create-scanner "^\\s*$" :multi-line-mode t) nm))
         (objs (mapcar #'parse-obj-symbol-from-string obj-nm)))
    objs))




(defclass component ()
  ((name  :initarg :name
          :accessor  name)
   (objs  :initarg :objs
          :accessor  objs)
   (resolved-objs  :initarg :resolved-objs
                   :accessor  resolved-objs)
   (usyms  :initarg :usyms
           :accessor  usyms)
   (rsyms  :initarg :rsyms
           :accessor  rsyms)
   (tsyms  :initarg :tsyms
           :accessor  tsyms)))

(defclass resolved-symbol ()
  ((name  :initarg :name
          :accessor  name)
   (ref-module  :initarg :ref-module
                :accessor  ref-module)
   (def-module :initarg :def-module
     :accessor def-module)))

(defun get-component-from-objs (objs)
  (let* ((non-empty-objs (remove-if-not #'(lambda (o) (or (t-sym o) (u-sym o))) objs))
         (paths (remove-duplicates
                 (mapcar #'(lambda (f) (directory-namestring (obj-name f)))
                         non-empty-objs)
                 :test #'string=))
         (path-objs (mapcar #'(lambda (name)
                                (remove-if-not
                                 #'(lambda (obj)
                                     (let ((obj-dir (directory-namestring (obj-name obj))))
                                       (string=  name obj-dir)))
                                 non-empty-objs))
                            paths))
         (components (mapcar
                      #'(lambda (name objs) (make-instance 'component :name name :objs objs))
                      paths path-objs)))
    (mapcar #'component-merge-objs  components)
    components))


(defun resolve-obj (obj  syms)
  (let ((usyms (u-sym obj))
        (resolved nil))
    (dolist (u usyms)
      (let ((found (find (car u) syms
                         :test #'string= :key #'car)))
        (if found
            (push (make-instance 'resolved-symbol
                                 :name (car u) :ref-module (cdr u) :def-module  (cdr found)) resolved))))
    (cons (obj-name obj) resolved)))

(defmethod component-merge-objs ( (c component))
  (let* ((objs  (objs c))
         (usym (reduce #'append (loop for i in objs collect (u-sym i))))
         (tsym (reduce #'append (loop for i in objs collect  (t-sym i))))
         (usym-new (remove-if #'(lambda (s)
                                  (find (car s) tsym :test #'string= :key #'car))
                              usym))
         (resolve-objs nil))
    (dolist (o objs)
      (push (resolve-obj o tsym) resolve-objs))
    (setf (tsyms c) tsym
          (usyms c) usym-new
          (resolved-objs c) resolve-objs)))


(defmethod component-resolve-syms ( (c component)  t-syms)
  (declare (optimize debug))
  (let ((u-syms (usyms c))
        (resolved nil)
        (unresolved nil))
    (dolist (usym u-syms)
      (let ((found (find (car usym) t-syms
                         :test #'string= :key #'car)))
        (if found
            (push (make-instance 'resolved-symbol
                                 :name (car usym) :ref-module (cdr usym) :def-module  (cdr found)) resolved)
            (push usym unresolved))))
    (setf (usyms c) unresolved
          (rsyms c) resolved)
    c))

(defmethod component-get-depend-components ( (c component))
  (let* ((module-names (mapcar #'def-module (rsyms c)))
         (component-names (mapcar #'directory-namestring module-names) ))
    (remove-duplicates component-names
                       :test #'string=)))

(defmethod component-get-depend-modules ( (c component))
  (let ((module-names (mapcar #'def-module (rsyms c))))
    (remove-duplicates module-names
                       :test #'string=)))

(defmethod component-get-modules ( (c component))
  (let ((module-names (mapcar #'ref-module (rsyms c))))
    (remove-duplicates module-names :test #'string=)))

(defmethod component-get-depend-components-of ( (c component) ref-module)
  (declare (optimize debug))
  (let* ((syms (rsyms c))
         (syms (remove-if-not  #'(lambda (s) (string= ref-module (ref-module s))) syms))
         (modules (mapcar #'def-module syms))
         (modules (mapcar #'directory-namestring modules)))
    (remove-duplicates  modules :test #'string=)))

(defun get-module-shortname (full-name)
  (let* ((mod-dir (reverse (pathname-directory full-name)))
         (mod-name (file-namestring full-name))
         (mod-dir (if (cadr mod-dir)
                      (concatenate 'string  (cadr mod-dir) "/" (car mod-dir))
                      (car mod-dir))))
    (concatenate 'string mod-dir "/" mod-name)))

(defmethod component-dump ( (c component))
  (declare (optimize debug))
  (let* ( (c-name (name c))
         (usyms (usyms c))
          (syms (rsyms c))
          (objs  (resolved-objs c))
          (depend-components (component-get-depend-components c))
          (depend-modules (component-get-depend-modules c))
          (modules (component-get-modules c)))
    (format t  "~%~a~%" c-name)
    (format t  "depends modules: ~d  unresolved symbols: ~d ~%"
            (length depend-components) (length usyms))
    (format t  "~{~a~%~} ~%" (mapcar #'get-module-shortname depend-components))
    ;;(format t  "~{~a~%~} ~%" (mapcar #'get-module-shortname depend-modules))
    (dolist (m modules )
      (format t  "~a  ~%~{~a~%~} ~%" (file-namestring m)
              (mapcar #'get-module-shortname (component-get-depend-modules-of c m)) ))
    (format t  "component internal symbols:~%")
    (dolist (o objs)
      (format t "~a ~%" (file-namestring (car o)))
      (let ((syms (cdr o)))
        (dolist (s syms)
          (format t  "~a  ==>  ~a ~%" (name s) (file-namestring (def-module s))))))
    (format t  "~%component  symbols:~%")
    (dolist (s syms)
      (format t  "~a/~a  ==>  ~a ~%" (name s)  (file-namestring (ref-module s))
              (get-module-shortname (def-module s))))
    #+ (or)
    (progn
      (format t  "~%component  undefined symbols:~%")
      (dolist (u usyms)
        (format t  "~a from ~a  undefined ~%"  (car u)  (file-namestring (cdr u)))))))

(defun analyse-module-dependency (components)
  (declare (optimize debug))
  (let ((t-syms  (reduce #'append (mapcar #'tsyms components))))
    (mapcar #'(lambda (c) (component-resolve-syms c t-syms)) components)))

(defun get-component-dependency  (path)
  (let* ((cmd (read-file-into-string path ))
         (objs (get-obj-syms-from-nm (get-nm-from-cmdline cmd)))
         (components (get-component-from-objs objs))
         (dependency (analyse-module-dependency components)))
    (sort dependency  #'> :key #'(lambda (d)
                                   (car (get-dependency-count d))))))

(defun show-component-dependency (dep)
  (let ((dep (stable-sort dep #'string> :key #'name)))
    #+ (or)
    (dolist (c dep)
      (let* ( (c-name (name c))
             (usyms (usyms c))
              (syms (rsyms c))
              (depend-components (component-get-depend-components c))
              (depend-modules (component-get-depend-modules c))
              (modules (component-get-modules c)))
        (format t  "~a  ~%" c-name)
        (format t  "~%depends modules: ~d  unresolved symbols: ~d ~%"
                (length depend-components) (length usyms))
        (format t  "~{~a~%~} ~%" (mapcar #'get-module-shortname depend-components))
        ;;(format t  "~{~a~%~} ~%" (mapcar #'get-module-shortname depend-modules))
        (dolist (m modules )
          (format t  "~a  ~%~{~a~%~} ~%" (file-namestring m)
                  (mapcar #'get-module-shortname (component-get-depend-modules-of c m)) ))))
    (dolist (c dep)
      (component-dump c))))

(defparameter *nm-cmd* "/opt/mips-2014.11/bin/mips-sde-elf-nm ")

#+ (or)
(defparameter *d* (get-component-dependency "/home/local/ACTIONS/zhangdf/sdk/5211/gs700e/android/android/out/target/product/s700_cb7/obj/STATIC_LIBRARIES/cmd.txt"))
#+ (or)
(defparameter *d* (get-component-dependency "/home/local/ACTIONS/zhangdf/sdk/3505/GL5118_WIFI/samples/storyapp/outdir/cmd.txt"))



;;(defparameter *t-syms* (mapcan #'t-sym (remove-if-not #'t-sym *m*)))

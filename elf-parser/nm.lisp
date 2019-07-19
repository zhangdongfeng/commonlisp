(in-package :elf-parser)
(defun read-file-into-lines (path)
  (with-open-file (f path )
    (loop for line = (read-line f  nil nil)
       while  line  collect line)))

(defparameter *nm-cmd* "/opt/mips-2014.11/bin/mips-sde-elf-nm ")
(defparameter *METAS* nil)
(defparameter *ALL-USED-SYMS* nil)
(defparameter  *ALL-RESOLVED-SYMS* nil)
(defparameter *META-COMPONENTS* nil)

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

(defun get-nm-from-files (files)
  (let* ((lines (string-to-lines files))
         (lines (remove-if  #'(lambda(l)(scan "built-in\\.o" l))
                            lines)))
    (trivial-shell:shell-command
     (concatenate 'string *nm-cmd*
                  (reduce #'(lambda (a b) (concatenate 'string a " " b))
                          lines )))))

(defun get-obj-syms-from-lib (lib)
  (declare (optimize debug))
  (let*  ((nm  (trivial-shell:shell-command
                (concatenate 'string *nm-cmd* lib))            )
          (obj-nm (split (create-scanner "^\\s*$" :multi-line-mode t) nm))
          (objs (mapcar #'parse-obj-symbol-from-string obj-nm)))
    (mapcar #'(lambda (o)
                (setf (obj-name o) (concatenate 'string lib "/" (obj-name o))))  objs)
    objs))

(defun get-objs-syms-from-files (files)
  (declare (optimize debug))
  (let* ((lines (string-to-lines files))
         (libs  (remove-if-not  #'(lambda(l)(scan "/\\S+\\.a" l))
                                lines))
         (objs  (remove-if  #'(lambda(l)(scan "/\\S+\\.a" l))
                            lines))
         (objs (remove-if  #'(lambda(l)(scan "built-in\\.o" l))
                           objs))
         (objs (reduce #'(lambda (a b) (concatenate 'string a " " b))
                       objs))
         (objs-nms (trivial-shell:shell-command
                    (concatenate 'string *nm-cmd* objs)))
         (obj-nm-list (split (create-scanner "^\\s*$" :multi-line-mode t) objs-nms))
         (obj-syms (mapcar #'parse-obj-symbol-from-string obj-nm-list))
         (obj-syms (remove-if-not #'identity obj-syms))
         (lib-syms (if libs (mapcan #'get-obj-syms-from-lib libs))))
    (append obj-syms lib-syms)))

(defclass component ()
  ((name  :initarg :name
          :accessor  name)
   (objs  :initarg :objs
          :accessor  objs)
   (usyms  :initarg :usyms
           :accessor  usyms)
   (rsyms  :initarg :rsyms
           :accessor  rsyms)
   (isyms  :initarg :isyms
           :accessor  isyms)
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
  (declare (optimize debug))
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

(defmethod component-merge-objs ( (c component))
  (declare (optimize debug))
  (let* ((objs  (objs c))
         (usym (reduce #'append (loop for i in objs collect (u-sym i))))
         (tsym (reduce #'append (loop for i in objs collect  (t-sym i))))
         (find-internal-syms-func #'(lambda (s)
                                      (find (car s) tsym :test #'string= :key #'car)))
         (usym-new (remove-if find-internal-syms-func usym))
         (internal-sym (remove-if-not find-internal-syms-func usym))
         (internal-sym (mapcar find-internal-syms-func internal-sym)))
    (setf (tsyms c) tsym
          (isyms c) internal-sym
          (usyms c) usym-new)))

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
    (setf (usyms c) (remove-duplicates  unresolved  :test #'string= :key #'car)
          (rsyms c) resolved)
    c))

(defun get-meta-component-name-from (dir-name)
  (let* ((matches (mapcar #'(lambda (m) (if (search m dir-name) m nil))  *METAS*))
         (path  (reduce #'(lambda (a b)  (if  (>  (length a) (length b))  a b))  matches)))
    (if path  path dir-name)))

(defmethod component-get-depend-components ( (c component))
  (let* ((module-names (mapcar #'def-module (rsyms c)))
         (component-names (mapcar #'directory-namestring module-names) )
         (component-names (remove-duplicates component-names :test #'string=)))
    (if *METAS*
        (remove-duplicates
         (mapcar #'(lambda (c) (get-meta-component-name-from c))
                 component-names)
         :test #'string=)
        component-names)))

(defun get-module-shortname (full-name)
  (if *METAS*
      (let* ((meta-name (get-meta-component-name-from full-name))
             (start (search meta-name full-name)))
        (if start
            (subseq  full-name start)
            (subseq  full-name 0)))
      (let* ((mod-dir (reverse (pathname-directory full-name)))
             (mod-name (file-namestring full-name))
             (mod-dir (if (cadr mod-dir)
                          (concatenate 'string  (cadr mod-dir) "/" (car mod-dir))
                          (car mod-dir))))
        (concatenate 'string mod-dir "/" mod-name))))

(defun show-component-dependency (obj-files-path)
  (let* ((files (read-file-into-string obj-files-path ))
         (objs (get-objs-syms-from-files files))
         (components (get-component-from-objs objs))
         (t-syms  (reduce #'append (mapcar #'tsyms components))))
    (mapcar #'(lambda (c) (component-resolve-syms c t-syms)) components)
    (dolist (c components)
      (component-dump c))))

(defun get-meta-component-from-objs (metas objs)
  (let ((comp nil)
        ( rem-objs objs))
    (setq comp
          (mapcar
           #'(lambda (m)
               (let* ((match-func #'(lambda (obj) (search m (obj-name obj))))
                      (match-objs (remove-if-not  match-func  rem-objs) ))
                 (setq rem-objs (remove-if match-func  rem-objs))
                 (make-instance 'component :name m :objs match-objs)))
           metas))
    (push (make-instance 'component :name "others" :objs rem-objs) comp)
    (mapc #'component-merge-objs  comp)))

(defun get-path-from-objs (objs)
  (declare (optimize debug))
  (let* ((non-empty-objs (remove-if-not #'(lambda (o) (or (t-sym o) (u-sym o))) objs))
         (paths (mapcar #'(lambda (f) (directory-namestring (obj-name f)))
                        non-empty-objs)))
    (stable-sort (remove-duplicates paths  :test #'string=)  #'string<)))

(defun get-ref-components-by (sym)
  (let* ((coms (remove-if-not   #'(lambda (r) (string= sym  (name r)))
                                *ALL-RESOLVED-SYMS*))
         (coms (remove-duplicates coms :key  #'ref-module :test #'string=))
         (paths (mapcar #'ref-module coms)))
    ;;(format  t  "~a ~{~a ~%~}~% "  sym paths)
    (remove-duplicates (mapcar  #'get-meta-component-name-from  paths)  :test #'string= )))


(defun get-required-symbols-from (syms)
  (let* ((syms1 (remove-duplicates syms :key #'(lambda (s) (name s)) :test #'string=))
         (tmp syms1)
         (res nil))
    (dolist (m *METAS*)
      (push  (remove-if-not #'(lambda (s) (search m (def-module s)))  tmp)   res)
      (setq tmp  (remove-if #'(lambda (s) (search m (def-module s)))  tmp)))
    (reverse res)))


(defmethod component-dump ( (c component))
  (declare (optimize debug))
  (let* ( (c-name (name c))
         (prefix-size (length c-name))
          (usyms (usyms c))
          (tsyms (tsyms c))
          (isyms (isyms c))
          (syms (rsyms c))
          (syms1 (get-required-symbols-from syms))
          (used-sym-func #'(lambda (r) (find  r *ALL-USED-SYMS* :test #'(lambda (a b)
                                                                     (string= (car a ) (name b))))))
          (used-syms (remove-if-not used-sym-func tsyms))
          (unused-syms (remove-if used-sym-func tsyms))
          (unused-syms (remove-if
                        #'(lambda (s) (find (car s) isyms :test #'string= :key #'car))
                        unused-syms))
          (depend-components (component-get-depend-components c)))
    (format t  "~&~%~a~%" c-name)
    (format t  "depends components: ~d  unresolved symbols: ~d~%"
            (length depend-components) (length usyms))
    (format t  "~{~a~%~} ~%" (mapcar #'get-module-shortname depend-components))
    (format t  "~%component  symbols:~%")
    (dolist (s syms)
      (format t  "~a@~a  ==>  ~a~%" (name s)  (get-module-shortname (ref-module s))
              (get-module-shortname (def-module s))))
    (format t  "~%component  provided symbols:~%")
    (let (tmp)
      (dolist (u used-syms)
        (let* ((short-name (get-module-shortname (cdr u)))
               (module-name  (subseq  short-name  (min prefix-size (length short-name) ) )))
          (if (string= tmp module-name) nil
              (progn
                (setq tmp module-name)
                (format t "~a~%" module-name)))
          (format t  "~&~a       ~{~a ~}~%"  (car u)  (get-ref-components-by (car u))))))
    (format t  "~%component  required symbols:")
    (dolist (s syms1)
      (if s (format t  "~&~a:"  (get-meta-component-name-from (def-module (car s)))))
      (dolist (s1 s)
        (format t  "~a  " (name s1))))
    (format t  "~&~%component  internal used symbols:~%")
    (dolist (u isyms)
      (format t  "~a     ~a~%"  (car u) (get-module-shortname (cdr u))))
    (format t  "~%component  not used symbols, can be static or delete:~%")
    (dolist (u unused-syms)
      (format t  "~a     ~a~%"  (car u) (get-module-shortname (cdr u))))
    (format t  "~%component  undefined symbols:~%")
    (dolist (u usyms)
      (format t  "~a "  (car u))))  )

(defun show-meta-component-dependency (obj-files-path  meta-file-path)
  (let* ((files (read-file-into-string obj-files-path ))
         (objs (get-objs-syms-from-files files))
         (obj-paths (get-path-from-objs objs))
         (metas (read-file-into-lines meta-file-path))
         (metas (remove-duplicates metas  :test #'string=))
         (meta-comonents (get-meta-component-from-objs metas objs))
         (t-syms  (reduce #'append (mapcar #'tsyms meta-comonents)))
         (metas (mapcar #'directory-namestring metas)))
    (mapcar #'(lambda (c) (component-resolve-syms c t-syms)) meta-comonents)
    (format t  "all modules: ~%~{~a~%~} ~%" obj-paths)
    (format t  "components: ~%~{~a~%~} ~%" metas)
    (let* ((*METAS* metas)
           (*ALL-RESOLVED-SYMS* (reduce #'append (mapcar #'rsyms meta-comonents)))
           (*ALL-USED-SYMS* (remove-duplicates *ALL-RESOLVED-SYMS*  :key #'(lambda (x) (name x)) :test  #'string= )))
      (dolist (c meta-comonents)
        (component-dump c)))
    (setq *METAS* metas)
    (setq *ALL-RESOLVED-SYMS* (reduce #'append (mapcar #'rsyms meta-comonents)))
    (setq *ALL-USED-SYMS* (remove-duplicates *ALL-RESOLVED-SYMS*  :key #'(lambda (x) (name x)) :test  #'string= ))
    (setq *META-COMPONENTS*  meta-comonents)
    nil))


(defun component-main ()
  (if (= (length sb-ext:*posix-argv*) 3)
      (let*  ((objs-file  (nth 1 sb-ext:*posix-argv*))
              (metas-file  (nth 2 sb-ext:*posix-argv*)))
        (show-meta-component-dependency objs-file metas-file))
      (format t "usage:dump-component   objs-input-file  componet-input-file ")))

#+(or)
(sb-ext:save-lisp-and-die #p "dump-component" :toplevel #'component-main :executable t )

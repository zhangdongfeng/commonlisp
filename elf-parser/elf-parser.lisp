;;;; elf-parser.lisp

(in-package :elf-parser)

(defparameter *elf* nil)
(defparameter *debug-infos* nil)

;;; "elf-parser" goes here. Hacks and glory await!
(defmacro with-dw-att-name ((&rest at-name) compile-unit &body body)
  "bind dw-att-name to its value in a die"
  `(let (,@(mapcar
            #'(lambda (x)
                `(,x (loop for (name val) in ,compile-unit
                        when (eq name ',x)  return  val)))
            at-name))
     ,@body))

(defmacro with-dw-debug-entry ((tag val children) compile-unit &body body)
  (alexandria:once-only (compile-unit)
    `(let ((,tag (car ,compile-unit))
           (,val (cadr ,compile-unit))
           (,children (caddr ,compile-unit)))
       ,@body)))

(defmacro define-accessor (&rest methods)
  "define trivial accessor functions,
methods: list function spec
first is the function name,  second is the function"
  (let ((arg (gensym)))
    `(progn
       ,@(mapcar  #'(lambda (x)
                      `(defun ,(car x) (,arg)
                         (funcall ,(cadr x) ,arg)))
                  methods))))

(define-accessor
    (die-tag #'car)
    (die-attr #'cadr)
  (die-children #'caddr))


(defclass debug-symbol ()
  ((name  :initarg :name
          :accessor name)
   (file-name :initarg :file-name
              :accessor file-name  )))

(defun find-debug-symbols (compile-unit)
  "  find symbols in elf debug  info,   dw_tag_subprogram or  dw_tag_variable
compile-unit:  the dwart info of compile unit,  list of  -tag val children-
"
  (declare (optimize debug))
  (let* ((file-attrs (die-attr compile-unit) )
         (file-name (with-dw-att-name (dw_at_name) file-attrs  dw_at_name))
         (res nil))
    (flet (( make-debug-symbol (die)
             (with-dw-att-name ( dw_at_name)  (die-attr die)
               (make-instance 'debug-symbol
                              :name dw_at_name
                              :file-name file-name))))
      (dolist (entry (die-children compile-unit))
        (case (die-tag entry)
          (dw_tag_subprogram
           (push (make-debug-symbol entry) res))
          (dw_tag_variable
           (push (make-debug-symbol entry) res))
          (otherwise  nil)))
      res)))

(defparameter *all-symbols* nil)

(defclass file-symbol ()
  ((elf-sym  :initarg :elf-sym
             :accessor  elf-sym)
   (file-name :initarg :file-name
              :accessor file-name  )))

(defun get-all-debug-symbols (debug-info)
  "get all soruce file global symbols,
tag: list of dwarf tag
==> \(file-name \( elf-symbol * \) \)"
  (let* ((symbols (elf:data (elf:named-section *elf* ".symtab")))
         (debug-symbols  (loop for cu in debug-info
                            nconc (find-debug-symbols cu)))
         (res nil))
    (dolist (sym symbols)
      (let*  ( (sym-name (elf:sym-name sym))
              (file   (find  sym-name debug-symbols
                             :key #'name :test #'string=)))
        (if file
            (let ((fsym (make-instance 'file-symbol :elf-sym sym :file-name (file-name file))))
              (push fsym res))
            (let ((fsym (make-instance 'file-symbol :elf-sym sym :file-name "nofile")))
              (push fsym res)))))
    (setq *all-symbols*  res)
    nil))


(defun read-elf (file)
  "read elf file content
file: the full path of elf file"
  (setf *elf* (elf:read-elf file))
  (when (elf:named-section *elf* ".debug_info")
    (setf *debug-infos* (dw-get-debug-info *elf*  file  (elf:sh (elf:named-section *elf* ".debug_info")))))
  nil)

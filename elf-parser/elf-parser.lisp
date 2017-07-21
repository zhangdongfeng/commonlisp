;;;; elf-parser.lisp

(in-package :elf-parser)

(defparameter *elf* nil)
(defparameter *file* nil)
(defparameter *symbols* nil)
(defparameter *debug-infos* nil)
(defparameter *all-files* nil)

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

(defun find-symbols(compile-unit  types)
  "find compile unit's symbol in elf symbols tab section
compile-unit: the dwarf compile-unit die info
types: list of dw_tag_xxx
==> \( compile-unit's path  \( elf-symbol * \) \)"
  (declare (optimize debug))
  (let ((res nil))
    (dolist (entry (die-children compile-unit))
      (when (find (die-tag entry) types)
        (ecase (die-tag entry)
          ('dw_tag_subprogram
           (with-dw-att-name
               ( dw_at_name  DW_AT_high_pc ) (die-attr entry)
             (when dw_at_high_pc
               (push (find dw_at_name  *symbols*
                           :key #'elf:sym-name :test #'string=)
                     res))))
          ('dw_tag_variable
           (with-dw-att-name
               (dw_at_name DW_AT_location) (die-attr entry)
             (when (> (length dw_at_location) 2)
               (push (find dw_at_name  *symbols*
                           :key #'elf:sym-name :test #'string=)
                     res))))
          (others nil))))
    (with-dw-att-name (dw_at_name) (die-attr compile-unit)
      (list dw_at_name (remove-if #'null  res )))))



(defun get-dw-all-files (debug-info)
  "find all source file name  from dwarf debug info section
debug-info: the infos return by  dw-get-debug-info
==> \( path * \(elf-symbol * \) \)"
  (let ((all (loop for unit in debug-info
                collect (with-dw-att-name (dw_at_name)
                            (die-attr unit)
                          dw_at_name))))
    (delete-duplicates all :test #'string=)))

(defun read-elf (file)
  "read elf file content
file: the full path of elf file"
  (setf *elf* (elf:read-elf file)
        *file* file)
  (when (elf:named-section *elf* ".symtab")
    (setf *symbols* (elf:data (elf:named-section *elf* ".symtab"))))
  (when (elf:named-section *elf* ".debug_info")
    (setf *debug-infos* (dw-get-debug-info *elf* *file* (elf:sh (elf:named-section *elf* ".debug_info")))))
  (setf *all-files* (stable-sort (get-dw-all-files *debug-infos*)  #'string< ))
  nil)

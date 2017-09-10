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

(defun get-all-file-symbols (tags)
  "get all soruce file global symbols,
tag: list of dwarf tag
==> \(file-name \( elf-symbol * \) \)"
  (flet ((find-symbols (compile-unit  types symbols)
           (declare (optimize debug))
           (let ((res nil))
             (dolist (entry (die-children compile-unit))
               (when (find (die-tag entry) types)
                 (ecase (die-tag entry)
                   (dw_tag_subprogram
                    (with-dw-att-name
                        ( dw_at_name  DW_AT_high_pc  DW_AT_low_pc) (die-attr entry)
                      (when dw_at_high_pc
                        (let* ((public-sym (find dw_at_name  symbols
                                                 :key #'elf:sym-name :test #'string=)))
                          (if public-sym
                              (push public-sym res)
                              (let ((sym (make-instance 'elf::elf-sym-32)))
                                (setf (elf:value sym) dw_at_low_pc )
                                (setf (elf:size sym) (- dw_at_high_pc dw_at_low_pc) )
                                (setf (elf:info sym) 2)
                                (setf (elf:sym-name sym) dw_at_name)
                                (if (>  (elf:size sym) 0)
                                    (push sym res))))))))
                   (dw_tag_variable
                    (with-dw-att-name
                        (dw_at_name DW_AT_location) (die-attr entry)
                      (when (> (length dw_at_location) 2)
                        (push (find dw_at_name  symbols
                                    :key #'elf:sym-name :test #'string=)
                              res))))
                   (others nil))))
             (with-dw-att-name (dw_at_name) (die-attr compile-unit)
               (list dw_at_name (remove-if #'null  res )))))
         (get-dw-all-files (debug-info)
           (let ((all (loop for unit in debug-info
                         collect (with-dw-att-name (dw_at_name)
                                     (die-attr unit)
                                   dw_at_name))))
             (delete-duplicates all :test #'string=)))
         (find-by-name (name infos)
           (loop for unit in infos
              when (string= name (car unit))
              nconc (cadr unit))))
    (let* ((symbols (elf:data (elf:named-section *elf* ".symtab")))
           (syms (mapcar #'(lambda (compile-unit)
                             (funcall #'find-symbols compile-unit tags symbols))
                         *debug-infos*))
           (all-files (stable-sort (get-dw-all-files *debug-infos*)  #'string< )))
      (mapcar #'(lambda (f)
                  (list f (find-by-name f syms)))
              all-files))))

(defun read-elf (file)
  "read elf file content
file: the full path of elf file"
  (setf *elf* (elf:read-elf file))
  (when (elf:named-section *elf* ".debug_info")
    (setf *debug-infos* (dw-get-debug-info *elf*  file  (elf:sh (elf:named-section *elf* ".debug_info")))))
  nil)

;;;; elf-parser.lisp

(in-package :elf-parser)

(defun package-symbol-list  (package)
  (with-output-to-string (s)
    (let  ((*standard-output* s))
      (do-external-symbols (s package)
        (print s)))))

;;; "elf-parser" goes here. Hacks and glory await!
(defun show-static-symbols ()
  "Show all symbols in ELF in a manner similar to readelf."
  (let ((symtab (elf:named-section *elf* ".symtab")))
    (dolist (tab (list  symtab))
      (format t "~&~%Symbol table '~a' contains ~d entries:~%"
              (elf:name tab) (length (elf:data tab)))
      (format t "   Num:    Value  Size Type     Bind  Name~%")
      (let ((symtab (stable-sort (copy-list (elf:data tab))
                                 #'< :key #'(lambda (sym) (elf:value sym)))))
        (loop for sym in symtab  as i from 0
           do (unless (zerop (elf:size sym))
                (format t "~6d: ~8x ~5d ~8a ~6a ~a~%"
                        i (elf:value sym) (elf:size sym) (elf:type sym)
                        (elf:binding sym)
                        (elf:sym-name sym))))))))

(defun show-file-layout ()
  "Show the layout of the elements of an elf file with binary offset."
  (let* ((elf *elf*)
         (layout
          (mapcar (metabang-bind:lambda-bind
                   ((offset size data))
                   (list offset
                         ;; an identifier for the section data
                         (cond
                           ((numberp data) (elf:name (nth data (elf:sections elf))))
                           ((stringp data) data)
                           ((vectorp data) :filler)
                           (t data))
                         ;; the size in the file
                         (let ((sec (cond
                                      ((numberp data)(nth data (elf:sections elf)))
                                      ((stringp data) (elf:named-section elf data))
                                      (t nil))))
                           (+ offset (if (and sec (equal :nobits (elf:type sec)))
                                         0
                                         size)))))
                  (elf:ordering elf))))
    (format t "~:{~&~x   ~18a ~x~}~%" (cons (list 'offset 'contents 'end)
                                            layout))))

(defun show-memory-layout ()
  "Show the layout of the elements of an elf file with binary offset."
  (format t "~&addr     contents          end        size ~%")
  (format t "--------------------------------------------~%")
  (with-slots (elf:sections elf:section-table elf:program-table) *elf*
    (mapc
     #'(lambda (trio)
         (metabang-bind:bind (((beg size name) trio))
           (format t "~&0x~x ~18a 0x~x  ~D  ~%" beg name (+ beg size) size )))
     (stable-sort
      (remove-if
       #'(lambda (trio) (zerop (second trio)))
       (append
        (mapcar #'(lambda (head)
                    (list (elf:vaddr head) (elf:memsz head) (elf:type head)))
                elf:program-table)
        (when elf:section-table
          (mapcar #'(lambda (sec) (list (elf:address (elf:sh sec)) (elf:size sec) (elf:name sec)))
                  elf:sections))))
      #'< :key #'car)))
  nil)


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
returns: list compile-unit's dw_at_name  list elf-symbol "
  (declare (optimize debug))
  (let ((res nil))
    (dolist (entry (die-children compile-unit))
      (when (find (die-tag entry) types)
        (case (die-tag entry)
          ('dw_tag_subprogram
           (with-dw-att-name
               ( dw_at_name  DW_AT_high_pc ) (die-attr entry)
             (when dw_at_high_pc
               (progn
                 (push (find dw_at_name  *symbols*
                             :key #'elf:sym-name :test #'string=)
                       res)))))
          ('dw_tag_variable
           (with-dw-att-name
               (dw_at_name DW_AT_location) (die-attr entry)
             (when (> (length dw_at_location) 2)
               (progn
                 (push (find dw_at_name  *symbols*
                             :key #'elf:sym-name :test #'string=)
                       res)))))
          (others nil))))
    (with-dw-att-name (dw_at_name) (die-attr compile-unit)
      (list dw_at_name (remove-if #'null  res )))))

#+ (or)
(defun find-symbols111 (compile-unit)
  "find compile unit's symbol in elf symbols tab section"
  (declare (optimize debug))
  (let ((res nil))
    (dolist (entry (caddr compile-unit))
      (let ((obj (case (car entry)
                   ('dw_tag_subprogram
                    (with-dw-att-name
                        ( dw_at_name DW_AT_low_pc DW_AT_high_pc )
                        (cadr  entry)
                      (when dw_at_high_pc
                        (list dw_at_name (- dw_at_high_pc dw_at_low_pc)))))
                   (others nil))))
        (when obj (push obj res))))
    (with-dw-att-name
        (dw_at_name dw_at_comp_dir DW_AT_low_pc DW_AT_high_pc )
        (cadr compile-unit)
      (list (if  dw_at_high_pc
                 (list dw_at_name (- dw_at_high_pc dw_at_low_pc))
                 (list dw_at_name ))
            res))))

(defparameter *elf* nil)
(defparameter *file* nil)
(defparameter *symbols* nil)

(defun read-elf (file)
  "read elf file content"
  (setf *elf* (elf:read-elf file)
        *file* file)
  (setf *symbols* (elf:data (elf:named-section *elf* ".symtab")))
  nil)


(defun dump-symbol-list (sym-list)
  (loop for sym in sym-list
     do (format t "~&    ~8x ~5d ~8a ~6a ~a~%"
                (elf:value sym) (elf:size sym) (elf:type sym)
                (elf:binding sym)
                (elf:sym-name sym))))


(defun show-debug-symbols ()
  (declare (optimize debug))
  (let* ((infos (dw-get-debug-info *elf* *file*))
         (symbols (mapcar #'(lambda (x)
                              (funcall #'find-symbols x '(dw_tag_subprogram
                                                          dw_tag_variable)))
                          infos)))
    (mapc #'(lambda (l)
              (format t "~a~%" (car l))
              (dump-symbol-list (cadr l)))
          symbols)
    nil))



(defun help ()
  (format t "read-elf file-path ~% show-debug-symbols"))


(defun main ()
  (if (= (length sb-ext:*posix-argv*) 2)
      (let ((file  (nth 1 sb-ext:*posix-argv*)))
        (format t "~a" file)
        (read-elf file)
        (show-debug-symbols))
      (loop for s in sb-ext:*posix-argv*
         do (format t "~&~a~%" s))))

#+ (or)
(sb-ext:save-lisp-and-die #p "elf-parser" :toplevel #'main :executable t)

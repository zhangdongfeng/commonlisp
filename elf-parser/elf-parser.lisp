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

(defparameter *elf* nil)
(defparameter *file* nil)
(defparameter *symbols* nil)
(defparameter *debug-infos* nil)
(defparameter *all-files* nil)

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
  (setf *symbols* (elf:data (elf:named-section *elf* ".symtab")))
  (setf *debug-infos* (dw-get-debug-info *elf* *file*))
  (setf *all-files* (stable-sort (get-dw-all-files *debug-infos*)  #'string< ))
  nil)


(defun dump-symbol-list (sym-list)
  (loop for sym in sym-list
     do (format t "~&    ~8x ~5d ~8a ~6a ~a~%"
                (elf:value sym) (elf:size sym) (elf:type sym)
                (elf:binding sym)
                (elf:sym-name sym))))

(defun get-all-file-symbols (tags)
  "get all soruce file global symbols,
tag: list of dwarf tag
==> \(file-name \("
  (let* ((syms (mapcar #'(lambda (compile-unit)
                           (funcall #'find-symbols compile-unit tags))
                       *debug-infos*)))
    (mapcar #'(lambda (f)
                (list f (find-by-name f syms)))
            *all-files*)))

(defun find-by-name (name infos)
  "collect all debug info symbols of the same file
name: file path
info: dwarf debug info
==>list elf-symblos"
  (loop for unit in infos
     when (string= name (car unit))
     nconc (cadr unit)))

(defun rodata-sym? (sym)
  (let* ((rodata (elf:sh (elf:named-section *elf* "rodata")))
         (rodata-start (elf:address rodata))
         (rodata-end (+ rodata-start (elf:size rodata))))
    (and (> (elf:value sym) rodata-start)
         (< (elf:value sym) rodata-end))))

(defun filter-rodata (pred file-sym)
  (list (car file-sym)
        (remove-if-not pred (cadr file-sym))))

(defun show-debug-symbols (&optional &key (tag :data)
                                       (threshold 0) (dump-symbol nil) (path nil)  (rodata :no))
  "show  debug symbols info in dwarf .debug_info section
tags: optional, default is '(dw_tag_subprogram  dw_tag_variable)
threshod: optional, the threshold size to dump info
dump-symblo: should also dump sysmbols
path: path filter
no-rodata: exclude rodata symbols"
  (declare (optimize debug))
  (let* ((total-size 0)
         (files (get-all-file-symbols (ecase tag
                                        (:code '(dw_tag_subprogram))
                                        (:data '(dw_tag_variable))
                                        (:all '(dw_tag_subprogram dw_tag_variable)))))
         (files (remove-if-not  #'(lambda (f) (if path  (search path (car f))  t))  files))
         (files (ecase rodata
                  (:no (mapcar
                        (alexandria:curry #'filter-rodata  (alexandria::compose #'not #'rodata-sym?))
                        files))
                  (:only (mapcar  (alexandria:curry #'filter-rodata #'rodata-sym?) files))
                  (:all files))))
    (mapc #'(lambda (f)
              (let* ((syms (cadr f))
                     (size (loop for s in syms
                              sum (elf:size s))))
                (setf total-size (+ total-size size))
                (if (> size threshold)
                    (progn
                      (format t "~&~a  ~d~%" (car f) size)
                      (if dump-symbol
                          (progn
                            (dump-symbol-list syms)
                            (format t "~%")))))))
          files)
    (format t "~&total size ~:d~%" total-size))
  nil)


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

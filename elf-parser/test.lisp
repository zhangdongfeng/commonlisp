(in-package :elf-parser)

(defun package-symbol-list  (package)
  (with-output-to-string (s)
    (let  ((*standard-output* s))
      (do-external-symbols (s package)
        (print s)))))

(defun all-memory-items ( elf-file )
  (with-slots (elf:sections elf:section-table elf:program-table) elf-file
    (stable-sort
     (remove-if
      #'(lambda (trio) (zerop (second trio)))
      (append (mapcar #'(lambda (head)  (list (elf:vaddr head)
                                          (elf:memsz head)
                                          (symbol-name (elf:type head))))
                      elf:program-table)
              (when elf:section-table
                (mapcar #'(lambda (sec)  (list (elf:address (elf:sh sec))
                                           (elf:size sec)
                                           (elf:name sec)))
                        elf:sections))))
     #'< :key #'car)))



(defun overlay-sym? (sym)
  (let* ((overlay-secs (remove-if-not
                        #'(lambda (sec) (search ".overlay." (elf:name sec))) (elf:sections *elf*)))
         (addr-pairs  (mapcar
                       #'(lambda (sec) (list (elf:address (elf:sh sec))
                                         (+ (elf:address (elf:sh sec)) (elf:size (elf:sh sec)))))    overlay-secs)))
    (find-if  #'(lambda (range) (and (>= (elf:value sym)  (car range))
                                 (<= (elf:value sym) (cadr range))))
              addr-pairs)))

(defun rodata-sym? (sym)
  (let* ((rodata (elf:sh (elf:named-section *elf* ".rodata")))
         (rodata-start (elf:address rodata))
         (rodata-end (+ rodata-start (elf:size rodata))))
    (and (>= (elf:value sym) rodata-start)
         (<= (elf:value sym) rodata-end))))

(defun sym-filter (pred file-sym)
  (list (car file-sym)
        (remove-if-not pred (cadr file-sym))))


(defun show-memory-sec ( names)
  "Show the layout of the elements of an elf file with binary offset."
  (format t "~&|addr|contents|size| ~%")
  (format t "|------------------~%")
  (mapc   #'(lambda (trio)
              (metabang-bind:bind (((beg size name) trio))
                                  (loop for n in names
                                        when (search n name)
                                        do (format t "~&|0x~x|~18a|~:D|~%" beg name  size ))))
          (all-memory-items *elf*))
  nil)

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
                        (alexandria:curry #'sym-filter  (alexandria::compose #'not #'rodata-sym?))
                        files))
                  (:only (mapcar  (alexandria:curry #'sym-filter #'rodata-sym?) files))
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

(defun show-debug-module-symbols (modules &optional &key
                                          (prefix "") (threshold 0) (dump-file nil) (dump-symbol nil))
  "show  debug symbols info in dwarf .debug_info section
threshod: optional, the threshold size to dump info
dump-file: should also dump file info
dump-symblo: should also dump sysmbols
path: path filter"
  (declare (optimize debug))
  (let* ((all-code-files (get-all-file-symbols '(dw_tag_subprogram)))
         (all-data-files (get-all-file-symbols '(dw_tag_variable))))
    (flet ((dump-code-data (module codes datas)
                           (flet ((sum-syms (f) (loop for s in (cadr f)  sum (elf:size s)))
                                  (dump-symbol-list (f)
                                                    (loop for sym in (cadr f)
                                                          do (format t "~&    ~8x ~5d ~8a ~6a ~a~%"
                                                                     (elf:value sym) (elf:size sym) (elf:type sym)
                                                                     (elf:binding sym)
                                                                     (elf:sym-name sym)))))
                             (let* ((module-rodatas (mapcar  (alexandria:curry #'sym-filter #'rodata-sym?) datas))
                                    (module-overlay-datas (mapcar  (alexandria:curry #'sym-filter #'overlay-sym?) datas))
                                    (module-datas (mapcar (alexandria:curry #'sym-filter
                                                                            #'(lambda (s) (and (not (rodata-sym? s))
                                                                                           (not (overlay-sym? s)))))
                                                          datas)))
                               (flet ((dump-file-info(f)
                                                     (let* ((name (car f))
                                                            (data-file (find-if #'(lambda (x) (string= (car f) (car x))) module-datas))
                                                            (rodata-file (find-if #'(lambda (x) (string= (car f) (car x))) module-rodatas))
                                                            (overlay-file (find-if #'(lambda (x) (string= (car f) (car x))) module-overlay-datas))
                                                            (short-name (if (search prefix name) (subseq name  (length prefix)) name)))
                                                       (format t "~&| ~a~{~T|~:D~}|~%" short-name
                                                               (mapcar #'sum-syms (list  f  data-file rodata-file overlay-file)))
                                                       (when dump-symbol (mapc #'dump-symbol-list
                                                                               (list  f  data-file rodata-file overlay-file))))))
                                 (format t "~&|~18a~{~T|~D~}|~%"  module
                                         (mapcar #'(lambda (files)
                                                     (loop for f in files sum (sum-syms f)))
                                                 (list codes module-datas module-rodatas module-overlay-datas)))
                                 (when dump-file (format t "~&**** module details:~%")
                                       (format t "~&| file name |code size|data size|rodata size| overlay-data-size|~%")
                                       (mapc #'dump-file-info   codes)))))))
      (flet ((dump-module (m)
                          (let* ((module-codes (remove-if-not  #'(lambda (f) (search m  (car f)))  all-code-files))
                                 (datas (remove-if-not  #'(lambda (f) (search m  (car f)))  all-data-files)))
                            (dump-code-data m module-codes datas)
                            (setq all-code-files (remove-if  #'(lambda (f) (search m  (car f)))  all-code-files)
                                  all-data-files (remove-if #'(lambda (f) (search m  (car f)))  all-data-files)))))
        (format t "~&*** module mem :~%")
        (format t "~&| module name |code size|data size|rodata size|overlay data size|~%")
        (mapc #'dump-module modules)
        (dump-code-data "others" all-code-files all-data-files)
        ))))



(defun help ()
  (format t "usage:
elf-parser   elf-file   [module=xxx]  [prefix=xxxx]   [dump-file]   [dump-symbol]
elf-file:   the elf-file with dwarf debug info,
module:   the path fragment to search,  seperated by space
prefix:  when dump file details, ignore the path prefix
dump-file:  dump file info details
dump-symbol:  dump  symbols details in file
"))

(defun main ()
  (if (>= (length sb-ext:*posix-argv*) 2)
      (flet ((get-option (key)
               (cadr (remove-if #'(lambda (s) (string= s ""))
                                (uiop/utility:split-string
                                 (find-if #'(lambda (arg) (search key  arg)) sb-ext:*posix-argv* )
                                 :separator "=")))))
        (let*  ((file  (nth 1 sb-ext:*posix-argv*))
                (restarg (cddr sb-ext:*posix-argv*))
                (prefix  (get-option "prefix="))
                (dump-file (find-if #'(lambda (arg) (search "dump-file" arg)) restarg))
                (dump-symbol (find-if #'(lambda (arg) (search "dump-symbol" arg)) restarg))
                (modules (remove-if #'(lambda (s) (string= s ""))
                                    (uiop/utility:split-string  (get-option "module=")))))
          (format t "~a" file)
          (loop for s in sb-ext:*posix-argv*
             do (format t "~&~a~%" s))
          (read-elf file)
          (when *elf*
            (show-memory-sec '( "LOAD" "text" "rodata" "datas" "bss" "noinit"))
            (show-debug-module-symbols
             modules
             :prefix prefix
             :dump-symbol dump-symbol
             :dump-file dump-file))))
      (help)))

#+(or)
(sb-ext:save-lisp-and-die #p "elf-parser" :toplevel #'main :executable t)

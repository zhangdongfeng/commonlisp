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

(defun overlay-sym? (sym)
  (let* ((overlay-secs (remove-if-not
                        #'(lambda (sec) (search ".overlay." (elf:name sec))) (elf:sections *elf*)))
         (addr-pairs  (mapcar
                       #'(lambda (sec) (list (elf:address (elf:sh sec))
                                         (+ (elf:address (elf:sh sec)) (elf:size (elf:sh sec)))))    overlay-secs)))
    (find-if  #'(lambda (range) (and (>= (elf:value sym)  (car range))
                                 (<= (elf:value sym) (cadr range))))
              addr-pairs)))

(defun sym-filter (pred file-sym)
  (list (car file-sym)
        (remove-if-not pred (cadr file-sym))))


(defun show-debug-module-symbols ( modules &optional &key
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
        ;;(dump-code-data "others" all-code-files all-data-files)
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

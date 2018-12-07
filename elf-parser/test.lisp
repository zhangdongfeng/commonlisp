(in-package :elf-parser)

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

(defun show-static-symbols-test ()
  "Show all symbols in ELF in a manner similar to readelf."
  (let ((symtab (elf:named-section *elf* ".symtab")))
    (dolist (tab (list  symtab))
      (format t "~&~%Symbol table '~a' contains ~d entries:~%"
              (elf:name tab) (length (elf:data tab)))
      (format t "   Num:    Value  Size Type     Bind  Name~%")
      (let ((symtab (stable-sort (copy-list (elf:data tab))
                                 #'string<  :key #'(lambda (sym) (elf:sym-name sym)))))
        (loop for sym in symtab  as i from 0
           do (unless (zerop (elf:size sym))
                (format t "~a~%"
                        (elf:sym-name sym)
                        )))
        (loop for sym in symtab  as i from 0
           do (unless (zerop (elf:size sym))
                (format t "~a~%"
                        (elf:size sym))))))))

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
  (format t "~&addr     contents                  size ~%")
  (format t "--------------------------------------------~%")
  (with-slots (elf:sections elf:section-table elf:program-table) *elf*
    (mapc
     #'(lambda (trio)
         (metabang-bind:bind (((beg size name) trio))
           (format t "~&0x~x ~18a   ~D  ~%" beg name  size )))
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

(defun overlay-sym? (file-sym)
  (let* ((sym (elf-sym file-sym))
         (overlay-secs (remove-if-not
                        #'(lambda (sec) (search ".overlay." (elf:name sec))) (elf:sections *elf*)))
         (addr-pairs  (mapcar
                       #'(lambda (sec) (list (elf:address (elf:sh sec))
                                        (+ (elf:address (elf:sh sec)) (elf:size (elf:sh sec)))))    overlay-secs)))
    (find-if  #'(lambda (range) (and (>= (elf:value sym)  (car range))
                                (<= (elf:value sym) (cadr range))))
              addr-pairs)))

(defparameter *rodata-start* 0)
(defparameter *rodata-end* 0)
(defun rodata-sym? (file-sym)
  (let ((sym (elf-sym file-sym)))
    (when (elf:named-section *elf* "rodata")
      (let* ((rodata (elf:sh (elf:named-section *elf* "rodata")))
             (rodata-start (elf:address rodata))
             (rodata-end (+ rodata-start (elf:size rodata))))
        (setq  *rodata-start*  rodata-start
               *rodata-end* rodata-end)))
    (let ((rodata-start *rodata-start*)
          (rodata-end *rodata-end*))
      (and
       (eql :object (elf:type (elf-sym file-sym)))
       (>= (elf:value sym) rodata-start)
       (<= (elf:value sym) rodata-end)))))

(defun code-sym? (file-sym)
  (eql :func (elf:type (elf-sym file-sym))))

(defun data-sym?  (file-sym)
  (and
   (eql :object (elf:type (elf-sym file-sym)))
   (not (rodata-sym? file-sym))
   (not (overlay-sym? file-sym))))


(defun calc-size (syms)
  (flet ((sum-syms (syms)
           (loop for s in syms
              sum (elf:size (elf-sym s)))))
    (let* ((code (remove-if-not  #'code-sym? syms))
           (data (remove-if-not  #'data-sym? syms))
           (rodata (remove-if-not  #'rodata-sym?  syms) )
           (overlay (remove-if-not  #'overlay-sym?  syms)))
      (let ((c (sum-syms code))
            (r (sum-syms rodata) )
            (o (sum-syms overlay) )
            (d (sum-syms data)))
        (values c d  r o)))))

(defun find-module-symbols  (module  all-syms)
  (remove-if-not #'(lambda (x )
                     (search   module  (file-name x))) all-syms))

(defun remove-module-symbols  (module all-syms)
  (remove-if #'(lambda (x )
                 (search   module  (file-name x))) all-syms))

(defun dump-file-syms  (sym-list &key (dump-code-symbol nil)
                                   (dump-data-symbol nil)
                                   (dump-rodata-symbol nil)
                                   (dump-overlay-symbol nil)
                                   &allow-other-keys)
  (flet ((dump-sym (file-sym)
           (let ((sym (elf-sym file-sym)))
             (unless   (zerop (elf:size sym))
               (format t "~&    ~8x ~5d ~8a ~6a ~a~%"
                       (elf:value sym) (elf:size sym) (elf:type sym)
                       (elf:binding sym)
                       (elf:sym-name sym))))))
    (loop for sym in sym-list
       do (cond
            ((and  (rodata-sym? sym) dump-rodata-symbol) (dump-sym sym))
            ((and  (data-sym? sym) dump-data-symbol) (dump-sym sym))
            ((and  (code-sym? sym) dump-code-symbol) (dump-sym sym))
            ((and  (overlay-sym? sym) dump-overlay-symbol) (dump-sym sym))))))

(defun dump-module-file (module-syms &rest rest  &key (prefix "")  &allow-other-keys)
  (flet ((get-all-filename (syms)
           (let* ((all-path (mapcar #'file-name  syms)))
             (delete-duplicates all-path :test #'string=))))
    (let* ((file-name (get-all-filename module-syms)))
      (format t "~&**** module details:~%")
      (format t "~&| file name |code size|data size|rodata size| overlay-data-size|~%")
      (dolist (f file-name)
        (let* ((syms  (remove-if-not #'(lambda (s) (string= f (file-name s))) module-syms))
               (short-name (if (search prefix f) (subseq f (length prefix)) f)))
          (format t "~&| ~a~{~T|~D~}|~%"
                  short-name  (multiple-value-list (calc-size syms)))
          (apply #'dump-file-syms syms rest))))))

(defun show-debug-module-symbols
    (modules &rest rest &key (dump-file nil)
                          (prefix "")   (dump-symbol nil)
                          dump-code-symbol
                          dump-data-symbol
                          dump-overlay-symbol
                          dump-rodata-symbol)
  "show  debug symbols info in dwarf .debug_info section
threshod: optional, the threshold size to dump info
dump-file: should also dump file info
dump-symblo: should also dump sysmbols
prefix: path filter"
  (declare (optimize debug))
  (let ((all-syms *all-symbols* ))
    (flet ((dump-module (m)
             (let* ((module-syms  (find-module-symbols  m all-syms)))
               (format t "~&|~18a~{~T|~:D~}|~%"  m
                       (multiple-value-list (calc-size module-syms)))
               (if dump-file
                   (apply  #'dump-module-file module-syms rest))
               (setq all-syms (remove-module-symbols m all-syms)))))
      (format t "~&*** module mem :~%")
      (format t "~&| module name |code size|data size|rodata size|overlay data size|~%")
      (mapc #'dump-module modules)
      (format t "~&|~18a~{~T|~:D~}|~%"  "others"
              (multiple-value-list (calc-size all-syms)))
      (if dump-file
          (apply  #'dump-module-file all-syms rest)))))



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
            (show-debug-module-symbols
             modules
             :prefix prefix
             :dump-symbol dump-symbol
             :dump-file dump-file))))
      (help)))

#+(or)
(sb-ext:save-lisp-and-die #p "elf-parser" :toplevel #'main :executable t)

#+ (or)
(show-debug-module-symbols '(
                             "libsystem/service"
                             "mqtt/"
                             "libsystem/library"
                             "actions/porting/"
                             "wifi/host"
                             "subsys/bluetooth"
                             "/drivers/wifi/"
                             "drivers"
                             "subsys/net"
                             "/samples/storyapp/"
                             "/kernel/"
                             "nofile")
                           :dump-file nil
                           :dump-data-symbol nil
                           :dump-overlay-symbol t
                           :prefix "/home/local/ACTIONS/zhangdf/sdk/GL5118_WIFI")

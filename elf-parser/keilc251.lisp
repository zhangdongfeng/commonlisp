(in-package :elf-parser)

(defparameter %memory-class% "[0-9A-Z_]+\\b")
(defparameter %data-addr-range% "D?:?0X[0-9A-F]{2,8}-D?:?0X[0-9A-F]{2,8}\\b")
(defparameter %data-addr% "D?:?[0-9A-F]{2,8}H")
(defparameter %data-length% %data-addr%)
(defparameter %align%  "BYTE|ALN 1|ALN 2")
(defparameter %symbol-type%  "---|FAR LAB|BIT|DWORD|WORD|BYTE")
(defparameter %reloc%  "INSEG|UNIT|AT..")
(defparameter %section-name%  "[0-9A-Z_*?]+")
(defparameter %symbol-name% "[0-9A-Za-z_*?]+")

#+or
(with-open-file (f #p "/Users/zhangdongfeng/Downloads/airaha/AB1520S_SVN72747_Headset_OBJ/output/AB1520S/Release_Flash/BTStereoHeadset_AB1520S_FlashLinkRom.MAP1"  :direction :output :if-exists :overwrite :if-does-not-exist :create)
  (loop for line in *lines*
     do (write-line line  f)))

(defparameter *blank-line-regex*  "^\\x0d*$")
(defparameter *line-wrap*  "\\x0d*\\x0a>> ")
(defparameter *invoke-line* "\\bINVOKED BY:")
(defun pre-process (path)
  (flet ((read-file-into-lines (path)
           (with-open-file (f path )
             (loop for line = (read-line f  nil nil)
                while  line  collect line)))
         (remove-tricky-lines (lines)
           (remove-if #' (lambda (line)
                           (or (and (scan "L251 LINKER" line)
                                    (scan "\\bPAGE\\b" line))
                               (scan *blank-line-regex* line)
                               (scan "^\\s+-\\S+\\s*$" line)
                               (scan "---      \\*\\*GAP\\*\\*" line )
                               (scan "\\*SFR\\* " line )))
                         lines))
         (remove-line-warp (str)
           (regex-replace-all *line-wrap* str ""  )))
    (let ((proc (compose #'string-to-lines
                         #'remove-line-warp
                         #'lines-to-string
                         #'remove-tricky-lines
                         #'read-file-into-lines)))
      (setq *lines* (funcall proc path)))))

(defparameter *keil-file* #p "/Users/zhangdongfeng/Downloads/airaha/AB1520S_SVN72747_Headset_OBJ/output/AB1520S/Release_Flash/BTStereoHeadset_AB1520S_FlashLinkRom.MAP")
(defparameter *lines* nil)
(defparameter *merge-publics* nil)
(defparameter  *overlay* nil)
(defparameter  *input-modules* nil)
(defparameter *memory-map* nil)
(defparameter *symbols* nil)


(defparameter *invoke-str* nil)
(defparameter *module-str* nil)
(defparameter *memory-str* nil)
(defparameter *memory-class* nil)
(defparameter *memory-map-str* nil)
(defparameter *symbol-str* nil)
(defparameter *symbol-table-str* nil)
(defparameter *symbol-table* nil )


(defun parse (lines)
  (let ((str (lines-to-string lines)))
    (setq
     *invoke-str* (extract-by-marker
                   "INVOKED BY:"
                   "CPU MODE:"
                   str)
     *merge-publics* (parse-regex-spec-by-str
                      "\\bMERGEPUBLICS CLASSES\\b"
                      `((repeat ,%memory-class% (repeat ,%data-addr-range%)))
                      *invoke-str*)
     *overlay* (parse-regex-spec-by-str
                "\\bOVERLAY\\b"
                `((repeat ,%memory-class% "\\s*!" (repeat ,%section-name%)))
                *invoke-str*)
     *module-str* (extract-by-marker
                   "INPUT MODULES INCLUDED:"
                   "ACTIVE MEMORY CLASSES OF MODULE"
                   str)
     *input-modules* (flet ((remove-brackets (m)
                              (list (car m) (subseq (cadr m) 1 (1- (length (cadr m)))))))
                       (mapcar #'remove-brackets (parse-regex-spec-by-line
                                                  "\\bINPUT MODULES INCLUDED\\b"
                                                  `("\\.\\\\[0-9A-Za-z_\\\\]+\\.[objLIB]{3}|C:\\\\[0-9A-Za-z_\\\\]+\\.LIB|\\.\\\\[0-9A-Za-z_\\\\]+"
                                                    "\\([0-9A-Za-z_?]+\\)")
                                                  *module-str*)))
     *memory-str* (extract-by-marker
                   "ACTIVE MEMORY CLASSES OF MODULE"
                   "MEMORY MAP OF MODULE:"
                   str)
     *memory-class* (flet ((parse (sym)
                             (let* ((addr (parse-integer (cadr sym) :radix 16 :junk-allowed t) )
                                    (size (parse-integer (cadddr sym) :radix 16 :junk-allowed t))
                                    (name (nth 4 sym)))
                               (list addr size  name))))
                      (let ((string-syms
                             (remove-if #'have-null-elements
                                        (parse-regex-spec-by-line
                                         "ACTIVE MEMORY CLASSES OF MODULE"
                                         `(,%data-addr% ,%data-addr%  ,%data-addr%  ,%data-length% ,%section-name% )
                                         *memory-str*))))
                        (stable-sort (mapcar #'parse string-syms) #'< :key #'car)))

     *memory-map-str*     (extract-by-marker
                           "MEMORY MAP OF MODULE:"
                           "PUBLIC SYMBOLS OF MODULE:"
                           str)
     *memory-map* (flet ((parse-map (sym)
                           (let* ((addr (parse-integer (car sym) :radix 16 :junk-allowed t) )
                                  (size (parse-integer (caddr sym) :radix 16 :junk-allowed t))
                                  (memory (nth 5  sym))
                                  (name (nth 6 sym))
                                  (reg-group (nth-value 1
                                                        (scan-to-strings "\\?(\\w+)\\?*(\\w*)\\?*(\\w*)" name))))
                             (destructuring-bind (type class module)
                                 (coerce reg-group 'list)
                               (list addr size memory type class module name)))))
                    (let ((string-syms
                           (remove-if #'have-null-elements
                                      (parse-regex-spec-by-line
                                       "MEMORY MAP OF MODULE:"
                                       `(,%data-addr% ,%data-addr% ,%data-length%
                                                      ,%align% ,%reloc% ,%memory-class% ,%section-name% )
                                       *memory-map-str*))))
                      (stable-sort (mapcar #'parse-map string-syms) #'< :key #'car)))

     *symbol-str* (extract-by-marker
                   "PUBLIC SYMBOLS OF MODULE:"
                   "SYMBOL TABLE OF MODULE:"
                   str)

     *symbols* (flet ((parse-sym (sym)
                        (list
                         (parse-integer (car sym) :radix 16 :junk-allowed t)
                         (cadr sym)
                         (cadddr sym))))
                 (let ((string-syms
                        (remove-if #'have-null-elements
                                   (parse-regex-spec-by-line
                                    "PUBLIC SYMBOLS OF MODULE:"
                                    `(,%data-addr% ,%memory-class% ,%symbol-type%  ,%symbol-name% )
                                    *symbol-str*))))
                   (stable-sort (mapcar #'parse-sym string-syms) #'< :key #'car)))

     *symbol-table-str* (extract-by-marker
                         "SYMBOL TABLE OF MODULE:"
                         "Program Size: data="
                         str)

     *symbol-table* (flet ((collect-module-symbol-table (sym-tbl)
                             (loop with result = nil
                                with str =  sym-tbl
                                with res-str = nil
                                do (progn
                                     (multiple-value-bind (res  rest-str)
                                         (extract-by-marker
                                          "---         MODULE    ---      ---       "
                                          "---         MODULE    ---      ---       "
                                          str)
                                       (push res  result)
                                       (setq str rest-str
                                             res-str res)))
                                when (not res-str)
                                return (nreverse result))))
                      (mapcar #'parse-module-symbol-table (collect-module-symbol-table *symbol-table-str*))))
    (set-elf-symbol-size *symbol-table* )
    nil))

(defun collect-module-symbol-table (sym-tbl)
  (loop with result = nil
     with str =  sym-tbl
     with res-str = nil
     do (progn
          (multiple-value-bind (res  rest-str)
              (extract-by-marker
               "---         MODULE    ---      ---       "
               "---         MODULE    ---      ---       "
               str)
            (push res  result)
            (setq str rest-str
                  res-str res)))
     when (not res-str)
     return (nreverse result)))

(defun parse-module-symbol-table (module-str)
  (let* ((module-name (nth-value 1
                                 (scan-to-strings "---         MODULE    ---      ---       (\\S+)" module-str))))
    (if module-name
        (let* ((file-name (car (find (elt module-name 0 )  *input-modules* :key #'cadr :test #'string=)))
               (symbols (remove-if #'have-null-elements
                                   (parse-regex-spec-by-line
                                    "---         MODULE    ---      ---       "
                                    `(,%data-addr% "PUBLIC|SYMBOL" "ECODE|EDATA|HCONST"  "FAR LAB|---|BYTE|INT|WORD|DWORD"  ,%symbol-name% )
                                    module-str))))
          (pprint file-name)
          (list file-name
                (loop for sym-str in symbols
                   collect (let ((sym (make-instance 'elf::elf-sym-32)))
                             (setf (elf:value sym) (parse-integer (car sym-str) :radix 16 :junk-allowed t))
                             (setf (elf:info sym) (if (string= (caddr sym-str) "ECODE") 2 1))
                             (setf (elf:sym-name sym) (nth 4 sym-str))
                             sym)))))))

(defun set-elf-symbol-size (file-syms)
  (flet ((find-memory-class-by-addr (addr)
           (let ((result  (loop for m in *memory-class*
                             when (<= addr (+ (car m) (cadr m)))
                             return (+ (car m) (cadr m)))))
             (if result  result #xffffff))))
    (let* ((all-sym (flatten (loop for s in file-syms
                                collect (cadr s))))
           (sorted-sym (stable-sort all-sym #'<  :key #'elf:value)))
      (mapc #'(lambda (sym)
                (setf (elf:size sym) 0)) sorted-sym)
      (reduce #'(lambda (x y)
                  (setf (elf:size x) (min (- (elf:value y) (elf:value x))
                                          (- (find-memory-class-by-addr (elf:value x)) (elf:value x))))
                  y)
              (cdr sorted-sym)  :initial-value (car sorted-sym)))))

(defun  dump-symbols-by-addr (file-syms)
  (let* ((all-sym (flatten (loop for s in file-syms
                              collect (cadr s))))
         (sorted-sym (stable-sort all-sym #'<  :key #'elf:value)))
    (loop for sym in sorted-sym
       do (format t "~&    ~8x ~5d ~8a ~6a ~a~%"
                  (elf:value sym) (elf:size sym) (elf:type sym)
                  (elf:binding sym)
                  (elf:sym-name sym)))))

(defun read-map (map-file)
  (parse (pre-process map-file)))

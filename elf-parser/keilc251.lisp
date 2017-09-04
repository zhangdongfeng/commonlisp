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
     *input-modules* (parse-regex-spec-by-line
                      "\\bINPUT MODULES INCLUDED\\b"
                      `("\\.\\\\[0-9A-Za-z_\\\\]+\\.[objLIB]{3}|C:\\\\[0-9A-Za-z_\\\\]+\\.LIB|\\.\\\\[0-9A-Za-z_\\\\]+"
                        "\\([0-9A-Za-z_?]+\\)")
                      *module-str*)
     *memory-str* (extract-by-marker
                   "ACTIVE MEMORY CLASSES OF MODULE"
                   "MEMORY MAP OF MODULE:"
                   str)
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

     *symbol-table* (flet ((parse-sym (sym)
                             (list
                              (parse-integer (car sym) :radix 16 :junk-allowed t)
                              (cadr sym)
                              (cadddr sym))))
                      (let ((string-syms
                             (remove-if #'have-null-elements
                                        (parse-regex-spec-by-line
                                         "SYMBOL TABLE OF MODULE:"
                                         `(,%data-addr% ,%memory-class% ,%symbol-type%  ,%symbol-name% )
                                         *symbol-str*))))
                        (stable-sort (mapcar #'parse-sym string-syms) #'< :key #'car))))))


(defun parse-symbol-table (sym-tbl)
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


(defun read-map (map-file)
  (parse (pre-process map-file)))

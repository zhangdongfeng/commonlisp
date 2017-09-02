(in-package :elf-parser)


(defparameter *keil-file* #p "/Users/zhangdongfeng/Downloads/airaha/AB1520S_SVN72747_Headset_OBJ/output/AB1520S/Release_Flash/BTStereoHeadset_AB1520S_FlashLinkRom.MAP")

#+or
(with-open-file (f #p "/Users/zhangdongfeng/Downloads/airaha/AB1520S_SVN72747_Headset_OBJ/output/AB1520S/Release_Flash/BTStereoHeadset_AB1520S_FlashLinkRom.MAP1"  :direction :output :if-exists :overwrite :if-does-not-exist :create)
  (loop for line in *lines*
     do (write-line line  f)))

(defparameter *blank-line-regex*  "^\\x0d*$")
(defparameter *line-wrap*  "\\x0d*\\x0a>> ")
(defparameter *invoke-line* "\\bINVOKED BY:")


(defparameter *lines* nil)
(defparameter *merge-publics* nil)
(defparameter %section-name% "^\\b[0-9A-Z_]+\\b")
(defparameter %data-addr-range% "^\\bD?:?0X[0-9A-F]{2,8}-D?:?0X[0-9A-F]{2,8}\\b")
(defparameter  *overlay* nil)
(defparameter %symbol-name% "^\\b[0-9A-Z_*?]+")

(defun parse-linker-invoke-line (invoke-str)
  (let ((str invoke-str))
    (multiple-value-setq (*merge-publics*  str)
      (parse-regex-spec-by-str "\\bMERGEPUBLICS CLASSES\\b"
                               `((repeat ,%section-name% (repeat ,%data-addr-range%)))
                               str))
    (multiple-value-setq (*overlay*  str)
      (parse-regex-spec-by-str "\\bOVERLAY\\b"
                               `((repeat ,%section-name% "\\s*!" (repeat ,%symbol-name%)))
                               str))
    str))



(defparameter  *input-modules* nil)



(defun parse-input-modules (module-str)
  (setq *input-modules* (parse-regex-spec-by-line
                         "\\bINPUT MODULES INCLUDED\\b"
                         `("\\.\\\\[0-9A-Za-z_\\\\]+|C:\\\\[0-9A-Za-z_\\\\]+\\.LIB"
                           "\\([0-9A-Za-z_?]+\\)")
                         module-str)))



(defparameter *invoke-str* nil)
(defparameter *module-str* nil)
(defparameter *memory-str* nil)
(defparameter *memory-map-str* nil)
(defparameter *symbol-str* nil)
(defparameter *symbol-table-str* nil)


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
      (setq *lines* (funcall proc path))
      nil)))

(defun parse-lines (map-lines)
  (flet ((collect-linker-invoke-line (lines)
           (when (scan *invoke-line* (car lines))
             (progn
               (setq *invoke-str* (cadr lines))
               (parse-linker-invoke-line (cadr lines))
               (cddr lines))))
         (collect-input-modules (lines)
           (let ((modules (nthcdr  4  lines)))
             (loop with result = (list (nth 3 lines))
                for m on modules
                when (scan "^  " (car m))
                do (push (car  m) result)
                else  do
                  (let ((lines (nreverse result)))
                    (setq *module-str* (lines-to-string lines)))
                and return m)))
         (collect-memory-class (lines)
           (let ((modules (nthcdr  3  lines)))
             (loop
                with result = nil
                for m on modules
                when (scan "^\\d{6}H\\.?\\d?\\b" (car m) )
                do (push (car  m) result)
                else when (scan "^\\s+\\d{6}H" (car m))
                do (format t (car m))
                else do (let ((lines (nreverse result)))
                          (setq *memory-str* (lines-to-string lines)))
                and return m)))
         (collect-memory-map-module (lines)
           (let ((modules (nthcdr  3  lines)))
             (loop
                with result = nil
                for m on modules
                when (scan "^[0-9A-F]{6}H\\.?\\d?\\b" (car m) )
                do (push (car  m) result)
                else do (let ((lines (nreverse result)))
                          (setq *memory-map-str* (lines-to-string lines)))
                and return m))  )
         (collect-module-symbols (lines)
           (let ((modules (nthcdr  3  lines)))
             (loop
                with result = nil
                for m on modules
                when (scan "^\\s+[0-9A-F]{8}H\\b" (car m) )
                do (push (car  m) result)
                else do (let ((lines (nreverse result)))
                          (setq *symbol-str* (lines-to-string lines)))
                and return m)))
         (collect-module-symbols-table (lines)
           (let ((modules (nthcdr  3  lines)))
             (loop for m on modules
                unless (scan "^      \\S+" (car m) )
                return m))))
    (let ((proc (compose
                 #'collect-module-symbols-table
                 #'collect-module-symbols
                 #'collect-memory-map-module
                 #'collect-memory-class
                 #'collect-input-modules
                 #'collect-linker-invoke-line)))
      (funcall proc map-lines))))

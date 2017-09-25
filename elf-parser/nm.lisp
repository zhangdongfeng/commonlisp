(in-package :elf-parser)

(defparameter *nm-file* #p "/Users/zhangdongfeng/Downloads/WLAN_LIB/wlanlib27_nm.txt")

(defparameter *sdio-nm-file* #p "/Users/zhangdongfeng/Downloads/00005078-SDK-patch_RTL8189FTV_v02.6_v12728_SDIO/release-v02.6/component/common/drivers/wlan/realtek/wlan_lib/cortex-m/mdk/sdio_nm.lst")



(defun nm-pre-process (path)
  (flet ((read-file-into-lines (path)
           (with-open-file (f path )
             (loop for line = (read-line f  nil nil)
                while  line  collect line)))
         (remove-tricky-lines (lines)
           (remove-if #' (lambda (line)
                           (or
                            (scan *blank-line-regex* line)
                            (scan "^00000000 N __ARM_grp" line)
                            (scan "---      \\*\\*GAP\\*\\*" line )
                            (scan "\\*SFR\\* " line )))
                         lines)))
    (let ((proc (compose #'string-to-lines
                         #'lines-to-string
                         #'remove-tricky-lines
                         #'read-file-into-lines)))
      (setq *lines* (funcall proc path)))))

(defparameter *undefine-symbols* nil)
(defparameter *define-symbols* nil)



(defun nm-all-defined-symbos (modules)
  (flet ((nm-parse-sym (str)
           (let ((syms (remove-if #'have-null-elements
                                  (parse-regex-spec-by-line
                                   (concatenate 'string %obj-file-name% ":")
                                   `("\\S+" "B|R|D|T|t"  ,%symbol-name% )  str))))
             (mapcar #'caddr syms))))
    (flatten (mapcar #'nm-parse-sym modules))))

(defun nm-all-undefined-symbos (modules)
  (flet ((nm-parse-sym (str)
           (let ((syms (remove-if #'have-null-elements
                                  (parse-regex-spec-by-line
                                   (concatenate 'string %obj-file-name% ":")
                                   `( "U"  ,%symbol-name% )  str))))
             (mapcar #'cadr syms))))
    (flatten (mapcar #'nm-parse-sym modules))))



(defun nm-collect-module (sym-tbl)
  (loop with result = nil
     with str =  sym-tbl
     with res-str = nil
     do (progn
          (multiple-value-bind (res  rest-str)
              (extract-by-marker
               (concatenate 'string %obj-file-name% ":")
               (concatenate 'string %obj-file-name% ":")
               str)
            (push res  result)
            (setq str rest-str
                  res-str res)))
     when (not res-str)
     return (nreverse result)))

(defun nm-parse (path)
  (let* ((lines (nm-pre-process path))
         (modules (nm-collect-module (lines-to-string lines))))
    (setq
     *undefine-symbols* (nm-all-undefined-symbos modules)
     *define-symbols* (nm-all-defined-symbos modules))
    (delete-duplicates (stable-sort (remove-if #'(lambda (s) (find s *define-symbols* :test #'string=))
                                               *undefine-symbols*)
                                    #'string< )
                       :test #'string=)))


#+or
(with-open-file (f #p "/Users/zhangdongfeng/Downloads/00005078-SDK-patch_RTL8189FTV_v02.6_v12728_SDIO/release-v02.6/component/common/drivers/wlan/realtek/wlan_lib/cortex-m/mdk/wlan_nm1.lst"  :direction :output :if-exists :overwrite :if-does-not-exist :create)
  (loop for line in *lines*
     do (write-line line  f)))

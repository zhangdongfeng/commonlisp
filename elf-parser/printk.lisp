(in-package :elf-parser)

(defclass str-info ()
  ((name  :initarg :name
          :accessor  name)
   (line  :initarg :line
          :accessor  line)
   (file :initarg :file
         :accessor file)
   (line-str :initarg :line-str
             :accessor line-str)))


(defun parse-strs-from-file (f) 
  (let* ((file-str (read-file-into-string  f  :external-format :iso8859-1))
         (file-lines (string-to-lines file-str))
         (line-pos 0)
         (res nil))
    (dolist (l  file-lines)
      (setq line-pos (1+ line-pos))
      (register-groups-bind  (name) ("\\( ?\"([^\"]+)\"" l)
        (if name
            (push  (make-instance 'str-info :name name :line line-pos :file f :line-str l)
                   res))))
    (reverse res)))

(defun get-strs-from-cfiles (cfiles)
  (let* ((files (string-to-lines cfiles))
         (strs (mapcar #'parse-strs-from-file files)))
    (remove-duplicates
     (reduce  #'append strs)
     :key #'name
     :test #'string=)))

(defun get-str-infos-from(ro-sym-list)
  (let* ((str-syms (get-rodata-strs-from ro-sym-list))
         (strs (mapcar #'rodata-string-at str-syms))
         (strs (reduce #'append strs)))
    (mapcar #'(lambda (s)
                (make-instance 'str-info :name s))
            strs)))

(defclass rodata-sym ()
  ((name  :initarg :name
          :accessor  name)
   (size  :initarg :size
          :accessor  size)
   (addr  :initarg :addr
          :accessor  addr)
   (file  :initarg :file
          :accessor file)))

(defun rodata-string-at (rodata-sym)
  (declare (optimize debug))
  (let* ((rodata-start (elf:address(elf:sh (elf:named-section  *elf* "rodata"))))
         (rodatas (elf:data (elf:named-section  *elf* "rodata")))
         (str-start (- (addr rodata-sym) rodata-start))
         (str-end (+ str-start (size rodata-sym))))
    (labels ((string-at (offset)
               (if (>=  offset str-end)
                   nil
                   (let ((str (coerce (loop
                                         for i = offset then (1+ i)
                                         for ch = (aref rodatas i)
                                         until (zerop ch)
                                         collect (code-char ch))
                                      'string)))
                     (cons str (string-at (+ offset (length str) 1)))))))
      (string-at str-start))))

(defun get-ro-syms-from-mapfile (map-file)
  (let* ((file-str (read-file-into-string  map-file))
         (rodata-regex "(\\.rodata\\.[\\.A-Za-z0-9_]+)\\n? *0x([0-9a-f]+) +0x([0-9a-f]+) (.+)")
         (all-rodata (all-matches-as-strings rodata-regex  file-str ))
         (res nil))
    (dolist (r all-rodata)
      (register-groups-bind (name addr size file) (rodata-regex r)
        (let ((addr (parse-integer addr :radix 16))
              (size (parse-integer size :radix 16)))
          (if (= 0 addr) nil
              (push
               (make-instance 'rodata-sym
                              :name name
                              :addr addr
                              :size size
                              :file file)
               res)))))
    (reverse res)))

(defun get-rodata-strs-from(ro-sym-list)
  (remove-if-not #'(lambda (r)
                     (scan-to-strings "\\.str1\\." (name r)))
                 ro-sym-list))

(defun dump-ro-str (ro-sym)
  (register-groups-bind (func) ("\\.([^\\.]+)\\.str1\\." (name ro-sym))
    (format t "~&~%~a" (file ro-sym))
    (format t "~&~a ==>" func)
    (format t "~{~&~a~}" (rodata-string-at ro-sym))))

(defun show-mapfile-strings( map-file-path)
  (let* ((dir (directory-namestring map-file-path))
         (elf (concatenate 'string dir "zephyr.elf"))
         (ro-syms (get-ro-syms-from-mapfile map-file-path))
         (ro-strs (get-rodata-strs-from ro-syms))
         (ro-strs (sort ro-strs #'string< :key #'file)))
    (read-elf elf)
    (dolist (s ro-strs) (dump-ro-str s))))

"/home/local/ACTIONS/zhangdf/sdk/5120/zs283c/samples/actions_sdk_demo/ble_test/outdir/ats2837_evb/zephyr.map"


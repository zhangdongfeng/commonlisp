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
      (format t "~a~%" l)
      (register-groups-bind  (name) ("\\( ?\"([^\"]+)\"" l)
        (if name
            (push  (make-instance 'str-info :name name :line line-pos :file f :line-str l)
                   res))))
    (reverse res)) )

(defun get-strs-from-cfiles (cfiles)
  (let* ((files (string-to-lines cfiles))
         (strs (mapcar #'parse-strs-from-file files)))
    (reduce  #'append strs)))

(defclass rodata-str ()
  ((name  :initarg :name
          :accessor  name)
   (size  :initarg :size
          :accessor  size)
   (str :initarg :str
        :accessor str)))

(defun str-to-number (str)
  
  )

(defun get-strs-from-mapfile (map-file)
  (let* ((file-str (read-file-into-string  map-file))
         (rodata-regex "(\\.rodata\\.[\\.A-Za-z0-9_]+)\\n? *0x([0-9a-f])+ +0x([0-9a-f]+)")
         (all-rodata (all-matches-as-strings rodata-regex  file-str )))))


(defun show-printk-strs (cfiles-path  map-file-path)
  (get-strs-from-cfiles (read-file-into-string  cfiles-path))
  )


"\\.rodata\\.[A-Za-z0-9_]+\\n? *0x[0-9a-f]+ +0x[0-9a-f]+"

".rodata.rom_func_list_cache_init
0x0000000001078e28       0xf8 /home/local/ACTIONS/zhangdf/sdk/5120/ATS350B/ext/lib/actions/libabt/bt_drv/bt_drv_andes/libbtdrv.a(sys_api.o)"

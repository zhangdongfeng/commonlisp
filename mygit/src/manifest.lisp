(in-package :mygit)
;; blah blah blah.

;;(cl-fad:list-directory #p "/Volumes/tmp/tmp/AOSP")

(defparameter %name% "([0-9a-zA-Z/\\-_.]+)")

(defun list-have-elements (seq)
  (reduce  #'(lambda(x y)  (or x y))   seq ))

(defun list-have-required-elements (ele-index seq)
  (loop for  index in ele-index
     unless (nth  index seq) return nil
     finally (return t)))

(defun have-null-elements (seq)
  (not (reduce
        #'(lambda(x y)  (and x y))   seq )))

(defun parse-manifest (path spec func)
  (let (prjs)
    (setq prjs (mapcar (curry #'parse-regex-spec spec)
                       (read-file-into-lines path)))
    (setq prjs (mapcar func prjs))
    (setq prjs (remove-if-not #'list-have-elements prjs))
    (stable-sort prjs #'string< :key #'car)))

(defun get-mainfest-diff-wholepath (aosp-xml owl-xml)
  (set-difference (parse-manifest aosp-xml
                                  `( ,(concatenate 'string  "<project path=\"" %name%  "\"" )
                                      ,(concatenate 'string "name=\"" %name%  "\""))
                                  #'(lambda (prj) (list (cadr prj) (car prj))))
                  (parse-manifest owl-xml
                                  `( ,(concatenate 'string  "<project name=\"" %name%  "\"" )
                                      ,(concatenate 'string "path=\"" %name%  "\""))
                                  #'(lambda (prj)  prj))

                  :key #'(lambda (x)
                           (let ((prj (car x)))
                             (cond
                               ((scan "^android/" prj ) (subseq  prj (length "android/")))
                               (t prj)))) :test #'string= ))

(defparameter *dir-prefix* nil )

(defun rename-git-config (prj)
  (let ((file (cl-fad:merge-pathnames-as-file
               (make-repo-path *dir-prefix* prj) ".git/config")))
    (if (cl-fad:file-exists-p  file)
        (rename-file file "config_bak")
        (format t "~a does not exist ~%" file))))

(defun make-repo-path (prefix prj)
  (let* ((dir (cl-fad:pathname-as-directory (cadr prj))))
    (cl-fad:merge-pathnames-as-file prefix  dir)))

(defun rewrite-git-config (remote repo-prefix prj)
  (let ((file (cl-fad:merge-pathnames-as-file
               (make-repo-path *dir-prefix* prj) ".git/config"))
        (project-name (car prj)))
    (if (cl-fad:file-exists-p  file)
        (delete-file file))
    (handler-case
        (with-output-to-file (s file :if-exists :overwrite :if-does-not-exist :create)
          (format s "[core]~%")
          (format s "~trepositoryformatversion = 0~%")
          (format s "~tfilemode = true~%")
          (format s "[remote \"~a\"]~%" remote)
          (format s "~turl = ssh://192.168.4.4:29418/ZH/actions/~a/~a~a~%" remote repo-prefix project-name)
          (format s "~treview = notused.actions-semi.com~%")
          (format s "~tprojectname = ~a~%" project-name)
          (format s "fetch = +refs/heads/*:refs/remotes/~a/*~%" remote))
      (error (e) (format t "~a~%"  e)))))

(defun gen-git-scripts(remote branch prj)
  (let ((file (make-repo-path *dir-prefix* prj)))
    (let ((dir (directory-namestring file)))
      (format t "cd ~a~%" dir)
      (format t "git push ~a HEAD:refs/heads/~a~%" remote branch))))

(defun restore-git-bak-config  (prj)
  (let ((file (make-repo-path *dir-prefix* prj)))
    (let* ((dir (directory-namestring file))
           (git-cofig-file (format nil "~a/.git/config" dir))
           (git-bak-file (format nil "~a/.git/config_bak" dir)))
      (if (cl-fad:file-exists-p git-bak-file)
          (cl-fad:copy-file git-bak-file git-cofig-file :overwrite t)))))

(defun gen-checktout-branch-script   (branch prj)
  (let ((file (make-repo-path *dir-prefix* prj)))
    (let* ((dir (directory-namestring file)))
      (format t "cd ~a~%" dir)
      (format t "git checkout ~a~%" branch))))

(defun parse-manifest-ls370a (path)
  (let (prjs)
    (setq prjs (mapcar (curry #'parse-regex-spec
                              `( ,(concatenate 'string  "<project name=\"" %name%  "\"" )
                                  ,(concatenate 'string "path=\"" %name%  "\"")
                                  ,(concatenate 'string "revision=\"" %name%  "\"")))
                       (read-file-into-lines path)))
    (setq prjs (remove-if-not #'list-have-elements prjs))
    (setq prjs (mapcar #'(lambda (prj) (if (not (nth 2 prj))
                                      (list (car prj) (cadr prj))))  prjs))
    (setq prjs (remove-if #'null  prjs))))


(defun gen-git-scripts-ls370a (path remote branch)
  (let (( *dir-prefix* "/home/local/ACTIONS/zhangdf/LS370A/" )
        manifest)
    (setq manifest
          (parse-manifest-ls370a path))
    ;;    (mapcan #'rename-git-config manifest)
    (mapcan (curry #'rewrite-git-config  remote "")  manifest)
    (mapcan (curry #'gen-git-scripts  remote branch)  manifest)))

(defun gen-git-scripts-aosp (remote branch)
  (let (( *dir-prefix* "/home/local/ACTIONS/zhangdf/aosp/" )
        manifest)
    (setq manifest
          (get-mainfest-diff-wholepath
           "/home/local/ACTIONS/zhangdf/aosp/.repo/manifest.xml"
           "/home/local/ACTIONS/zhangdf/gs700e/android/.repo/manifests/GS700E_android_7000.xml"))
    (mapcan #'rename-git-config manifest)
    (mapcan (curry #'rewrite-git-config  remote "android/")  manifest)
    (mapcan (curry #'gen-git-scripts  remote branch)  manifest)))

"/home/local/ACTIONS/zhangdf/TB_170711_LS370A__TAG_GS705B_4420_160408.xml"

#+ (or)
(loop for prj in
     (get-mainfest-diff-wholepath
      "/home/local/ACTIONS/zhangdf/aosp/.repo/manifest.xml"
      "/home/local/ACTIONS/zhangdf/gs700e/android/.repo/manifests/GS700E_android_7000.xml")
   do (format t "ZH/actions/GL5206/android/~a ~%" (car prj)))

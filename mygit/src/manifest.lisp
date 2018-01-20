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
  "==> list of project + path"
  (let (prjs)
    (setq prjs (mapcar (curry #'parse-regex-spec spec)
                       (read-file-into-lines path)))
    (setq prjs (mapcar func prjs))
    (setq prjs (remove-if-not #'list-have-elements prjs))
    (stable-sort prjs #'string< :key #'car)))

(defun get-manifest-diff-wholepath (aosp-xml owl-xml)
  "diff  prjs of aosp manifest xml and gs700e manifest xml"
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


(defparameter *android-root* nil )

(defun rename-git-config (prj)
  "rename .git/config file to bak"
  (let ((file (cl-fad:merge-pathnames-as-file
               (make-repo-path prj) ".git/config")))
    (if (cl-fad:file-exists-p  file)
        (rename-file file "config_bak")
        (format t "~a does not exist ~%" file))))

(defun make-repo-path (prj)
  "make new file system path from prj path and project file system root
==> absolute path "
  (let* ((path (cond ((cadr prj)  (cadr prj))
                     (t (car prj))))
         (root (cond ((starts-with-subseq "android" path) *android-root*)
                     (t *android-root*))))
    (let* ((dir (cl-fad:pathname-as-directory path)))
      (cl-fad:merge-pathnames-as-file root  dir))))

(defun rewrite-git-config (remote repo-prefix prj)
  "rewite git .config file to new remote "
  (let ((file (cl-fad:merge-pathnames-as-file
               (make-repo-path  prj) ".git/config"))
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

(defun gen-git-push-scripts(remote branch prj)
  " generate shell command to push prj to remote's branch "
  (let ((file (make-repo-path  prj)))
    (let ((dir (directory-namestring file)))
      (format t "cd ~a~%" dir)
      (format t "git push ~a HEAD:refs/heads/~a~%" remote branch)))  )

(defmacro with-project-path ((path prj) &body form )
  (let ((file (make-gensym "file")))
    `(let ((,file (make-repo-path  ,prj)))
       (let ((,path (directory-namestring ,file)))
         ,@form))))

(defun parse-remote-branch (rb)
  "parse git remote branch string
==> remote  branch"
  (multiple-value-bind (m r)
      (scan-to-strings "remotes/([a-zA-Z0-9]+)/(\\S+)" rb)
    (cond ((= 2 (length r)) (values (elt r 0) (elt r 1)))
          (t nil))))

(defun gen-delete-branch-scripts (remote-branch pb)
  " generate shell command to delete remote's branch "
  (multiple-value-bind (remote branch)
      (parse-remote-branch remote-branch)
    (with-project-path (path (project pb)) 
      (format t "cd ~a~%" path)
      (format t "git push ~a :refs/heads/~a~%" remote branch))))


(defclass project-branches ()
  ((project :initarg :project
            :reader project)
   (branches :initarg :branches
             :reader branches)))

(defun pprint-project-branches (*stand-output* prj)
  (pprint-logical-block (*standard-output* nil)
    (pprint-logical-block (*standard-output* (project prj))
      (pprint-exit-if-list-exhausted)
      (loop
         (write-string (pprint-pop))
         (write-char #\space)
         (pprint-exit-if-list-exhausted)
         (pprint-newline :fill)))
    (pprint-newline :linear)
    (pprint-logical-block (*standard-output* (branches prj))
      (pprint-exit-if-list-exhausted)
      (write-string (pprint-pop))
      (pprint-newline :fill))))

(set-pprint-dispatch  'project-branches 'pprint-project-branches)


(defun find-projects-by-branch (branch prj-branches)
  "branche: branch name to fine
prj-branches: list of project-branches instance"
  (remove-if-not  #'(lambda (b)
                      (search branch (branches b) :test #'string=))
                  prj-branches))

(defun restore-git-bak-config  (prj)
  (let ((file (make-repo-path  prj)))
    (let* ((dir (directory-namestring file))
           (git-cofig-file (format nil "~a/.git/config" dir))
           (git-bak-file (format nil "~a/.git/config_bak" dir)))
      (if (cl-fad:file-exists-p git-bak-file)
          (cl-fad:copy-file git-bak-file git-cofig-file :overwrite t)))))

(defun gen-checktout-branch-script   (branch prj)
  " generate shell command to checkout prj branch"
  (let ((file (make-repo-path  prj)))
    (let* ((dir (directory-namestring file)))
      (format t "cd ~a~%" dir)
      (format t "git checkout ~a~%" branch))))


(defun get-project-branches (prj)
  (let* ((file (make-repo-path  prj))
         (dir (directory-namestring file))
         shell-res)
    (setq shell-res
          (multiple-value-list (shell-command
                                (format nil "cd ~a &&  git branch -a" dir))))
    (let ((status (nth 2 shell-res) ))
      (cond ((and status  (= 0 status)) (make-instance  'project-branches
                                                        :project prj :branches  (car shell-res)))
            (t nil)))))


(defun shell-command-ok (res)
  (let ((status (nth 2 res) ))
    (if status
        (= 0 (nth  2 res)))))

(defun get-all-project-branches (prjs)
  "==>list of prj & git branch cmd output"
  (remove-if #'null
             (mapcar #'get-project-branches prjs)))

(defun get-manifest-all-branches (prjs)
  "get all branches in maniest xml"
  (let ((all-branches (get-all-project-branches prjs)))
    (stable-sort
     (remove-duplicates
      (remove-if-not
       (curry #'starts-with-subseq "  remotes/")
       (mapcan #'string-to-lines
               (mapcar #'branches  all-branches)))
      :test #'string=)
     #'string<)))




#+ (or)
(loop for prj in
     (get-mainfest-diff-wholepath
      "/home/local/ACTIONS/zhangdf/aosp/.repo/manifest.xml"
      "/home/local/ACTIONS/zhangdf/gs700e/android/.repo/manifests/GS700E_android_7000.xml")
   do (format t "ZH/actions/GL5206/android/~a ~%" (car prj)))

#+(or)
(defun gen-git-scripts-aosp (remote branch)
  (let (( *repo-root* "/home/local/ACTIONS/zhangdf/aosp/" )
        manifest)
    (setq manifest
          (get-manifest-diff-wholepath
           "/home/local/ACTIONS/zhangdf/aosp/.repo/manifest.xml"
           "/home/local/ACTIONS/zhangdf/gs700e/android/.repo/manifests/GS700E_android_7000.xml"))
    (mapcan #'rename-git-config manifest)
    (mapcan (curry #'rewrite-git-config  remote "android/")  manifest)
    (mapcan (curry #'gen-git-push-scripts  remote branch)  manifest)))

"/home/local/ACTIONS/zhangdf/TB_170711_LS370A__TAG_GS705B_4420_160408.xml"

#+(or)
(defun gen-git-scripts-ls370a (path remote branch)
  (let (( *repo-root* "/home/local/ACTIONS/zhangdf/LS370A/" )
        manifest)
    (setq manifest
          (parse-manifest-ls370a path))
    ;;    (mapcan #'rename-git-config manifest)
    (mapcan (curry #'rewrite-git-config  remote "")  manifest)
    (mapcan (curry #'gen-git-scripts  remote branch)  manifest)))


"/home/local/ACTIONS/zhangdf/gs700e/android/.repo/manifests/GS700E_android_5110.xml
"



#+ (or)
(find-projects-by-branch "remotes/gl5209/remotes/gl5209/zh/OWL_Integratio" branch)

#+(or)
(defun parse-manifest-gs700e (xml-path)
  (parse-manifest xml-path
                  `( ,(concatenate 'string  "<project name=\"" %name%  "\"" )
                      ,(concatenate 'string "path=\"" %name%  "\""))
                  #'(lambda (prj)  prj)))

#+(or)
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

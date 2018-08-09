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

(defun rename-git-config (prj)
  "rename .git/config file to bak"
  (let ((file (cl-fad:merge-pathnames-as-file
               (make-repo-path prj) ".git/config")))
    (if (cl-fad:file-exists-p  file)
        (rename-file file "config_bak")
        (format t "~a does not exist ~%" file))))

(defparameter *android-root* nil )

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
         (let ((obj (pprint-pop)))
           (pprint-exit-if-list-exhausted)
           (when  obj
             (write-string obj)
             (write-char #\space)
             (pprint-newline :fill)))))
    (pprint-newline :linear)
    (pprint-logical-block (*standard-output* (branches prj))
      (pprint-exit-if-list-exhausted)
      (write-string (pprint-pop))
      (pprint-newline :fill))))

(set-pprint-dispatch  'project-branches 'pprint-project-branches)

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

(defclass repo-project ()
  ((projects  :initarg :projects
              :accessor projects)
   (project-branches :initarg :project-branches
                     :accessor project-branches)))

(defun find-projects-by-branch (branch prj-branches)
  "branche: branch name to fine
prj-branches: list of project-branches instance"
  (remove-if-not  #'(lambda (b)
                      ;;(format t "~a ~a~%" (project b) (branches b))
                      (search branch (branches b) :test #'string=))
                  prj-branches))

(defun get-project-remote (prj)
  (let* ((file (make-repo-path prj))
         (dir (directory-namestring file))
         shell-res)
    (setq shell-res
          (multiple-value-list (shell-command
                                (format nil "cd ~a &&  git remote" dir))))
    (let ((status (nth 2 shell-res) ))
      (cond ((and status  (= 0 status)) (scan-to-strings "\\S+" (car shell-res)))
            (t nil)))))

(defun gen-delete-branch-scripts (remote-branch project-branches)
  " generate shell command to delete remote's branch "
  (let (remote branch)
    (cond ((starts-with-subseq "  remotes/" remote-branch)
           (multiple-value-bind (r b)
               (parse-remote-branch remote-branch)
             (setq remote r  branch b )))
          (t (setq remote (get-project-remote (project project-branches))
                   branch remote-branch)))
    (with-project-path (path  (project project-branches))
      (format t "cd ~a~%" path)
      (format t "git push ~a :refs/heads/~a~%" remote branch))))

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

#+(or)
(defun gen-git-scripts-ls370a (path remote branch)
  (let (( *repo-root* "/home/local/ACTIONS/zhangdf/LS370A/" )
        manifest)
    (setq manifest
          (parse-manifest-ls370a path))
    ;;    (mapcan #'rename-git-config manifest)
    (mapcan (curry #'rewrite-git-config  remote "")  manifest)
    (mapcan (curry #'gen-git-scripts  remote branch)  manifest)))

(defun parse-manifest-gs700e (xml-path)
  (setq *android-root* (subseq xml-path 0 (search ".repo/" xml-path )))
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

(defparameter +help-msg+ "
list-all-branches: list all branches of repo project
list-projects: list all project
rename-config: backup git's config file to config_bak
restore-config: restor git's config_bak to config file
rewrite-git-config remote repo-prefix: , and rewrite .config to new gerrit remote
gen-git-push remote branch: generate git push scripts for projects
list-branch  branch: list all projects by branch
del-branch branch: generate shell scripts to delete repo branch")

(defun command-interface (repo cmd &rest args)
  (let ((prjs (projects repo))
        (branches (project-branches repo)))
    (ecase cmd
      (help
       (format t "~a" +help-msg+))
      (list-all-branches
       (let ((all (get-manifest-all-branches prjs)))
         (loop for b in all
            do (format t "~a~%" b))))
      (list-projects prjs)
      (rename-config
       (mapcan #'rename-git-config prjs))
      (restore-config
       (mapcan #'restore-git-bak-config  prjs))
      (rewrite-git-config
       (mapcan (apply #'curry #'rewrite-git-config  args)  prjs))
      (gen-git-push
       (mapcan (apply #'curry #'gen-git-push-scripts  args) prjs))
      (list-branch
       (apply  #'find-projects-by-branch (append args (list branches))))
      (del-branch
       (mapcar #'(lambda (prj) (apply  #'gen-delete-branch-scripts (append args (list prj))))
               (apply  #'find-projects-by-branch (append args (list branches)))))
      (t  nil))))

(defparameter command  nil)
(defun make-command-interface (xml)
  "parse manifest xml"
  (let* ((prjs (parse-manifest-gs700e  xml))
         (prj-branches (get-all-project-branches prjs))
         (repo (make-instance 'repo-project :projects prjs :project-branches prj-branches)))
    (setq command (curry #'command-interface repo))))

#+(or)
(make-command-interface  "/home/local/ACTIONS/zhangdf/sdk/5206/ifly/android/.repo/manifests/TOYCLOUD__TAG_180211_BF2__TAG_AD500A_5110_160623.xml")

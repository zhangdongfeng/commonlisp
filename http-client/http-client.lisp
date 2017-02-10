;;;; http-client.lisp

(in-package #:http-client)


(use-package (list :drakma :html5-parser :cl-ppcre :cl-html-parse))
;;; "http-client" goes here. Hacks and glory await!

#+ (or)
(defun flex-dom-map (recurse-p fn node)
  "fn is applied to each visited node
   recurse-p controls whether to visit the children of node"
  (if node
      (progn
        (funcall fn node) ;apply the function to the node
        (if (funcall recurse-p node)
            (html5-parser:element-map-children
             (lambda (n-node) (flex-dom-map recurse-p fn n-node)) node)))))


#+ (or)
(defun standard-recurse-p (node)
  "returns true only if you aren't trying to recurse into a script,
  style, or noscript tag."
  (not (or (equalp (node-name node) "script")
	   (equalp (node-name node) "style")
	   (equalp (node-name node) "noscript"))))



#+ (or)
(defun remove-node-if (pred node)
  (declare (optimize debug))
  (if node
      (if (not (funcall pred node))
          (cons (list (node-type node)
                      (node-name node)
                      (node-value node))
                (list (element-map-children
                       (lambda (n-node)
                         (remove-node-if pred n-node))
                       node))))))

#+ (or)
(defun get-title (dom-node)
  (flex-dom-map
   #'standard-recurse-p
   (lambda (node) (if (equalp (node-name node) "title")
                 (return-from get-title (node-value (node-first-child node)))))
   dom-node))

(defun remove-newlines (str)
  (remove-if (lambda (ch) (or (eql ch #\return)
                         (eql ch #\linefeed))) str))
#+ (or)
(defun scrapetext (top-node recurse-p)
  (remove-newlines
   (with-output-to-string (s)
     (flex-dom-map
      recurse-p
      (lambda (node) (if (equal (html5-parser:node-type node) :TEXT)
                    (format s " ~a " (html5-parser:node-value node))))
      top-node))))

#+ (or)
(defun get-article-text-node (root-node)
  "this function maps its way through the DOM nodes till it finds a 
   node with name 'div' and 'id' attribute equal to 'mw-content-text', 
   then returns that node"
  (flex-dom-map #'standard-recurse-p
                (lambda (node)
                  (if (equalp (html5-parser:node-name node) "div")
                      (if (equalp (html5-parser:element-attribute node "id") "mw-content-text")
                          (return-from get-article-text-node node))))
                root-node ))

#+ (or)
(defmethod no-applicable-method ((method (eql #'html5-parser::%node-attributes)) &rest args)
  "this method surpresses 'no-applicable-method' errors from html5-parser::%node-attributes and
   simply returns nil instead.  These errors come about because we are calling element-attributes
   on text nodes, which of course do not have attributes"
  nil)


#+ (or)
(defun getwikitext (root-node)
  (let ((wikitextnode (get-article-text-node root-node)))
    (remove-excess-whitespace 
     (scrapetext wikitextnode 
                 (lambda (node)
                   (and (standard-recurse-p node)
                        (not (equalp (node-name node) "table"))
                        (not (cl-ppcre:scan "thumb" (element-attribute node "class")))))))))

(defun remove-excess-whitespace (str)
  (cl-ppcre:regex-replace-all "\\s+" str " "))

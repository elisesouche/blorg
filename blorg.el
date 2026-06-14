;;; blorg.el --- SSG designed for blogs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author: Élise Souche <elise@souche.one>
;; Maintainer: Élise Souche <elise@souche.one>
;; Created: September 23, 2025
;; Modified: September 23, 2025
;; Version: 0.0.1
;; Keywords: hypermedia multimedia text tools
;; Homepage: https://github.com/elisesouche/blorg
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Publish a blog written in Org
;;
;;; Code:


(load-library "dom.el")
(require 'ox-html)
(load-library "org-element.el")

(defvar blorg-lexical-evaluation 't "Whether evaluation of <el> blocks should be lexically scoped.")

(defun blorg--htmel-map-tail (fun list)
  "Similar to mapcar, but only does it on the cddr."
  `(,(car list) ,(cadr list) . ,(mapcar fun (cddr list))))

(defun blorg--htmel-htmel-eval-outer-el (html)
  "Evaluate the outermost <el> tags in HTML and insert them back into the DOM."
  (if (listp html)
      (if (eq (dom-tag html) 'el)
          (eval (car (read-from-string (dom-text html))) blorg-lexical-evaluation)
        (blorg--htmel-map-tail #'blorg--htmel-htmel-eval-outer-el html))
    html))

(defun blorg--htmel-remove-html-body (html) (caddr (caddr html)))

(defun blorg-htmel-parse-htmel ()
  "Return the HTML DOM, with Elisp evaluation done."
  (let ((tree (blorg--htmel-remove-html-body (libxml-parse-html-region))))
    (blorg--htmel-htmel-eval-outer-el tree)))

(defun blorg-htmel-parse-htmel-string (htmel)
  (with-temp-buffer
    (insert htmel)
    (blorg-htmel-parse-htmel)))

(defun blorg-htmel-expand-htmel (htmel-str)
  "Expand HTMEL-STR into HTML, as a string."
  (with-temp-buffer
    (insert htmel-str)
    (let ((html (blorg-htmel-parse-htmel)))
      (delete-region (point-min) (point-max))
      (dom-print html)
      (buffer-string))))

(defun blorg-htmel-expand-htmel-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (blorg-htmel-expand-htmel (buffer-string))))

(defun blorg-htmel-dom-to-html (dom-str)
  "Evaluate the string DOM-STR, returning the corresponding HTML (as a string)."
  (with-temp-buffer
    (let ((dom (eval (car (read-from-string dom-str)) blorg-htmel-lexical-evaluation)))
      (dom-print dom)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defvar blorg-build-dir "_build")
(defvar blorg-site-dir "site")
(defvar blorg-static-dir "static")

(defun blorg-categories ()
  "Find all categories for the blorg project."
  (seq-filter
   (lambda (f) (not (or (string= f ".") (string= f ".."))))
   (seq-map
    (lambda (f) (file-name-base f))
    (seq-filter
     #'file-directory-p
     (directory-files blorg-site-dir 'full)))))

(cl-defstruct blorg-blogpost name date desc file)

(defun blorg--org-buffer-paragraphs ()
  "Return a list of all the paragraphs in the document.
They are represented as pairs of positions in the buffer."
  (org-element-map (org-element-parse-buffer) 'paragraph
    (lambda (par)
      `(,(org-element-property :contents-begin par) . ,(org-element-property :contents-end par)))))


(defun blorg--org-buffer-first-paragraph ()
  "Return the first paragraph of the current buffer, as a string."
  (let* ((contents (blorg--org-buffer-paragraphs))
         (first-par (nth 0 contents)))
    (buffer-substring-no-properties (car first-par) (cdr first-par))))

(defun blorg--parse-blogpost (filename)
  "Build a blogpost structure representing FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (delay-mode-hooks (org-mode))
    (let ((desc (blorg--org-buffer-first-paragraph))
          (date (encode-time (org-parse-time-string (cadar (org-collect-keywords '("DATE" "date"))))))
          (title (org-get-title (current-buffer)))
          (file (concat "./" (file-name-nondirectory filename))))
      (and
       date
       (make-blorg-blogpost :name title :date date :desc desc :file file)))))

(defun blorg--get-blogposts (dir)
  "Parse all posts in DIR, returning them as a list."
  (seq-reverse
   (seq-sort-by
    (lambda (post) (blorg-blogpost-date post))
    #'time-less-p
    (seq-filter
     #'identity
     (seq-map
      #'blorg--parse-blogpost
      (seq-filter #'file-regular-p (directory-files dir 'full)))))))

(defun blorg--put-postdata (data buffer)
  "Pretty-print blogpost DATA in BUFFER."
  (with-current-buffer buffer
    (insert "* [[" (blorg-blogpost-file data) "][" (blorg-blogpost-name data) "]]\n"
            "*Posted on " (format-time-string "%Y-%m-%d" (blorg-blogpost-date data)) "*\n\n"
            (blorg-blogpost-desc data) "\n\n"
            "-----\n\n")))

(defun blorg-build-index (dir buffer)
  "Build the index for DIR in BUFFER."
  (interactive (list default-directory (current-buffer)))
  (let ((posts (blorg--get-blogposts dir)))
    (seq-do (lambda (post) (blorg--put-postdata post buffer)) posts)))

(defun blorg--index-for-dir (dir)
  "Return the index file path for DIR."
  (concat dir "/" "index.org"))

(defun blorg-build-index-file (dir)
  "Build the index for DIR in it's index file."
  (with-temp-buffer
    (insert "#+title: " (file-name-nondirectory (substring dir 0 -1)) "\n\n")
    (blorg-build-index dir (current-buffer))
    (write-file (blorg--index-for-dir dir))))


(defun blorg-build-all-blog-index (directories)
  "Build the index for each directory in DIRECTORIES."
  (seq-do
   (lambda (dir)
     (message "Building index file for %s" dir)
     (blorg-build-index-file dir))
   directories))

(defun blorg-delete-all-blog-index (directories)
  "Delete all generated index files in DIRECTORIES."
  (seq-do
   (lambda (dir) (delete-file (blorg--index-for-dir dir)))
   directories))

(org-export-define-derived-backend 'htmel 'html
  :translate-alist '((special-block . blorg-ox-html-special-block)))

(defun blorg-ox-html-special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK for HTMEL.
It is the same as ox-html, but it treats htmel blocks specially.
CONTENTS and INFO are passed the normal way."
  (if
      (string= "htmel" (org-element-property :type special-block))
      (let ((real-contents (substring contents 4 -5)))
        (progn
          (message "Expanding HTMEL block...%s\n" real-contents)
          (htmel-dom-to-html real-contents)))
    (org-html-special-block special-block contents info)))

(defun blorg-htmel-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML, interpreting HTMEL.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'htmel filename ".html" plist pub-dir))

(defmacro with-setq (variable value &rest body)
  "Temporarily set VARIABLE to VALUE to evaluate BODY."
  (let ((var-backup (gensym variable)))
    `(progn
       (setq ,var-backup ,variable)
       (setq ,variable ,value)
       (progn ,@body)
       (setq ,variable ,var-backup))))

(defmacro with-setq* (binds &rest body)
  "Evaluate BODY with BINDS.
This is like nesting a bunch of `with-setq.

\(fn ((VAR VALUE) ...) &rest BODY)"
  (if (consp binds)
      (let* ((bind (car binds))
             (binds (cdr binds))
             (variable (car bind))
             (value (cadr bind)))
        `(with-setq ,variable ,value (with-setq* ,binds ,@body)))
    `(progn ,@body)))


(defmacro with-cd (directory &rest body)
  (let ((cur (gensym)))
    `(let ((,cur default-directory))
       (cd ,directory)
       (progn ,@body)
       (cd ,cur))))

;;;###autoload
(defun blorg-publish (directory)
  (interactive "DDirectory to publish: ")
  (with-cd directory
           (load-file "config.el")
           (with-setq* ((org-html-head (blorg-htmel-expand-htmel-file "./data/header.html"))
                        (org-html-home/up-format (blorg-htmel-expand-htmel-file "./data/chunks/navbar.html"))

                        (org-publish-project-alist
                         `(("org"
                            :base-directory ,blorg-site-dir
                            :base-extension nil
                            :recursive t
                            :publishing-function blorg-htmel-publish-to-html
                            :publishing-directory ,blorg-build-dir
                            :html-validation-link nil
                            :html-head-include-default-style nil
                            :html-link-home "/"
                            :html-link-up ".."
                            :section-numbers nil
                            :with-toc nil
                            :author ,author)
                           ("static"
                            :base-directory ,blorg-static-dir
                            :base-extension "css\\|js\\|png\\|svg\\|ico\\|gpg\\|pdf\\|mp3\\|mp4"
                            :recursive t
                            :publishing-function org-publish-attachment
                            :publishing-directory ,blorg-build-dir)
                           ("site" :components ("org" "static")))))

                       ;; (setq org-html-htmlize-output-type 'css)

                       (blorg-build-all-blog-index blog-directories)
                       (org-publish "site" t)
                       (blorg-delete-all-blog-index blog-directories))))


(provide 'blorg)
;;; blorg.el ends here

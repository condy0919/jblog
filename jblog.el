;;; jblog.el --- jekyll blog posts manager -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: tools
;; Url: https://github.com/condy0919/jblog
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs major mode for managing blog posts with jekyll or other WYSIWYG bloging
;; systems.

;;; Code:

(require 'rx)
(require 'cl-lib)
(require 'cl-macs)
(require 'subr-x)
(require 'tabulated-list)

(defgroup jblog nil
  "Major mode managing blog posts."
  :group 'tools)

(defcustom jblog-posts-directory nil
  "The directory for you blog posts.
For example, \"~/blog/_posts\"."
  :group 'jblog
  :type 'string)

(defcustom jblog-post-exts-regexp (rx "." (or "md" "markdown") string-end)
  "The regexp of post extensions to match."
  :group 'jblog
  :type 'string)

(defcustom jblog-post-default-ext "md"
  "The default extension of a blog post."
  :group 'jblog
  :type 'string)

(defcustom jblog-post-headers [("Date"  10 t)
                               ("Title" 36 t)]
  "Post headers listed in main view.
DATE is retrieved from filename while TITLE is from post
headers."
  :group 'jblog
  :type 'vector)

(defcustom jblog-post-sort-key '("Date" . t)
  "Post sort key."
  :group 'jblog
  :type '(alist :key-type string :value-type boolean))

(defcustom jblog-post-headers-format "---
layout: post
title: %s
---
"
  "The header format on creating new posts."
  :group 'jblog
  :type 'string)

(defcustom jblog-post-headers-guard "---"
  "The headers guard."
  :group 'jblog
  :type 'string)

(defconst jblog--buffer-name "*JBlog*"
  "JBlog major buffer name.")

(defun jblog--extract-headers-from-file (file)
  "Extract all headers from FILE.
Assume that FILE is created via jblog."
  ;; Generate a regexp from `jblog-post-headers-format'
  (let* ((lines (split-string jblog-post-headers-format "\n" t "\\s-"))
         (hdrs (cl-loop for s in lines
                        when (string-match "^\\(.+\\):\\(.+\\)$" s)
                        collect (downcase (string-trim (match-string 1 s)))))
         (hdr-map))
    (with-temp-buffer
      (insert-file-contents file)
      (when-let* ((beg (search-forward jblog-post-headers-guard))
                  (end (search-forward jblog-post-headers-guard))
                  (content (delete-and-extract-region beg end)))
        (dolist (hdr hdrs)
          (when (string-match (format "%s.*:\\(.+\\)$" hdr) content)
            (push `(,hdr . ,(string-trim (match-string 1 content))) hdr-map)))))
    hdr-map))

(defun jblog--retrieve-date-from-file (file)
  "Retrieve the YYYY-mm-dd date from FILE (actuall from file name)."
  (let ((regexp "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"))
    (and (string-match regexp file)
         (match-string 1 file))))

(defun jblog--post-entries (file)
  "Get entries of FILE."
  (cl-block ensure-date-field
    ;; Computes the intersection of header fields
    (let* ((hdr-map (jblog--extract-headers-from-file file))
           (date (jblog--retrieve-date-from-file file))
           (len (length jblog-post-headers))
           (entries (make-vector len nil)))
      ;; Early return for those "template.md" files whose date is nil
      (when (not date)
        (cl-return-from ensure-date-field))

      (cl-loop for i below len do
               (let ((key (car (elt jblog-post-headers i)))
                     (open-it (lambda (&optional _) (find-file (tabulated-list-get-id)))))
                 (pcase (downcase key)
                   ("date"
                    (aset entries i `(,date
                                      action ,open-it)))
                   (k
                    ;; When other header fields are missing, use "-" instead
                    (aset entries i `(,(or (cdr (assoc k hdr-map)) "-")
                                      action ,open-it))))))
      (list file entries))))

(defun jblog-create (title permalink)
  "Create a new blog post with TITLE which can be visited by PERMALINK."
  (interactive "sTitle: \nsPermalink for post '%s': \n")
  (unless jblog-posts-directory
    (error "Please set jblog-posts-directory variable"))
  (let* ((date (format-time-string "%4Y-%2m-%2d"))
         (filename (format "%s-%s.%s" date permalink jblog-post-default-ext))
         (header (format jblog-post-headers-format title)))
    (find-file (expand-file-name filename jblog-posts-directory))
    (insert header)
    (newline)))

(defun jblog-delete ()
  "Delete post."
  (interactive)
  (unless jblog-posts-directory
    (error "Please set jblog-posts-directory variable"))
  (let ((file (tabulated-list-get-id)))
    (when (y-or-n-p (format "Really delete '%s'? " file))
      (when-let* ((buf (find-buffer-visiting file)))
        (kill-buffer buf))
      (delete-file file)
      (jblog-refresh))))

(defun jblog-search ()
  "Search posts."
  (interactive)
  (unless jblog-posts-directory
    (error "Please set jblog-posts-directory variable"))
  (jblog-refresh (read-from-minibuffer "Search filter: ")))

(defun jblog-refresh (&optional keyword remember-pos update)
  "Refresh with &optional KEYWORD REMEMBER-POS UPDATE."
  (interactive)
  (unless jblog-posts-directory
    (error "Please set jblog-posts-directory variable"))
  (with-current-buffer (get-buffer-create jblog--buffer-name)
    (let* ((posts (directory-files jblog-posts-directory t jblog-post-exts-regexp))
           (entries (mapcar 'jblog--post-entries posts)))
      ;; Filter out the entries without date field
      (setq entries (cl-remove-if 'not entries))

      (when keyword
        (setq entries (cl-remove-if-not
                       (lambda (x)
                         (cl-destructuring-bind (file ents) x
                           (let* ((filename (file-name-nondirectory file))
                                  (hdrs (cl-loop for i below (length ents)
                                                 collect (car (elt ents i)))))
                             (string-match-p keyword
                                             (mapconcat 'identity `(,filename ,@hdrs) " ")))))
                       entries)))
      (setq tabulated-list-sort-key jblog-post-sort-key)
      (setq tabulated-list-entries entries)
      (tabulated-list-print remember-pos update))))

(defun jblog-open-posts-directory ()
  "Open `jblog-posts-directory' with `dired'."
  (interactive)
  (unless jblog-posts-directory
    (error "Please set jblog-posts-directory variable"))
  (dired jblog-posts-directory))

(defvar jblog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "q" 'quit-window)
    (define-key map "s" 'jblog-search)
    (define-key map "g" 'jblog-refresh)
    (define-key map "C" 'jblog-create)
    (define-key map "D" 'jblog-delete)
    (define-key map "O" 'jblog-open-posts-directory)
    map)
  "JBlog mode map.")

(define-derived-mode jblog-mode tabulated-list-mode "JBlog"
  "Major mode for managing blog posts.
\\<jblog-mode-map>
\\{jblog-mode-map}"
  (setq tabulated-list-format jblog-post-headers)
  (tabulated-list-init-header)
  (jblog-refresh))

;;;###autoload
(defun jblog ()
  "Start jblog."
  (interactive)
  (with-current-buffer (get-buffer-create jblog--buffer-name)
    (jblog-mode))
  (switch-to-buffer jblog--buffer-name))

(provide 'jblog)

;;; jblog.el ends here

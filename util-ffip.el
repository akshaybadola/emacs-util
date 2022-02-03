;;; util-ffip.el --- Find file in project extentions. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020,2021
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Thursday 27 January 2022 12:52:38 PM IST>
;; Keywords:	utility, convenience, emacs-lisp, org, helm
;; Version:     0.4.0
;; Package-Requires: ((a "0.1.1") (dash "2.17.0")
;;                    (find-file-in-project "6.0.6"))

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


;;; Commentary:

;; Extensions for `find-file-in-project'

;;; Code:


(require 'f)
(require 'util/core "util-core.el")
(require 'find-file-in-project)         ; Only used for `ffip-project-root'


(defun util/ffip-gitignore-expr (expr)
  "Generate patterns for use with find command according to EXPR."
  (cond ((string-suffix-p "/" expr)
         (--> expr
              (replace-regexp-in-string "\\*" "\\\\*" it)
              (replace-regexp-in-string (rx "/" (group (+ any)) eol) "./\\1" it)
              (replace-regexp-in-string (rx  (group (+ any)) "/" eol) "\\1" it)
              (cons 'prune (format "-path %s%s\\*"
                                   (if (string-prefix-p "./" it) "" "\\*") it))))
        (t
         (cons 'exclude
               (->> expr
                    (replace-regexp-in-string "\\*" "\\\\*")
                    (replace-regexp-in-string (rx "/" (group (+ any)) eol) "./\\1")
                    (format "\\! -path \\*%s\\*"))))))

(defun util/ffip-search-paths (pat)
  "Like `util/ffip-search' but search pattern PAT as path instead."
  (interactive
   (list (read-string "Enter the search term: ")))
  (util/ffip-search pat t))

;; TODO: This is incomplete
(defvar util/ffip-debug nil)
(defvar util/ffip-debug-cmd nil)
(defun util/ffip-search (&optional pattern search-for-path)
  "Search for PATTERN and list files in the project in a Dired buffer.

Optional SEARCH-FOR-PATH modifies the find behaviour to use
\"-ipath\" instead of \"-iname\"."
  (interactive "p")
  (let* ((ci (not pattern))
         (pattern (pcase pattern
                    ((pred stringp) pattern)
                    (1 (read-string "Enter the search term: "))
                    ('nil (user-error "Error.  Pattern is required if not called interactively"))
                    (_ pattern)))
         (root (expand-file-name (ffip-project-root)))
         (gitignore (f-join root ".gitignore"))
         (gitignore-open (find-buffer-visiting gitignore))
         ;; NOTE: These are all directories
         (prune-patterns (-uniq (mapcar
                                 (lambda (x)
                                   (replace-regexp-in-string "^\\*/\\(.+\\)"
                                                             "\\1/" x))
                                 (-concat ffip-prune-patterns
                                          '("*/env" "*/node_modules" "*/build" "*/dist")))))
         ;; (prune-patterns (-concat ffip-prune-patterns
         ;;                          '("*/env" "*/node_modules" "*/build" "*/dist")))
         excludes)
    (when (f-exists? gitignore)
      (with-current-buffer (find-file-noselect gitignore)
        (setq prune-patterns
              (mapcar #'util/ffip-gitignore-expr
                      (-concat prune-patterns
                               (split-string
                                (replace-regexp-in-string
                                 "#.*" ""
                                 (buffer-substring-no-properties (point-min) (point-max)))))))
        (setq excludes (-keep (lambda (x) (and (eq (car x) 'exclude) (cdr x))) prune-patterns))
        (setq prune-patterns (-keep (lambda (x) (and (eq (car x) 'prune) (cdr x))) prune-patterns))
        (unless gitignore-open
          (kill-buffer (current-buffer)))))
    (let* ((cmd (format "find %s %s \\( %s \\) %s %s -type f -print"
                        root
                        (format " \\( %s \\) -prune -o " (string-join prune-patterns " -o "))
                        (string-join excludes " ")
                        (if search-for-path "-ipath" "-iname")
                        (if (string-match-p "\\*" pattern)
                            (replace-regexp-in-string "\\*" "\\\\*" pattern)
                          (concat "\\*" pattern "\\*"))))
           (file-list (split-string (shell-command-to-string cmd)))
           buf)
      (unless (and (boundp 'util/ffip-debug) util/ffip-debug)
        (setq util/ffip-debug-cmd cmd))
      (cond ((and ci file-list)
             (setq buf (dired-internal-noselect
                        (mapcar (lambda (x) (f-join root x)) file-list)))
             (pop-to-buffer buf)
             (with-current-buffer buf
               (rename-buffer (format "*find* \"%s\"" pattern) t)))
            ((and (not ci) file-list)
             file-list)
            ((and ci (not file-list))
             (message "No files found matching pattern %s" pattern))
            (t nil)))))

(defun util/ffip-grep-pattern (&optional file-pattern grep-pattern)
  "Grep for GREP-PATTERN in files returned by `util/ffip-search'.
The files are matched with FILE-PATTERN."
  (interactive "p")
  (let* ((phrase (thing-at-point 'symbol t))
         (file (concat "*." (car (last (split-string (buffer-file-name) "\\.")))))
         (file-pattern (util/read-if-nil file-pattern file))
         (grep-pattern (util/read-if-nil grep-pattern phrase))
         (files (util/ffip-search file-pattern))
         (cmd (concat "grep --color -nH --null -i -E " (format "\"%s\"" grep-pattern) " "
                      (mapconcat (lambda (x) (format "\"%s\"" x)) files " ")))
         grep-save-buffers)
    (if files
        (grep cmd)
      (message "No files found matching pattern %s" grep-pattern))))

(defun util/ffip-grep-default ()
  "Grep for PATTERN in files of current mode."
  (interactive)
  (util/ffip-grep-pattern (concat "*." (car (last (split-string (buffer-file-name) "\\."))))))

(defalias 'util/ffip-gg 'util/ffip-grep-git-files)

(defcustom util/ffip-grep-num-files-threshold
  50
  "Number of max files to grep for util/*grep functions."
  :type 'number
  :group 'util)

(defun util/ffip-grep-git-files (&optional pattern filter-re)
  "Grep for PATTERN in git staged files.

If optional FILTER-RE is given then filter files based on that
regexp first."
  (interactive)
  (let* ((root (ffip-project-root))
         (phrase (thing-at-point 'symbol t))
         (cmd-output (shell-command-to-string (format "cd %s && git ls-files" root)))
         (filter-re (if (or filter-re current-prefix-arg)
                        (util/read-if-nil
                         filter-re nil
                         "Restrict grep to files matching regexp: (default all): ")
                      "."))
         (files (if (string-match-p "fatal: not a git" cmd-output)
                    (user-error "%s does not seem to be a git repo" root)
                  (-filter (lambda (x) (string-match-p filter-re x))
                           (split-string cmd-output))))
         (prompt (if (not (string= filter-re "."))
                     (concat "Grep in git files matching " filter-re  " : (default %s): ")
                   "Grep in git files: (default %s): "))
         (pattern (util/read-if-nil pattern phrase prompt))
         (default-directory root)
         (cmd (concat "grep --color -nH --null -i -E " (format "\"%s\"" pattern) " "
                      (mapconcat (lambda (x) (format "\"%s\"" x)) files " ")))
         ;; NOTE: Old cmd doesn't escape file names or even pattern
         ;; (cmd (string-join (-concat `("grep --color -nH --null -i -E" ,pattern) files) " "))
         grep-save-buffers)
    (if files
        (if (> (length files) util/ffip-grep-num-files-threshold)
            (if (y-or-n-p
                 (format "You are about to grep in %s files.  Are you sure? " (length files)))
                (grep cmd)
              (user-error "Aborted"))
          (grep cmd))
      (message "No files found matching pattern %s" pattern))))


(provide 'util/ffip)

;;; util-ffip.el ends here

;;; util-misc.el --- Various utility functions. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020,2021,2022
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Tuesday 07 June 2022 12:02:29 PM IST>
;; Keywords:	utility, convenience, emacs-lisp, org, helm
;; Version:     0.4.0
;; Package-Requires: ((util/core))

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

;; Slightly quirky or a bit buggy utility functions.

;;; Code:


(require 'util/core "util-core.el")


(defvar util/insert-heading-python-executable "/usr/bin/python"
  "The python executable for `util/insert-heading-from-url'.")

(defun util/try-copy-help-buffer-link ()
  "In *Help* buffer, copy the url under point if it exists."
  (interactive)
  (when (and (eq major-mode 'help-mode))
    (let ((maybe-string (caadr (text-properties-at (point))))
          (maybe-url (get-text-property (point) 'help-echo)))
      (cond ((and maybe-url (string-match-p url-handler-regexp maybe-url))
             (message maybe-url)
             (kill-new maybe-url))
            ((stringp maybe-string)
             (message maybe-string)
             (kill-new maybe-string))
            (t (message "No link under point"))))))

;; FIXME: This thing is still buggy
(defun util/python-listify-lines-in-region ()
  "Convert lines in region to a python list."
  (interactive)
  (save-restriction
    (when (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (re-search-forward "\\(^[^ ].*\\)" nil t) ; "\\(\\w+\\)"
          (replace-match "\"\\1\",")
          (forward-line) (beginning-of-line))
        (insert "]")
        (goto-char (point-min))
        (insert "[")))))

;; NOTE: This should be webmacs but is CHROMIUM, actually sending a url to webmacs
;;       is pretty painless

;; (defalias 'util/browse-url-webmacs 'browse-url-webmacs)
;; (defun util/browse-url-webmacs (url &optional _new-window)
;;   "Ask the Chromium WWW browser to load URL.
;; Default to the URL around or before point.  The strings in
;; variable `browse-url-chromium-arguments' are also passed to
;; Chromium. The optional argument NEW-WINDOW is not used."
;;   (interactive (browse-url-interactive-arg "URL: "))
;;   (setq url (browse-url-encode-url url))
;;   (let* ((process-environment (browse-url-process-environment)))
;;     (apply 'start-process
;; 	   (concat "chromium " url) nil
;; 	   browse-url-chromium-program
;; 	   (append browse-url-chromium-arguments (list url)))))


;; FIXME: WTF does this even do?
(defun util/get-links-from-region ()
  "Get `shr' links from a buffer."
  (interactive)
  (let ((url-list nil))
    (while (thing-at-point (quote sentence))
      (push (get-text-property (point) 'shr-url) url-list)
      (forward-line))
    url-list))


(defun util/path-join (&rest elements)
  "Safely concatenate path ELEMENTS in a consistent manner."
  (declare (pure t) (side-effect-free t))
  (concat (if (string-prefix-p "~" (car elements)) "" "/" )
          (mapconcat (lambda (x)
                       (string-remove-prefix "/" (string-remove-suffix "/" x)))
                     elements "/")))

(defun util/max-ind (seq)
  (declare (pure t) (side-effect-free t))
  (let* ((max-val 0) (ind -1) (max 0))
    (cl-loop for x in seq
          do
          (progn
            (setq ind (+ 1 ind))
            (if x (if (> x max-val)
                      (progn (setq max-val x)
                             (setq max ind))))))
    max))


;; CHECK: perhaps should narrow to region in another buffer
(defun util/calc-with-braces ()
  "A utility function to calculate amounts written with braces.
For example, a region like:

3000 (some note) + 4000 (some other note) - 10000 (some other note)
+ 5000 (note) + 6000 (etc)

can be selected and the braces are ignored and the amount is summed up."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (let ((reg-begin (region-beginning))
              (reg-end (region-end)))
          (goto-char reg-begin)
          (insert "(+ ")
          (while (search-forward-regexp "+\\|(.*?)" reg-end t)
            (replace-match ""))
          (goto-char reg-end)
          (insert "))")
          (let (
                ;; (cur-end (point)) ; CHECK: Why unused?
                matches)
            (goto-char reg-begin)
            (while (search-forward-regexp "-[ 0-9]+? " reg-end t)
              (push (match-string 0) matches)
              (replace-match ""))
            (goto-char reg-begin)
            (insert "(- ")
            (forward-list)
            (insert
             (concat "(+ " (mapconcat (lambda (x) (util/replace-in-string x "-" "")) matches "") ")")))
          (message (number-to-string (eval-defun nil))))
      (message "Please select text to calculate"))))


;; TODO: Will do this later
;; (setq my/sphinx-doc-fun-beg-regex "class\\|def")
;; (defun my/sphinx-doc ()
;;   "Insert docstring for the Python function definition at point.
;; This is an interactive function and the docstring generated is as
;; per the requirement of Sphinx documentation generator."
;;   (interactive)

;;   ;; Cases: 1. ordinary func
;;   ;;        2. between class and __init__
;;   ;;        3. in class method
;;   ;;        4. in nested func
;;   ;;        5. in nested class
;;   ;;
;;   ;; By default this generates the doc for the backward class.
;;   ;; It should do by indentation. I'll do it some other time.
;;   (if (string= (thing-at-point 'word) "def")
;;       )
;;   (if (string= (thing-at-point 'word) "def")
;;       ;; (or (string= (thing-at-point 'word) "class")
;;       ;;     (string= (thing-at-point 'word) "def"))
;;       (back-to-indentation)
;;     (search-backward-regexp sphinx-doc-fun-beg-regex))

;;   (let ((fd (sphinx-doc-str->fndef (sphinx-doc-fndef-str))))
;;     (if fd
;;         (let ((indent (+ (sphinx-doc-current-indent) sphinx-doc-python-indent))
;;               (old-ds (sphinx-doc-existing))
;;               (new-ds (sphinx-doc-fndef->doc fd)))
;;           (progn
;;             (when old-ds (sphinx-doc-kill-old-doc indent))
;;             (sphinx-doc-insert-doc
;;              (if old-ds
;;                  (sphinx-doc-merge-docs old-ds new-ds)
;;                new-ds))
;;             (sphinx-doc-indent-doc indent)
;;             (search-forward "\"\"\""))))))


(defun util/fast-files-or-dirs (path f-or-d &optional recurse include-hidden)
  "Get all files or dirs or both or everything, recursively from PATH.
Only works where a POSIX \"find\" is available.

F-OR-D can be one of 'f 'files 'd 'dirs or 'both.  If anything
else, is given, everything is returned, including symlinks etc.

Optionally if RECURSE is non-nil recurse into the directories.
INCLUDE-HIDDEN includes hidden files and files in hidden
directories if non-nil.  Uses \"find\" shell command. Much faster
than using `directory-files-recursively'"
  (if (executable-find "find")
      (-remove #'string-empty-p (split-string
                               (shell-command-to-string
                                (format "find %s %s %s %s -print0" path
                                        (if include-hidden "" "-not -path '*/\\.*'")
                                        (pcase f-or-d
                                          ((or 'files 'f) "-type f")
                                          ((or 'dirs 'd) "-type d")
                                          ('both "\\( -type f -o -type d \\)")
                                          (_ ""))
                                        (if recurse "" "-maxdepth 1"))) "\0"))
    (message "\"find\" command not found.") nil))

;; Copied from https://www.emacswiki.org/emacs/DisabledCommands
(defun util/show-disabled-commands ()
  "Show commands that were disabled."
  (interactive)
  (with-output-to-temp-buffer "*Commands that were disabled*"
    (mapatoms
     (function
      (lambda (symbol)
        (when (get symbol 'disabled)
          (prin1 symbol)
          (princ "\n")))))))

(defun util/functions-matching-re (re &rest preds)
  "Return list of functions matching regexp RE.
Optional PREDS is a list of additional predicates to match for
atoms.  See `util/commands-matching-re' for example of PREDS."
  (let (atoms)
    (mapatoms (lambda (x)
                (when (and (fboundp x)
                           (or (not preds)
                               (funcall (-andfn preds) x))
                           (string-match-p re (symbol-name x)))
                  (push x atoms))))
    atoms))

(defun util/vars-matching-re (re &rest preds)
  "Return list of variables matching regexp RE.
Optional PREDS is a list of additional predicates to match for
atoms."
  (let (atoms)
    (mapatoms (lambda (x)
                (when (and (not (fboundp x))
                           (or (not preds)
                               (funcall (-andfn preds) x))
                           (string-match-p re (symbol-name x)))
                  (push x atoms))))
    atoms))

;; (defun util/commands-matching-re (re)
;;   "Return list of commands matching regexp RE."
;;   (let ((preds (-partial (lambda (x) (commandp (symbol-function x))))))
;;     (util/functions-matching-re re preds)))

;; FIXME: This is totally broken
;; (defun util/builtins-matching-re (re)
;;   "Return list of builtins matching regexp RE."
;;   (util/functions-matching-re re (lambda (x) (subrp (symbol-function x)))))

;; TODO
;; (defun util/run-python-file-from-str (str &optional python-path args)
;;   "Write the given python string STR to a temp file and run with python.
;; If optional PYTHON-PATH is given, that python executable is used
;; to run the script.  Optional ARGS is an list of arguments to
;; format the string."
;;   (let ((formatted-string (apply #'format (cons str args)))
;;         (python (or python-path (executable-find "python")))
;;         (tfile (make-temp-file "util-py-")))
;;     (with-temp-file tfile
;;       (insert formatted-string))
;;     ;; (make-process)
;;     ;; write temp file and run process with python
;;     ))


(provide 'util/misc)

;;; util-misc.el ends here

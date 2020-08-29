;;; util.el --- Various utility functions. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Wednesday 24 June 2020 09:16:15 AM IST>
;; Keywords:	utility
;; Version:     0.1
;; Package-Requires: ((org "9.1.9") (dash "2.17.0") (dash-functional "1.2.0") (bind-key "2.4") (sphinx-doc "0.3.0") (tern "0.0.1"))

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

;; A bunch of utility functions that I wrote in my Emacs config.  Some functions
;; are now required by other packages I've written so I've decided to make it a
;; separate package.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'dired)
(require 'ibuffer)
(require 'grep)
(require 'time-stamp)
(require 'tern)
(require 'imenu)
(require 'sphinx-doc)
(require 'org)
(require 'org-element)
(require 'package)

;; ibuffer functions
(defun util/ibuffer-copy-full-filenames-as-kill ()
  "Copy full buffer filename at point to kill ring without marking it.
Only copies buffer at point even if region is set.  Defaults to
copying marked buffers if there are any marked buffers."
  (interactive)
  (if (zerop (ibuffer-count-marked-lines))
      (kill-new (buffer-file-name (ibuffer-current-buffer t)))
    (ibuffer-copy-filename-as-kill 0)))

;; dired functions
(defun util/dired-copy-full-filename-as-kill ()
  "Copy the full filename at point in `dired'."
  (interactive)
  (dired-copy-filename-as-kill 0))

(defun util/dired-custom-sort (arg)
  "Sort the dired buffer according to ARG.
ARG can be 'time 'size or nil."
  (cond ((equal arg "time")
         (setq dired-listing-switches "-alht --group-directories-first")
         (setq dired-actual-switches "-alht --group-directories-first")
         (dired-sort-set-mode-line)
         (revert-buffer)
         (goto-char (point-min)))
        ((equal arg "size")
         (setq dired-listing-switches "-alhS --group-directories-first")
         (setq dired-actual-switches "-alhS --group-directories-first")
         (dired-sort-set-mode-line)
         (revert-buffer)
         (goto-char (point-min)))
        (t
         (setq dired-listing-switches "-alh --group-directories-first")
         (setq dired-actual-switches "-alh --group-directories-first")
         (dired-sort-set-mode-line)
         (revert-buffer)
         (goto-char (point-min)))))

(defun util/dired-custom-sort-size ()
  "Sort the `dired' buffer according to file size, descending order."
  (interactive)
  (util/dired-custom-sort "size"))

(defun util/dired-custom-sort-time ()
  "Sort the `dired' buffer according to modification time, descending order."
  (interactive)
  (util/dired-custom-sort "time"))

(defun util/dired-custom-sort-name ()
  "Default `dired' sort by name."
  (interactive)
  (util/dired-custom-sort nil))

(defun util/full-time-stamp ()
  "Insert full Time Stamp."
  (interactive)
  (insert (time-stamp-string "\"%:a %2d %:b %:y %02H:%02M:%02S %Z\"")))

;; DONE: occur like mode where the timestamps are sorted from latest to oldest
;;     : I made it :-)
;; NOTE: should be (interactive P)
;; CHECK: Why interactive p? I've forgotten the difference
(defun util/org-goto-latest-timestamp (&optional buf)
  "Goto latest timestamp in the current org buffer.
If optional BUF is given then search in that instead.  By default
search only in current subtree.  With a universal argument,
`\\[universal-argument]' search in full buffer."
  (interactive)
  (save-restriction
    (with-current-buffer (if buf buf (current-buffer))
      (unless current-prefix-arg
        (org-narrow-to-subtree))
      (goto-char (cdr (car (cl-sort
                            (org-element-map (org-element-parse-buffer) 'timestamp
                              (lambda (timestamp)
                                `(,(org-element-property :raw-value timestamp)
                                  . ,(org-element-property :begin timestamp))))
                            'org-time> :key 'car))))
      (org-reveal))))

(defun util/org-occur-sorted-timestamps ()
  "Run `occur' in the current org buffer for timestamps.
By default the `occur' is run for only the current subtree.  With
a universal argument, `\\[universal-argument]' run for full
buffer."
  (interactive)
  (save-restriction
    (progn
      (unless current-prefix-arg
        (org-narrow-to-subtree))
      (occur "\\[[0-9].*[0-9]\\]")
      (other-window 1)
      (read-only-mode -1)
      (goto-char 0)
      (forward-line)
      (sort-regexp-fields -1 "^.*$" "\\[[0-9].*[0-9]\\]" (point) (buffer-end 1))
      (read-only-mode))))

(defun util/org-num-finished ()
  "Message the number of FINISHED items in subtree."
  (interactive)
  (if (eq major-mode 'org-mode)
      (save-restriction
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (message (format "%s" (count-matches "\* FINISHED"))))
    (message "Not in org-mode")))

(defun util/try-copy-help-buffer-link ()
  "In *Help* buffer, copy the url under point if it exists."
  (interactive)
  (when (and (eq major-mode 'help-mode))
    (let ((maybe-string (caadr (text-properties-at (point)))))
      (if (stringp maybe-string)
          (progn
            (message maybe-string)
            (kill-new maybe-string))
        (message "No link under point")))))

(defun util/package-desc (pkg)
  "Return description of package PKG.
Copied from `describe-package-1'.  Returns of either installed
PKG or one available in package archives."
  (let* ((desc (or
                (if (package-desc-p pkg) pkg)
                (cadr (assq pkg package-alist))
                (let ((built-in (assq pkg package--builtins)))
                  (if built-in
                      (package--from-builtin built-in)
                    (cadr (assq pkg package-archive-contents))))))
         (name (if desc (package-desc-name desc) pkg))
         (pkg-dir (if desc (package-desc-dir desc)))
         (reqs (if desc (package-desc-reqs desc)))
         (required-by (if desc (package--used-elsewhere-p desc nil 'all)))
         (version (if desc (package-desc-version desc)))
         (archive (if desc (package-desc-archive desc)))
         (extras (and desc (package-desc-extras desc)))
         (homepage (cdr (assoc :url extras)))
         (commit (cdr (assoc :commit extras)))
         (keywords (if desc (package-desc--keywords desc)))
         (built-in (eq pkg-dir 'builtin))
         (installable (and archive (not built-in)))
         (status (if desc (package-desc-status desc) "orphan"))
         (incompatible-reason (package--incompatible-p desc))
         (signed (if desc (package-desc-signed desc)))
         (maintainer (cdr (assoc :maintainer extras)))
         (authors (cdr (assoc :authors extras))))
    desc))

;; package utility functions
(defun util/package-list-installed (pkg-regexp)
  "Return a list of all installed packages which match PKG-REGEXP."
  (util/sort-symbol-list
   (-non-nil (mapcar (lambda (x)
                       (when (and (package-installed-p (car x))
                                  (string-match pkg-regexp (symbol-name (car x))))
                         (car x)))
                     package-alist))))

(defun util/package-requires (pkg-name-or-symbol)
  "Return alist of symbols of installed packages required by PKG-NAME-OR-SYMBOL.
PKG-NAME-OR-SYMBOL can be a symbol or a string."
  (let* ((name (if (symbolp pkg-name-or-symbol)
                   pkg-name-or-symbol
                 (intern pkg-name-or-symbol)))
         (desc (when (assoc name package-alist)
                 (package-desc-reqs (cadr (assoc name package-alist))))))
    (when desc
      (mapcar (lambda (x)
                (cons (car x) (mapconcat #'number-to-string (cadr x) ".")))
              desc))))

(defun util/package-required-by (pkg-name-or-symbol &optional all)
  "Return alist of symbols of installed packages which require PKG-NAME-OR-SYMBOL.
PKG-NAME-OR-SYMBOL can be a symbol or a string."
  (let* ((name (if (symbolp pkg-name-or-symbol)
                   pkg-name-or-symbol
                 (intern pkg-name-or-symbol)))
         (reqs (mapcar (lambda (x)
                         (let ((desc (cadr x)))
                           (cons (package-desc-name desc) (package-desc-reqs desc))))
                       package-alist)))
    (mapcar 'car (-non-nil (mapcar (lambda (x)
                                     (let ((req-names (mapcar 'car (cdr x))))
                                       (when (member name req-names) x)))
                                   reqs)))))

(defun util/package-info (pkg-name-or-symbol)
  "Return plist of all the info for a given PKG-NAME-OR-SYMBOL.
PKG-NAME-OR-SYMBOL can be a symbol or a string."
  (let* ((name (if (symbolp pkg-name-or-symbol)
                   pkg-name-or-symbol
                 (intern pkg-name-or-symbol)))
         (desc (when (or (assoc name package-alist)
                         (assoc name package-archive-contents))
                 (util/package-desc name))))
    (when desc
      (list :summary (package-desc-summary desc)
            :version (mapconcat #'number-to-string (package-desc-version desc) ".")
            :directory (package-desc-dir desc)
            :extras (package-desc-extras desc)))))

(defun util/try-get-package-urls ()
  "Try and add to kill ring if there's a package url under point.
If a region is active then get all posssible in the region."
  (interactive)
  (when (and (eq major-mode 'package-menu-mode))
    (if (region-active-p)
        (save-restriction
          (save-excursion
            (let ((url-list nil))
              (narrow-to-region (region-beginning) (region-end))
              (goto-char (point-min))
              (while (not (eobp))
                (setq url-list (push (util/try-get-package-url) url-list))
                (forward-line))
              (message (format "%s" url-list))
              (kill-new (mapconcat (lambda (x) (format "%s" x)) url-list " ")))))
      (kill-new (util/try-get-package-url)))))

(defun util/try-get-package-url ()
  "Try and get single a package url under point."
  (when (and (eq major-mode 'package-menu-mode))
    (let ((pkg-desc (tabulated-list-get-id (point))))
      (when (and pkg-desc (package-desc-extras pkg-desc))
        (message (cdr (assq :url (package-desc-extras  pkg-desc))))
        (cdr (assq :url (package-desc-extras  pkg-desc)))))))

(defun util/rgrep-default-search (regexp)
  "`rgrep' for REGEXP in current directory for files with current extension.
Only the REGEXP pattern is asked on the prompt."
  (interactive (list (let ((phrase (thing-at-point 'symbol t)))
                       (read-from-minibuffer (format "Regexp (default %s): " phrase)
                                             nil nil t nil phrase))))
  (unless (stringp regexp)
    (setq regexp (format "%s" regexp)))
  (let* ((fname (buffer-file-name))
         (dir (file-name-directory fname))
         (files (concat "*." (car (last (split-string fname "\\."))))))
    (eval-after-load "grep"
      '(grep-compute-defaults))
    (rgrep regexp files dir)))

;; FIXME: What does it do?
(defun util/clean-generated-org-buf ()
  "Don't recall really."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (outline-show-branches)
    (outline-next-heading)
    (outline-next-heading)
    (kill-line 2)
    (goto-char (point-min))
    (while (re-search-forward "â\\|’" nil t)
      (replace-match "'"))
    (while (search-forward "–" nil t)
      (replace-match "--"))))

;; FIXME: This thing is still buggy
(defun util/python-listify-lines-in-region ()
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

(defun util/delete-blank-lines-in-region ()
  (interactive)
  (when (region-active-p)
    (save-restriction
      (let ((beg (region-beginning))
            (end (region-end)))
        (narrow-to-region beg end)
        (delete-trailing-whitespace)
        (goto-char (point-min))
        (while (re-search-forward "^[ ]+\n" nil t)
          (replace-match ""))
        (goto-char (point-min))
        (re-search-forward "\r?\n+\n" nil t)
        (replace-match "")
        (while (re-search-forward "\r?\n+\n" nil t)
          (replace-match "\n"))
        (goto-char (point-max))))))

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

(defun util/get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE."
  (let (buffer-mode-matches)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (push buf 'buffer-mode-matches))))
    buffer-mode-matches))

(defun util/get-or-create-window-on-side ()
  "Get the window on side if it exists else create it."
  (let* ((orig-win (selected-window))
         (win (cond ((window-in-direction 'right orig-win)
                     (window-in-direction 'right orig-win))
                    ((window-in-direction 'left orig-win)
                     (window-in-direction 'left orig-win))
                    (t (split-window-horizontally)))))
    win))

(defun util/tern-find-definition-other-window (&optional prompt-var)
  (interactive)
  (let ((varname (and (or prompt-var (not (tern-at-interesting-expression)))
                      (read-from-minibuffer "Variable: "))))
    (push-mark)
    (tern-run-query #'util/tern-show-definition
                    `((type . "definition") (variable . ,varname)) (point))))

(defun util/tern-show-definition (data)
  (let* ((file (cdr (assq 'file data)))
         (found (and file (setf file (expand-file-name (cdr (assq 'file data)) (tern-project-dir)))
                     (tern-find-position file data))))
    (if found
        (progn
          (push (cons (buffer-file-name) (point)) tern-find-definition-stack)
          (let ((too-long (nthcdr 20 tern-find-definition-stack)))
            (when too-long (setf (cdr too-long) nil)))
          (let ((buf (current-buffer))
                ;; CHECK: Why isn't this used?
                ;; (win (util/get-or-create-window-on-side))
                )
            (with-current-buffer buf
              (setf tern-last-point-pos (point)))
            (find-file-other-window file)
            (goto-char (min found (point-max)))))
      (let ((url (cdr (assq 'url data))))
        (if url
            (browse-url url)
          (tern-message "No definition found."))))))

;; FIXME: WTF does this even do?
(defun util/get-links-from-region ()
  "Get `shr' links from a buffer."
  (interactive)
  (let ((url-list nil))
    (while (thing-at-point (quote sentence))
      (push (get-text-property (point) 'shr-url) url-list)
      (forward-line))
    url-list))

(defun util/multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (util/get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun util/path-join (&rest elements)
  "Safely concatenate path ELEMENTS in a consistent manner."
  (concat "/" (mapconcat (lambda (x)
                           (string-remove-prefix "/" (string-remove-suffix "/" x)))
                         elements "/")))

(defun firstn (x n)
  "Return first N elements of a list X."
  (butlast x (- (length x) n)))

(defun butfirst (x &optional n)
  "Return all but first N elements of a list X."
  (let ((n (if n n 1)))
    (last x (- (length x) n))))

(defun util/max-ind (seq)
  (let* ((my/max-val 0) (my/ind -1) (my/max 0))
    (cl-loop for x in seq
          do
          (progn
            (setq my/ind (+ 1 my/ind))
            (if x (if (> x my/max-val)
                      (progn (setq my/max-val x)
                             (setq my/max my/ind))))))
    my/max))

(defun util/trim (str)
  "Trims the string and replaces multiple spaces with a single one."
  (util/replace-in-string (string-trim str) "[ ]+" " "))


;; custom replace string to work on rectangles
(defun replace-string (from-string to-string &optional delimited start end backward
                       region-noncontiguous-p)
  "...
Argument FROM-STRING input string.
Argument TO-STRING target string."
  (declare (interactive-only
            "use `search-forward' and `replace-match' instead."))
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Replace"
                   (if current-prefix-arg
                       (if (eq current-prefix-arg '-) " backward" " word")
                     "")
                   " string"
                   (if (use-region-p) " in region" ""))
           nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           (if (use-region-p) (region-beginning))
           (if (use-region-p) (region-end))
           (nth 3 common)
           (if (use-region-p) (region-noncontiguous-p)))))
  (perform-replace
   from-string to-string nil nil delimited nil nil
   start end backward region-noncontiguous-p))

(defun util/replace-in-string (in what with)
  "Hackey function to replace WHAT with WITH in string IN."
  (replace-regexp-in-string
   (regexp-quote what) with in nil 'literal))

(defun util/sort-string-list (list)
  "Sort a LIST of strings."
  (-sort (lambda (x y) (string-lessp x y)) list))

(defun util/sort-symbol-list (list)
  "Sort a LIST of symbols as if they're words."
  (-sort (lambda (x y) (string-lessp (symbol-name x) (symbol-name y))) list))

(defun util/sort-words ()
  "Sort words in region alphabetically, in REVERSE if negative.

Prefixed with a \\[universal-argument], sorts in reverse.
The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.  See `sort-regexp-fields'."
  (interactive)
  (when (region-active-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (sort-regexp-fields current-prefix-arg "\\w+" "\\&" beg end))))

(defun util/sort-symbols ()
  "Sort symbols in region alphabetically, in REVERSE if negative.
Prefixed with a \\[universal-argument], sorts in reverse.  See
`util/sort-words'."
  (interactive)
  (when (region-active-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (sort-regexp-fields current-prefix-arg "\\(\\sw\\|\\s_\\)+" "\\&" beg end))))

;; From https://www.emacswiki.org/emacs/ImenuMode#toc14
;; Need recursive function here to fix it
;; class def doesn't appear in this, nor do nested defs
;; inner most def appears
;; two levels of nesting also appear, but top level doesn't
(defvar util//selected-symbol nil)
(defvar util//name-and-pos nil)
(defvar util//symbol-names nil)
(defvar ido-enable-flex-matching)
(defun util/ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido.
Optional argument SYMBOL-LIST is used for recursion when the function calls itself a second time."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching t)
          util//symbol-names util//name-and-pos position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (util/ido-goto-symbol (imenu--make-index-alist))
               (setq util//selected-symbol
                     (ido-completing-read "Symbol? " util//symbol-names))
               (string= (car imenu--rescan-item) util//selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc util//selected-symbol util//name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (util/ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'util//symbol-names name)
          (add-to-list 'util//name-and-pos (cons name position))))))))

;; CHECK: perhaps should narrow to region in another buffer
(defun util/calc-sruthi ()
  "My utility function to calculate Sruthi's debt."
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


(defun util/library-version (lib)
  "Return the library version for LIB.
LIB can be symbol or library name.  It searches for the version
string in the file where the library is defined"
  (let ((str (condition-case nil
                 (shell-command-to-string
                  (format "grep -i \";; version\" %s"
                          (find-library-name (if (symbolp lib) (symbol-name lib) lib))))
               (error nil))))
    (when str
      (string-match ";; version: \\(.*\\)" str)
      (when (match-string 1 str)
        (string-trim (match-string 1 str))))))


(defmacro util/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defmacro util/measure-time-n (n &rest body)
  "Measure the time it takes to evaluate BODY.
Sum over N iterations."
  `(let ((time (current-time)))
     (dotimes (i ,n)
       ,@body)
     (message "%.06f" (float-time (time-since time)))))

;; NOTE: Old implementation didn't make sense. It should be &rest body
;; (defmacro util/measure-time (body &optional n)
;;   "Measure the time it takes to evaluate BODY.
;; Optionally repeat N times."
;;   `(let ((time (current-time))
;;          (n ,(if n n 1)))
;;      (dotimes (i n)
;;        ,body)
;;      (message "%.06f" (float-time (time-since time)))))


(defun util/fast-files-or-dirs (path f-or-d &optional recurse include-hidden)
  "Get all files or dirs or both or everything, recursively from PATH.

F-OR-D can be one of 'f 'files 'd 'dirs or 'both.  If anything
else, is given, everything is returned, including symlinks etc.

Optionally if RECURSE is non-nil recurse into the directories.
INCLUDE-HIDDEN includes hidden files and files in hidden
directories if non-nil.  Uses \"find\" shell command. Much faster
than using `directory-files-recursively'"
  (-remove #'string-empty-p (split-string
                             (shell-command-to-string
                              (format "find %s %s %s %s -print0" path
                                      (if include-hidden "" "-not -path '*/\\.*'")
                                      (pcase f-or-d
                                        ((or 'files 'f) "-type f")
                                        ((or 'dirs 'd) "-type d")
                                        ('both "\\( -type f -o -type d \\)")
                                        (_ ""))
                                      (if recurse "" "-maxdepth 1"))) "\0")))


(defvar util/no-capitalize
  '("a" "an" "and" "are" "as" "at" "by" "can" "did" "do" "does" "for" "from" "had" "has" "have" "having" "here" "how" "in" "into" "is" "it" "it's" "its" "not" "of" "on" "over" "should" "so" "than" "that" "the" "then" "there" "these" "to" "was" "were" "what" "when" "where" "which" "who" "why" "will" "with"))
(defvar util/no-capitalize-bigger
  '("me" "my" "myself" "we" "our" "ours"
    "ourselves" "you" "you're" "you've" "you'll" "you'd" "your" "yours" "yourself"
    "yourselves" "he" "him" "his" "himself" "she" "she's" "her" "hers" "herself"
    "it" "it's" "its" "itself" "they" "them" "their" "theirs" "themselves" "what"
    "which" "who" "whom" "this" "that" "that'll" "these" "those" "am" "is" "are"
    "was" "were" "be" "been" "being" "have" "has" "had" "having" "do" "does" "did"
    "doing" "a" "an" "the" "and" "but" "if" "or" "because" "as" "until" "while"
    "of" "at" "by" "for" "with" "about" "against" "between" "into" "through"
    "during" "before" "after" "above" "below" "to" "from" "up" "down" "in" "out"
    "on" "off" "over" "under" "again" "further" "then" "once" "here" "there" "when"
    "where" "why" "how" "all" "any" "both" "each" "few" "more" "most" "other"
    "some" "such" "no" "nor" "not" "only" "own" "same" "so" "than" "too" "very" "s"
    "t" "can" "will" "just" "don" "don't" "should" "should've" "now" "ain't" "aren"
    "aren't" "couldn" "couldn't" "didn" "didn't" "doesn" "doesn't" "hadn" "hadn't"
    "hasn" "hasn't" "haven" "haven't" "isn" "isn't" "ma" "mightn" "mightn't"
    "mustn" "mustn't" "needn" "needn't" "shan" "shan't" "shouldn't" "wasn't"
    "weren't" "won't" "wouldn't"))
(defun util/title-case (&optional str)
  "Convert STR to title case.
If called interactively, then transform the active region to
title case.  Words to ignore are determined by a NO-CAPITALIZE
list.  See `util/no-capitalize' and `util/no-capitalize-bigger'."
  (interactive)
  (when (called-interactively-p 'any)
    (if (region-active-p)
        (setq str (buffer-substring-no-properties
                   (region-beginning) (region-end)))
      (setq str "")))
  (let* ((split (split-string str))
        (result nil)
        (capitalize-next nil)
        (count 0)
        (result (progn
                  (cl-loop for x in split
                           do
                           (if (and (member x util/no-capitalize)
                                    (not capitalize-next)
                                    (not (= count 0))) ; first word
                               (push x result)
                             (push (capitalize x) result))
                           (if (string-match-p "[\.:]$" x) ; end of clause or sentence
                               (setq capitalize-next t)
                             (setq capitalize-next nil))
                           (cl-incf count))
                  (mapconcat 'identity (reverse result) " "))))
    (when (and (called-interactively-p 'any) str)
      (let ((start (region-beginning))
            (end (region-end)))
        (delete-region start end)
        (goto-char start)
        (insert result)))
    result))

(defun util/make-long-line ()
  "Convert a paragraph to one long line.
Removes all spaces and newlines with a single space and removes
space between all isolated punctuation characters and previous
word."
  (interactive)
  (save-excursion
    (save-restriction
      (when (region-active-p)
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        (when (looking-at "[ \t\n]+")
          (replace-match ""))
        (while (re-search-forward "[ \t\n]+" nil t nil)
          (replace-match " "))
        (re-search-backward "[ \t\n]+" nil t 1)
        (replace-match "")
        (goto-char (point-min))
        (while (re-search-forward "\\(\\w\\) \\([[:punct:]] \\)" nil t nil)
          (replace-match "\\1\\2"))))))

(provide 'util)

;;; util.el ends here


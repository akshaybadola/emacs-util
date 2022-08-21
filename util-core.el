;;; util-core.el --- Core utility functions. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020,2021,2022
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Tuesday 17 May 2022 04:14:55 AM IST>
;; Keywords:	utility, convenience, emacs-lisp, org, helm
;; Version:     0.4.0
;; Package-Requires: ((a) (dash) (f) (string-inflection))

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

;; Core utility functions with minimal dependencies.

;;; Code:


(require 'a)
(require 'cl-lib)
(require 'dash)
(require 'dired)
(require 'f)
(require 'grep)
(require 'gv)
(require 'ibuffer)
(require 'imenu)
(require 'org)                          ; Only used in `util/ido-goto-symbol'
(require 'package)
(require 'time-stamp)
(require 'string-inflection)


(defvar parse-time-weekdays)

;; All these for `util/ido-goto-symbol'
(defvar util//selected-symbol nil)
(defvar util//name-and-pos nil)
(defvar util//symbol-names nil)
(defvar ido-enable-flex-matching)

(defvar util/no-capitalize-small
  '("a" "an" "and" "are" "as" "at" "by" "can" "did" "do" "does" "for" "from" "had" "has" "have" "having" "here" "how" "in" "into" "is" "it" "it's" "its" "not" "of" "on" "over" "should" "so" "than" "that" "the" "then" "there" "these" "to" "was" "were" "what" "when" "where" "which" "who" "why" "will" "with")
  "List of words not to capitalize for `util/title-case'.")

(defvar util/no-capitalize-big
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
    "weren't" "won't" "wouldn't")
  "A bigger list of words not to capitalize for `util/title-case'.")

(defvar util/no-capitalize-list util/no-capitalize-small
  "Default value of list of words not to capitalize for `util/title-case'.")

(defvar util/stop-words util/no-capitalize-big
  "Default value of list of stop words.")


;; Some macros

(defmacro util/with-check-mode (mode msg-prefix &rest body)
  "Execute BODY only if `major-mode' equals MODE.
Otherwise message not in MODE.  MSG-PREFIX can be used to indicate
the package from which the macro is invoked.  If MSG-PREFIX is
nil, then nothing is prefixed to the message."
  (declare (debug t))
  `(if (eq major-mode ,mode)
       (progn ,@body)
     (message "%sNot in %s"
              (if ,msg-prefix (concat ,msg-prefix " ") "")
              ,mode)))

(defmacro util/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  (declare (debug t))
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defmacro util/measure-time-n (n &rest body)
  "Measure the time it takes to evaluate BODY.
Sum over N iterations."
  (declare (debug t))
  `(let ((time (current-time)))
     (dotimes (i ,n)
       ,@body)
     (message "Mean runtime %.06f seconds over %s runs"
              (/ (float-time (time-since time)) ,n) ,n)))

(defun util/check-mode (mode msg-prefix)
  "Alternative function to check MODE.
Message with MSG-PREFIX if not in MODE."
  (unless (eq major-mode mode)
    (user-error "%sNot in %s" msg-prefix mode)))
(make-obsolete 'util/check-mode 'util/with-check-mode "util/core 0.3.12")


(defun util/pairs-to-alist (pairs)
  "Merge cons PAIRS into an alist with first elements as keys.

The head of the list is the associative element.

Example:
    (pairs-to-alist '((a b) (b c d) (a d) (e f)))
     => '((a b d) (b c d) (e f))"
  (declare (pure t) (side-effect-free t))
  (when (and (consp pairs) (a-assoc pairs))
    (let (newlist)
      (seq-do (lambda (x)
                (if (a-has-key newlist (car x))
                    (setq newlist (a-update newlist (car x)
                                            (lambda (y) (-flatten (list (cdr x) y)))))
                  (push (cons (car x) (cdr x)) newlist)))
              pairs)
      newlist)))


(defun util/ibuffer-copy-full-filenames-as-kill ()
  "Copy full buffer filename at point to kill ring without marking it.
Only copies buffer at point even if region is set.  Defaults to
copying marked buffers if there are any marked buffers."
  (interactive)
  (if (zerop (ibuffer-count-marked-lines))
      (kill-new (buffer-file-name (ibuffer-current-buffer t)))
    (ibuffer-copy-filename-as-kill 0)))


;; From https://www.emacswiki.org/emacs/ImenuMode#toc14
;; Need recursive function here to fix it
;; class def doesn't appear in this, nor do nested defs
;; inner most def appears
;; two levels of nesting also appear, but top level doesn't
(defun util/ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido.
Optional argument SYMBOL-LIST is used for recursion when the
function calls itself a second time."
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
        (goto-char (overlay-start position))
        (when (eq major-mode 'org-mode)
          (org-reveal)))
       (t (goto-char position)
          (when (eq major-mode 'org-mode)
          (org-reveal))))))
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


;; `dired' functions

;; FIXME: The region code copies the last file also even though the filename
;;        itself may not intersect with region, which is not the same as
;;        behaviour with mark. Should fix that.
(defun util/dired-copy-full-filename-as-kill ()
  "Copy the full filename at point in `dired'."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (let ((beg (region-beginning))
              (end (region-end))
              files)
          (goto-char beg)
          (while (< (point) end)
            ;; Skip subdir line and following garbage like the `total' line:
            (while (and (< (point) end) (dired-between-files))
	      (forward-line 1))
            (when (and (not (looking-at-p dired-re-dot))
	               (push (dired-get-filename nil t) files)))
            (forward-line 1))
          (when files
            (setq files (string-join (-remove #'string-empty-p files) "\n"))
            (kill-new files)
            (message "%s" files)))
      (dired-copy-filename-as-kill 0))))

(defun util/dired-custom-sort (arg)
  "Sort the Dired buffer according to ARG.
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


;; Time stamp functions

(defun util/org-time-stamp-regexp (ts-format)
  "Return regexp for an org `time-stamp' format TS-FORMAT.
See `time-stamp-format' for how to use the format."
  (let ((pattern-list '(("%Y" . "[0-9]\\\\{4\\\\}")
                        ("%m" . "[0-9]\\\\{2\\\\}")
                        ("%d" . "[0-9]\\\\{2\\\\}")
                        ("%a" . "[A-Z]\\\\{1\\\\}[a-z]\\\\{2\\\\}")
                        ("%H" . "[0-9]\\\\{2\\\\}")
                        ("%M" . "[0-9]\\\\{2\\\\}")
                        ("<" . "[<\\\\|[]")
                        (">" . "[]\\\\|>]")))
        (retval ts-format))
    (dolist (pattern pattern-list)
      (setq retval (replace-regexp-in-string (car pattern) (cdr pattern) ts-format)))
    retval))

(defun util/generate-org-ts-regexp (ts-list)
  "Generate a single regexp from a `time-stamp' formats list TS-LIST."
  ;; Hack for converting `org-time-stamp-formats' which is a cons cell

  ;; NOTE: Test to check. Should be equal
  ;; (string= (concat "\\(" (util/org-time-stamp-regexp "<%Y-%m-%d %a>") "\\)" "\\|"
  ;;                "\\(" (util/org-time-stamp-regexp "<%Y-%m-%d %a %H:%M>") "\\)")
  ;;        (util/generate-org-ts-regexp org-time-stamp-formats))
  ;;
  ;; (string-match-p (concat "\\(" (util/org-time-stamp-regexp "<%Y-%m-%d %a>") "\\)" "\\|"
  ;;                       "\\(" (util/org-time-stamp-regexp "<%Y-%m-%d %a %H:%M>") "\\)")
  ;;               "[2020-09-10 Thu 10:10]")
  ;;
  ;; (string-match-p (util/generate-org-ts-regexp) "[2020-09-10 Thu 10:10]")
  ;; (string-match-p (util/generate-org-ts-regexp) "[2020-09-10 Thu 10:10>")
  ;; (string-match-p (util/generate-org-ts-regexp) "<2020-09-10 Thu 10:10>")
  ;; (string-match-p (util/generate-org-ts-regexp) "<2020-09-10 Thu 10:10]")
  ;; (not (string-match-p (util/generate-org-ts-regexp) "<2020-09-10 Thu 10:10a]"))
  ;; (not (string-match-p (util/generate-org-ts-regexp) "<020-09-10 Thu 10:10a]"))
  ;; (not (string-match-p (util/generate-org-ts-regexp) "<2020-09-10 thu 10:10a]"))
  ;; etc.
  (unless (listp (cdr ts-list))
    (setq ts-list (list (car ts-list) (cdr ts-list))))
  (mapconcat (lambda (x)
              (concat "\\(" (util/org-time-stamp-regexp x) "\\)"))
             ts-list "\\|"))

;; TODO: Check the time stamp format and only then update accordingly
;;       Convert to full in same format <>, [] or "".
(defun util/full-time-stamp ()
  "Insert or update the full Time Stamp."
  (interactive)
  ;; get maybe-time-stamp-at-point
  ;; when time-stamp-at-point get pattern
  ;; backup time-stamp-format and set new one
  ;; when override, override to new default pattern or keep existing pattern
  ;; insert or update pattern
  ;; restore time-stamp-format
  ;;(let (time-stamp-format "\"%:a %2d %:b %:y %02H:%02M:%02S %Z\""))
  (insert (time-stamp-string "\"%:a %2d %:b %:y %02H:%02M:%02S %Z\"")))


;; TODO
;; (defun util/upate-time-stamps-in-project ()
;;   )

;; NOTE: util/time-stamp-start etc. were written to update TS like
;;       [2021-07-28 Wed 15:12] at point but `org-time-stamp' does a better job
;;       of it
;;
;; (defvar util/time-stamp-start "\\([<[]\\)")
;; (defvar util/time-stamp-end "\\([]>]\\)")

(defun util/insert-or-update-time-stamp-at-point (&optional ts-format)
  "Insert or update a time stamp at point.
Default is to use a time stamp like `date' command.  With
optional TS-FORMAT, use that `time-stamp-format'."
  (interactive)
  (save-excursion
    (pcase-let* ((cur-point (point))
                 (`(,beg ,end) (list (re-search-backward time-stamp-start (point-at-bol) t)
                                     (re-search-forward time-stamp-end (point-at-eol) t)))
                 (time-stamp-format (or ts-format "%:A %02d %:B %Y %02H:%02M:%02S %P %Z")))
      ;; (unless (and beg end)
      ;;   (setq beg (re-search-backward util/time-stamp-start (point-at-bol) t))
      ;;   (setq end (re-search-forward util/time-stamp-end (point-at-eol) t)))
      (if (and beg end)
          (time-stamp)
        (goto-char cur-point)
        (insert (format "<%s>" (time-stamp-string)))))))

(defun util/insert-or-update-today ()
  "Insert or update time stamp with format `[%:y-%02m-%02d]' at point.
See `time-stamp-string' for details."
  (interactive)
  (save-excursion
    (cond ((looking-back "\\]" (point-at-bol))
           (backward-sexp))
          ((looking-at-p "[0-9-]\\|\\]")
           (backward-up-list))
          (t nil))
    (when (looking-at "\\[[0-9-]+]")
      (apply #'delete-region (match-data))))
  (insert (time-stamp-string "[%:y-%02m-%02d]")))

;; TODO:
;; (defun util/goto-latest-time-stamp-open-buffers ()
;;   "Goto the latest `time-stamp'.
;; Like `util/org-goto-latest-timestamp' but for all buffers."
;;   )

;; TODO:
;; (defun util/occur-sorted-timestamps-open-buffers ()
;;   "Goto the latest `time-stamp'.
;; Like `util/org-occur-sorted-timestamps' but for all buffers."
;;   )

(defun util/decode-time-stamp (ts)
  "Similar to `decode-time' but for time stamp TS."
  (pcase-let* ((`(,date ,day ,time) (split-string (substring ts 1 -1) " "))
               (`(,dy ,dm ,dd) (mapcar #'string-to-number (split-string date "-")))
               (`(,hh ,mm) (mapcar #'string-to-number (split-string time ":"))))
    (list 0 mm hh dd dm dy (a-get parse-time-weekdays (downcase day)) nil nil)))

(defun util/time-stamp-less-p (A B)
  "Similar to `time-less-p' but for time stamps A and B."
  (time-less-p (encode-time (util/decode-time-stamp A))
               (encode-time (util/decode-time-stamp B))))

(defmacro util/with-open-file-as-needed (file &rest body)
  (declare (debug t) (indent 1))
  `(let ((buf (find-buffer-visiting ,file)))
     (if buf
         (with-current-buffer buf
           ,@body
           (basic-save-buffer))
       (setq buf (find-file-noselect ,file))
       (with-current-buffer buf
         ,@body
         (basic-save-buffer)
         (kill-buffer)))))


(defun util/increment-version (buf &optional place)
  "Increment version number in buffer BUF.

Increment at optional PLACE if given, else the last digit is
incremented.  Only works for numeric versions like \"0.2.0\"."
  (with-current-buffer buf
    (let* ((version-re "^;; Version:.+?([0-9\\.]+)")
           (version (save-excursion
                      (goto-char (point-min))
                      (re-search-forward
                       (replace-regexp-in-string
                        "(" "\\\\("
                        (replace-regexp-in-string ")" "\\\\)" version-re)))
                      (save-match-data
                        (mapcar #'string-to-number
                                (split-string (substring-no-properties (match-string 1)) "\\.")))))
           (place (or place (- (length version) 1))))
      (setf (nth place version) (+ 1 (nth place version)))
      (replace-match (string-join (mapcar #'number-to-string version) ".") nil nil nil 1))))

;; TODO: This uses `git ls-files' instead of `util/ffip-search'
;; TODO: Should update time stamps only in modified files
;; TODO: What about the version strings?
(defun util/update-time-stamp-in-project-files (&optional pat update-version)
  "Update `time-stamp' in all files in project for a given pattern PAT.

Files are given by `util/ffip-search' which excludes certain
paths and files by default.  See `util/ffip-search' for details."
  (interactive)
  (let* ((pat (or pat
                  (read-from-minibuffer
                   (format "Enter the file pattern (default %s): " ".*el$")
                   nil nil nil nil ".*$")))
         (modified-files (split-string
                          (shell-command-to-string
                           (format "git ls-files -m | grep \"%s\"" pat))
                          "\n" t " "))
         (all-files (split-string
                     (shell-command-to-string
                      (format "git ls-files | grep \"%s\"" pat))
                     "\n" t " "))
         (version-re "^;; Version:.+?([0-9\\.]+)")
         (version-files (mapcar (lambda (x) (car (split-string x ":")))
                                (split-string
                                 (shell-command-to-string
                                  (format "egrep \"%s\" %s" version-re
                                          (string-join all-files " ")))
                                 "\n" t))))
    ;; Update copyright and `time-stamp' in modified files
    (seq-do (lambda (f)
              (util/with-open-file-as-needed f
                (copyright-update nil t)
                (time-stamp)))
            modified-files)
    ;; Update `version' in project files
    ;; Works only for lisp projects and numerical versions
    (when (or update-version current-prefix-arg)
      (if (= (length version-files) 1)
          (util/with-open-file-as-needed (car version-files)
            (copyright-update nil t)
            (time-stamp)
            (util/increment-version (current-buffer)
                                    (and current-prefix-arg (numberp current-prefix-arg))))
        (seq-do (lambda (f)
                  (util/with-open-file-as-needed f
                    (util/increment-version (current-buffer)
                                            (and current-prefix-arg (numberp current-prefix-arg)))))
                modified-files)))))




;; `package' functions

(defun util/package-desc (pkg)
  "Return description of package PKG.
Copied from `describe-package-1'.  Returns description of either
installed PKG or one available in package archives."
  (let* ((desc (or
                (when (package-desc-p pkg) pkg)
                (cadr (assq pkg package-alist))
                (when (stringp pkg) )
                (let ((built-in (assq pkg package--builtins)))
                  (if built-in
                      (package--from-builtin built-in)
                    (cadr (assq pkg package-archive-contents))))))
         ;; (name (if desc (package-desc-name desc) pkg))
         ;; (pkg-dir (if desc (package-desc-dir desc)))
         ;; (reqs (if desc (package-desc-reqs desc)))
         ;; (required-by (if desc (package--used-elsewhere-p desc nil 'all)))
         ;; (version (if desc (package-desc-version desc)))
         ;; (archive (if desc (package-desc-archive desc)))
         ;; (extras (and desc (package-desc-extras desc)))
         ;; (homepage (cdr (assoc :url extras)))
         ;; (commit (cdr (assoc :commit extras)))
         ;; (keywords (if desc (package-desc--keywords desc)))
         ;; (built-in (eq pkg-dir 'builtin))
         ;; (installable (and archive (not built-in)))
         ;; (status (if desc (package-desc-status desc) "orphan"))
         ;; (incompatible-reason (package--incompatible-p desc))
         ;; (signed (if desc (package-desc-signed desc)))
         ;; (maintainer (cdr (assoc :maintainer extras)))
         ;; (authors (cdr (assoc :authors extras)))
         )
    ;; (cons (list :name name :pkg-dir pkg-dir :reqs reqs :required-by required-by
    ;;             :version version :archive archive :extras extras :homepage
    ;;             homepage :commit commit :keywords keywords :built-in built-in
    ;;             :installable installable :status status :incompatible-reason
    ;;             incompatible-reason :signed signed :maintainer maintainer
    ;;             :authors authors))
    desc))

(defun util/package-list (pkg-regexp &optional builtins archives not-installed)
  "Return a list of packages which match PKG-REGEXP.
By default search only in installed packages.

When optional BUILTINS is non-nil, search in `package--builtins'
also.  With non-nil ARCHIVES search in package archives also.
Doesn't refresh the archives contents.

Optional NOT-INSTALLED when non-nil is used to exclude installed
packages from the search."
  (util/sort-symbol-list
   (-uniq (-non-nil (mapcar (lambda (x) (when (string-match pkg-regexp (symbol-name (car x)))
                                          (car x)))
                            (-concat (and builtins package--builtins) (unless not-installed
                                                                        package-alist)
                                     (and archives package-archive-contents)))))))

(defun util/package-list-all (pkg-regexp)
  "Return a list of all packages which match PKG-REGEXP."
  (util/package-list pkg-regexp t t))

(defun util/package-list-installed (pkg-regexp)
  "Return a list of only installed packages which match PKG-REGEXP."
  (util/package-list pkg-regexp nil nil))

(defun util/package-list-available (pkg-regexp)
  "Return a list of only available packages which match PKG-REGEXP."
  (util/package-list pkg-regexp nil t t))

;; FIXME: builtins aren't being listed correctly
;; (defun util/package-list-builtins (pkg-regexp)
;;   "Return a list of only available packages which match PKG-REGEXP."
;;   (util/package-list pkg-regexp t nil t))

(defun util/package-list-activated (pkg-regexp)
  "Return a list of loaded or activated packages which match PKG-REGEXP."
  (-filter (lambda (x) (string-match-p pkg-regexp (symbol-name x))) package-activated-list))

(defun util/package-requires (pkg-name-or-symbol)
  "Return alist of (symbol . version) of packages required by PKG-NAME-OR-SYMBOL.
PKG-NAME-OR-SYMBOL can be a symbol or a string and can be an
installed or available package."
  (let* ((name (if (symbolp pkg-name-or-symbol)
                   pkg-name-or-symbol
                 (intern pkg-name-or-symbol)))
         (pkg-list (-concat package-alist package-archive-contents))
         (desc (when (assoc name pkg-list)
                 (package-desc-reqs (cadr (assoc name pkg-list))))))
    (when desc
      (mapcar (lambda (x)
                (cons (car x) (mapconcat #'number-to-string (cadr x) ".")))
              desc))))

(defun util/package-delete-packages (pkg-list)
  "Delete all packages given in PKG-LIST without confirmation.
Uses `package-delete'.  PKG-LIST is a list of symbols.  Return
the results."
  (seq-map (lambda (x)
             (when (assoc x package-alist)
               (package-delete (cadr (assoc x package-alist)))))
           pkg-list))

(defun util/package-required-by (pkg-name-or-symbol &optional all)
  "Return alist of symbols of installed packages which require PKG-NAME-OR-SYMBOL.
PKG-NAME-OR-SYMBOL can be a symbol or a string.  With optional
argument ALL, return dependent packages from archives also (not implemented)."
  (let* ((name (if (symbolp pkg-name-or-symbol)
                   pkg-name-or-symbol
                 (intern pkg-name-or-symbol)))
         (reqs (mapcar (lambda (x)
                         (let ((desc (cadr x)))
                           (cons (package-desc-name desc) (package-desc-reqs desc))))
                       package-alist)))
    (when all (message "ALL not implemented"))
    (mapcar 'car (-non-nil (mapcar (lambda (x)
                                     (let ((req-names (mapcar 'car (cdr x))))
                                       (when (member name req-names) x)))
                                   reqs)))))

(defun util/package-top-level-packages (&optional sort-by order)
  "Return list of packages which are not a dependency of any other package.

Optional SORT-BY specifies how to sort-by the returned list.  If
'time then sort by installed date.  Return alphabetically sorted
by default.

Optioanl ORDER specifies whether to sort-by in 'ascending or
'descending order."
  (let* ((packages (-non-nil (mapcar (lambda (x)
                      (unless (util/package-required-by (car x)) (car x)))
                                     package-alist))))
    (setq packages
          (pcase sort-by
            ('time (mapcar
                    #'car
                    (-sort (lambda (y z) (time-less-p (cdr y) (cdr z)))
                           (mapcar
                            (lambda (pkg)
                              `(,pkg . ,(file-attribute-modification-time
                                         (file-attributes
                                          (-first (lambda (x) (string-match-p ".elc$" x))
                                                  (f-files (plist-get (util/package-info pkg) :directory)))))))
                            packages))))
            (_ packages)))
    (if (eq order 'descending) packages (reverse packages))))

(defun util/package-version (lib)
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

(defun util/package-try-get-package-urls ()
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
                (setq url-list (push (util/package-try-get-package-url) url-list))
                (forward-line))
              (message (format "%s" url-list))
              (kill-new (mapconcat (lambda (x) (format "%s" x)) url-list " ")))))
      (kill-new (util/package-try-get-package-url)))))

(defun util/package-try-get-package-url ()
  "Try and get single a package url under point."
  (when (and (eq major-mode 'package-menu-mode))
    (let ((pkg-desc (tabulated-list-get-id (point))))
      (when (and pkg-desc (package-desc-extras pkg-desc))
        (message (cdr (assq :url (package-desc-extras  pkg-desc))))
        (cdr (assq :url (package-desc-extras  pkg-desc)))))))


;; IO I guess?

(defun util/read-if-nil (sym val &optional prompt)
  "Read from minibuffer if SYM is nil with default value VAL.

Optional PROMPT is the prompt for `read-from-minibuffer'.

The return value is always string so one must not use this for
other return types or should convert to appropriate type."
  (format "%s" (or sym
                   (let ((read (read-from-minibuffer (format (or prompt "Enter the file pattern (default %s): ") val)
                                                     nil nil nil nil val)))
                     (if (string-empty-p read) val read)))))


;; Grep

(defun util/rgrep-default-search (regexp)
  "`rgrep' for REGEXP in current directory for files with current extension.
The REGEXP pattern is asked on the prompt by default.

Use `zrgrep' when we're in a `.gz' file.

See also `util/ffip-grep-git-files' and `util/ffip-grep-default'."
  (interactive (list (let* ((phrase (thing-at-point 'symbol t))
                            (read (read-from-minibuffer (format "Regexp (default %s): " phrase)
                                                        nil nil nil nil phrase)))
                       (if (or (not read) (string-empty-p read)) phrase read))))
  (unless (stringp regexp)
    (setq regexp (format "%s" regexp)))
  (let* ((fname (buffer-file-name))
         (suffix (f-ext fname))
         (dir (file-name-directory fname))
         (case-fold-search t)
         (env-dirs (when current-prefix-arg
                     (pcase major-mode
                       ('python-mode '("env"))
                       ('js2-mode '("node_modules"))
                       (_ nil))))
         (grep-find-ignored-directories
          (-concat env-dirs grep-find-ignored-directories))
         (files (if suffix (concat "*." suffix) "*"))
         (zrgrep-template "find -H <D> <X> -type f <F> -exec zgrep <C> -nH -e <R> \\{\\} +")
         grep-save-buffers)
    (eval-after-load "grep" '(grep-compute-defaults))
    (if (equal suffix "gz")
        (zrgrep regexp files dir nil zrgrep-template)
      (rgrep regexp files dir))))

(defun util/multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (util/get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))


;; Editing and text manipulation

(defun util/insert (&rest args)
  "Insert ARGS as strings in current buffer."
  (let ((print-length nil)
        (print-level nil))
    (if (listp args)
        (seq-do (lambda (x) (insert (format "%s\n" x)))
                args)
      (insert (format "%s\n" args)))))

(defun util/delete-blank-lines-in-buffer (&optional buf no-trailing-newline)
  "Delete all empty lines in the entire buffer BUF.
When optional BUF is not given, defaults to current buffer.  With
non-nil optional NO-TRAILING-NEWLINE remove all the empty
newlines from the end also."
  (interactive)
  (unless buf
    (setq buf (current-buffer)))
  (when current-prefix-arg
    (setq no-trailing-newline t))
  (with-current-buffer buf
    (util/delete-blank-lines-in-region (point-min) (point-max) no-trailing-newline)))

;; FIXME: Cask gives an error
(defun util/delete-blank-lines-in-region (&optional beg end no-trailing-newline)
  "Delete all empty lines in region.
Region is either the active region or optional points BEG and
END.  With non-nil NO-TRAILING-NEWLINE remove all the empty
newlines from the end also."
  (interactive)
  (when current-prefix-arg
    (setq no-trailing-newline t))
  (save-restriction
    (when (and (called-interactively-p 'any) (region-active-p))
      (setq beg (region-beginning)
            end (region-end)))
    (when (and beg end (< beg end))
      (narrow-to-region beg end)
      (delete-trailing-whitespace)
      (goto-char (point-min))
      (while (re-search-forward "^[ ]+\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (when (looking-at "\n")
        (delete-char 1))
      (while (re-search-forward "\r?\n+\n" nil t)
        (replace-match "\n"))
      (goto-char (point-max))
      (when (and no-trailing-newline (looking-back "\n" 1))
        (re-search-backward "\r?\n+\n" nil t)
        (replace-match "")))))

(defun util/title-case (&optional str)
  "Convert STR to title case.
If called interactively, then transform the active region to
title case.  Words to ignore are determined by a no-capitalize
list.  Default is `util/no-capitalize-list'.

Other possible lists are `util/no-capitalize-small' and `util/no-capitalize-big'."
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
                           (if (or (string-inflection-upcase-p x)
                                   (and (member x util/no-capitalize-list)
                                        (not capitalize-next)
                                        (not (= count 0)))) ; first word
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
  "Convert a paragraph or region to one long line.
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


;; Data and string manipulation

(defun util/trim (str)
  "Trims the string STR and replace multiple spaces with a single one."
  (util/replace-in-string (string-trim str) "[ ]+" " "))

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

(defun util/non-stop-words-prefix (string n)
  "Return a prefix string of N words from STRING which are not stop words.
Stop words list is `util/stop-words'."
  (let (words)
    (-take-while (lambda (x)
                   (push (replace-regexp-in-string "\\(.+\\)[[:punct:]]$" "\\1" x) words)
                   (< (length (-difference (mapcar #'downcase words) util/stop-words)) n))
                 (split-string string))
    (string-join (reverse words) " ")))


;; url

(defun util/url-buffer-string (buf)
  "Retrieve string from a buffer retrieved by `url-*' function."
  (with-current-buffer buf
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n")
    (buffer-substring-no-properties (point) (point-max))))

;; Buffers and windows

(defun util/hidden-buffers (&optional regexp pred n)
  "Get all hidden buffers matching REGEXP.
Optional REGEXP can be left in case all buffers are to be
returned.

Hidden buffers are those which start with a space.  With optional
PRED, call the function PRED on each buffer and return on those
on which PRED returns non-nil.  When optional N is given, return
only first N buffers.  Buffers are ordered as returned by
`buffer-list'"
  (let ((buffers (-filter (lambda (x)
                            (when (and (string-match-p "^ " (buffer-name x))
                                       (if regexp
                                           (string-match-p regexp (buffer-name x))
                                         t))
                              x))
                          (buffer-list))))
    (when pred
      (setq buffers (-filter (lambda (x) (with-current-buffer x
                                           (funcall pred)))
                             buffers)))
    (if (integerp n )
        (-take n buffers)
      buffers)))

(defun util/get-or-create-window-on-side ()
  "Get the window on side if it exists else create it."
  (let* ((orig-win (selected-window))
         (win (cond ((window-in-direction 'right orig-win)
                     (window-in-direction 'right orig-win))
                    ((window-in-direction 'left orig-win)
                     (window-in-direction 'left orig-win))
                    (t (split-window-horizontally)))))
    win))

(defun util/get-buffers-matching-mode (mode)
  "Return a list of buffers where their `major-mode' is equal to MODE."
  (declare (pure t) (side-effect-free t))
  (let (buffer-mode-matches)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (push buf buffer-mode-matches))))
    buffer-mode-matches))

(provide 'util/core)

;;; util-core.el ends here

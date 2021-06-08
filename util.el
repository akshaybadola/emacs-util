;;; util.el --- Various utility functions. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020,2021
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Keywords:	utility
;; Version:     0.2.0
;; Package-Requires: ((helm) (a "0.1.1") (org "9.4.4") (dash "2.17.0") (bind-key "2.4") (sphinx-doc "0.3.0") (tern "0.0.1") (xr "1.21"))

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

(require 'a)
(require 'helm)
(require 'cl-lib)
(require 'dash)
(require 'dired)
(require 'grep)
(require 'gv)
(require 'ibuffer)
(require 'imenu)
(require 'org)
(require 'org-element)
(require 'package)
(require 'sphinx-doc)
(require 'tern)
(require 'time-stamp)
(require 'xr)

(defvar parse-time-weekdays)

(defvar util/org-min-collect-heading-length 1)

(defvar util/org-simple-regexp-search-modes
  '(emacs-lisp-mode lisp-mode python-mode javascript-mode
                    rjsx-mode fundamental-mode text-mode)
  "Modes for which org should do a simple regexp search.
Used by `util/org-execute-simple-regexp-search'.")

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

(declare-function org-hide-drawer-toggle "org")

(defsubst cdass (elem alist)
  "Short for (cdr (assoc ELEM) list).
Argument ALIST association list."
    (cdr (assoc elem alist)))

(defun util/insert (&rest args)
  "Insert ARGS as strings."
  (let ((print-length nil)
        (print-level nil))
    (if (listp args)
        (seq-do (lambda (x) (insert (format "%s\n" x)))
                args)
      (insert (format "%s\n" args)))))

(defun util/pairs-to-alist (pairs)
  "Merge cons PAIRS into an alist with first elements as keys.

The head of the list is the associative element.

Example:
    (pairs-to-alist '((a b) (b c d) (a d) (e f)))
     => '((a b d) (b c d) (e f))"
  (when (and (consp pairs) (a-assoc pairs))
    (let (newlist)
      (seq-do (lambda (x)
                (if (a-has-key newlist (car x))
                    (setq newlist (a-update newlist (car x) (lambda (y) (push (cdr x) y))))
                  (push (list (car x) (cdr x)) newlist)))
              pairs)
      newlist)))

(defun util/hidden-buffers (&optional regexp pred n)
  "Get all hidden buffers matching REGEXP.
Optional REGEXP can be left in case all buffers are returned.
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
;;
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
                        (">" . "[]\\\\|>]"))))
    (dolist (pattern pattern-list)
      (setq ts-format (replace-regexp-in-string (car pattern) (cdr pattern) ts-format)))
    ts-format))

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

(defun util/goto-latest-time-stamp ()
  "Goto the latest `time-stamp'.
Like `util/org-goto-latest-timestamp' but for all buffers."
  )

(defun util/occur-sorted-timestamps ()
  "Goto the latest `time-stamp'.
Like `util/org-occur-sorted-timestamps' but for all buffers."
  )

(defun util/decode-time-stamp (ts)
  "Similar to `decode-time' but for time stamp TS."
  (pcase-let* ((`(,date ,day ,time) (split-string (substring ts 1 -1) " "))
               (`(,dy ,dm ,dd) (mapcar #'string-to-number (split-string date "-")))
               (`(,hh ,mm) (mapcar #'string-to-number (split-string time ":"))))
    (list 0 mm hh dd dm dy (cdass (downcase day) parse-time-weekdays) nil nil)))

(defun util/time-stamp-less-p (A B)
  "Similar to `time-less-p' but for time stamps A and B."
  (time-less-p (encode-time (util/decode-time-stamp A))
               (encode-time (util/decode-time-stamp B))))

;; DONE: occur like mode where the timestamps are sorted from latest to oldest
;;     : I made it :-)
;; NOTE: should be (interactive P)
;; CHECK: Why interactive p? I've forgotten the difference
;; FIXME: This only goes to `timestamp' and ignores clock entries
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

(defun util/org-occur-sorted-timestamps (&optional ts-regexp)
  "Run `occur' in the current org buffer for timestamps.
By default the `occur' is run for only the current subtree.  With
a universal argument, `\\[universal-argument]' run for full
buffer.  With optional TS-REGEXP, search is done for that
regexp.  Default is to generate the regexp from
`util/generate-org-ts-regexp'."
  (interactive)
  (save-restriction
    (let ((case-fold-search nil))
      (unless current-prefix-arg
        (org-narrow-to-subtree))
      (unless ts-regexp
        (setq ts-regexp (util/generate-org-ts-regexp org-time-stamp-formats)))
      ;; NOTE: old ts-regexp
      ;; (setq ts-regexp "\\[[0-9].*?[0-9]\\]")
      (occur ts-regexp)
      (other-window 1)
      (read-only-mode -1)
      (goto-char 0)
      (forward-line)
      ;; NOTE: This may not be generally valid but We use this as the time stamp
      ;;       is formatted YYYY-MM-DD first for us.
      ;;       Can use `util/time-stamp-less-p'
      ;;       I think the sorting mechanism may also have to change in that case.
      (sort-regexp-fields -1 "^.*$" ts-regexp (point) (buffer-end 1))
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

(defun util/org-count-subtree-children ()
  "Display the number of elements of immediate children of current subtree."
  (interactive)
  (if (eq major-mode 'org-mode)
      (save-excursion
        (save-restriction
          (org-narrow-to-subtree)
          (goto-char (point-min))
          (let ((level (org-outline-level))
                (children 0))
            (while (outline-next-heading)
              (when (= (org-outline-level) (+ level 1))
                (cl-incf children)))
            (message (format "Subtree has %s children" children)))))
    (message "Not in org-mode")))

(defun util/org-copy-subtree-elems (pred)
  "Copy all children subtrees of current heading for non-nil PRED.
PRED is a function, which is called at each subtree heading for
each child (but no deeper).  Each child is copied as an entire
subtree, so if there are other elements which may or may not
satisfy PRED, they will also be copied as part of the subtree."
  (if (eq major-mode 'org-mode)
      (let ((count 0)
            kill-str)
        (save-excursion
          (save-restriction
            (org-narrow-to-subtree)
            (goto-char (point-min))
            (let ((level (org-outline-level)))
              (while (outline-next-heading)
                (when (= (org-outline-level) (+ level 1))
                  (when (funcall pred)
                    (outline-back-to-heading t)
                    (cl-incf count)
                    (let ((beg (progn (beginning-of-line) (point)))
                          (end (org-end-of-subtree t)))
                      (push (buffer-substring-no-properties beg end) kill-str))))))
            (kill-new (mapconcat #'identity kill-str "\n"))
            (message (format "Killed %s subtrees" count)))))
    (message "Not in org-mode")))

(defun util/org-copy-subtree-elems-with-property (&optional prop)
  "Copy all children subtrees of current heading which have a property PROP.
When called interactively and with a \\[universal-argument] PROP
is input from user.  It defaults to PDF_FILE if not given."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let ((prop (or prop (and current-prefix-arg
                                (read-from-minibuffer "Property name: "))
                      "PDF_FILE"))
            (count 0)
            kill-str)
        (save-excursion
          (save-restriction
            (org-narrow-to-subtree)
            (goto-char (point-min))
            (let ((level (org-outline-level)))
              (while (outline-next-heading)
                (when (= (org-outline-level) (+ level 1))
                  (let ((val (org-entry-get (point) prop)))
                    (when (and val (not (string-empty-p val)))
                      (cl-incf count)
                      (let ((beg (progn (beginning-of-line) (point)))
                            (end (org-end-of-subtree t)))
                        (push (buffer-substring-no-properties beg end) kill-str)))))))
            (kill-new (mapconcat #'identity kill-str "\n"))
            (message (format "Copied %s subtrees" count)))))
    (message "Not in org-mode")))

(defun util/org-execute-simple-regexp-search (str)
  "Find the link for a search string STR with a simple `re-search-forward'.
When no function in `org-execute-file-search-functions' matches
`org-link-search' doeesn't always search correctly in non
`org-mode' files.  In a lot of cases a simple regexp search
suffices.  This function does just that.  Adapated from
`org-execute-file-search-in-bibtex'."
  ;; modes that we want to override
  (when (member major-mode util/org-simple-regexp-search-modes)
    (pop-to-buffer (current-buffer))
    (goto-char (point-min))
    (and (re-search-forward str nil t)
	 (goto-char (match-beginning 0)))
    (if (match-beginning 0)
	(let ((b (current-buffer)) (p (point)))
	  (with-current-buffer b
	    (goto-char p)))
      ;; CHECK: Copied from `org-execute-file-search-in-bibtex'. Not sure
      ;;        (recenter 0) is correct.
      ;; Move entry start to beginning of window
      ;; (recenter 0)
      )
    ;; return t to indicate that the search is done.
    t))

;; TODO: In case there are multiple matches, list all
(defun util/org-execute-org-heading-max-match-search (str)
  "Return maximum length match for minimum three words of STR of org heading.
If heading contains less than 3 words, then an exact match is
searched."
  (when (derived-mode-p 'org-mode)
    (let* ((buf (current-buffer))
           (words (split-string (string-remove-prefix "*" str) " "))
           (title-re
	    (format (rx bol (+ "*") " "
                        (opt (eval `(group
                                     ,(nconc (if (eq major-mode 'org-mode)
                                                 (cadr (xr org-todo-regexp))
                                               '(or)) '("COMMENT")))) " ")
                        (group "%s")
                        eol)
		    (if (< (length words) 3)
                        (string-join words " ")
                      (concat (mapconcat #'regexp-quote (-take 3 words) ".+") ".+"))))
	   (cookie-re "\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]")
	   (comment-re (format "\\`%s[ \t]+" org-comment-string))
           matches)
      (with-current-buffer buf
        ;; (switch-to-buffer buf)
        (goto-char (point-min))
        (while (re-search-forward title-re nil t)
          (push (list (length (-intersection words
		                             (split-string
		                              (replace-regexp-in-string
		                               cookie-re ""
		                               (replace-regexp-in-string
		                                comment-re "" (org-get-heading t t t))))))
                      (point))
	        matches))
        (when matches
          (goto-char (cadr (-max-by (lambda (x y) (> (car x) (car y))) matches)))
          (beginning-of-line)
          ;; (recenter 0)
          ;; CHECK: Why was I doing this `mapcar'?
          ;; (mapcar #'cadr matches)
          ))
      matches)))

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

;; package utility functions
(defun util/package-desc (pkg)
  "Return description of package PKG.
Copied from `describe-package-1'.  Returns description of either
installed PKG or one available in package archives."
  (let* ((desc (or
                (if (package-desc-p pkg) pkg)
                (cadr (assq pkg package-alist))
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

(defun util/package-top-level-packages ()
  "Return list of packages which are not a dependency."
  (-non-nil (mapcar (lambda (x)
                      (unless (util/package-required-by (car x)) (car x)))
                    package-alist)))

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

(defun util/package-required-by (pkg-name-or-symbol &optional all)
  "Return alist of symbols of installed packages which require PKG-NAME-OR-SYMBOL.
PKG-NAME-OR-SYMBOL can be a symbol or a string. With optional
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

(defun util/rgrep-default-search (regexp &optional exclude-dirs)
  "`rgrep' for REGEXP in current directory for files with current extension.
If optional EXCLUDE-DIRS is non-nil, add them to `grep' exclude
path.  With `current-prefix-arg', read EXCLUDE-DIRS from the
minibuffer by the user.  The REGEXP pattern is asked on the
prompt by default."
  (interactive (list (let ((phrase (thing-at-point 'symbol t)))
                       (read-from-minibuffer (format "Regexp (default %s): " phrase)
                                             nil nil t nil phrase))))
  ;; (when (and current-prefix-arg (not exclude-dirs))
  ;;   (setq exclude-dirs (split-string
  ;;                       (read-from-minibuffer "Exclude additional dirs: ") nil t " ")))
  (unless (stringp regexp)
    (setq regexp (format "%s" regexp)))
  (let* ((fname (buffer-file-name))
         (dir (file-name-directory fname))
         (case-fold-search t)
         (env-dirs (when current-prefix-arg
                     (pcase major-mode
                       ('python-mode '("env"))
                       ('js2-mode '("node_modules"))
                       (_ nil))))
         (grep-find-ignored-directories
          (-concat env-dirs grep-find-ignored-directories))
         (files (concat "*." (car (last (split-string fname "\\."))))))
    (eval-after-load "grep"
      '(grep-compute-defaults))
    (rgrep regexp files dir)
    ;; (if current-prefix-arg
    ;;     (grep (list regexp files))
    ;;   (rgrep regexp files dir))
    ))

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
  "Return a list of buffers where their `major-mode' is equal to MODE."
  (let (buffer-mode-matches)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (push buf buffer-mode-matches))))
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
"Goto definition of symbol at point in other window.
If optional PROMPT-VAR is given or tern can't figure out the
symbol, prompt the user for the symbol."
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
  (concat (if (string-prefix-p "~" (car elements)) "" "/" )
          (mapconcat (lambda (x)
                       (string-remove-prefix "/" (string-remove-suffix "/" x)))
                     elements "/")))

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

(defmacro util/with-check-mode (mode msg-prefix &rest body)
  "Execute BODY only if `major-mode' equals MODE.
Otherwise message not in MODE.  MSG-PREFIX can be used to indicate
the package from which the macro is invoked.  If MSG-PREFIX is
nil, then nothing is prefixed to the message."
  `(if (eq major-mode ,mode)
       (progn ,@body)
     (message "%sNot in %s"
              (if ,msg-prefix (concat ,msg-prefix " ") "")
              ,mode)))

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
     (message "Mean runtime %.06f seconds over %s runs"
              (/ (float-time (time-since time)) ,n) ,n)))

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

(defun util/title-case (&optional str)
  "Convert STR to title case.
If called interactively, then transform the active region to
title case.  Words to ignore are determined by a no-capitalize
list.  Default is `util/no-capitalize-list'.

See also, `util/no-capitalize-small' and `util/no-capitalize-big'."
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
                           (if (and (member x util/no-capitalize-list)
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
atoms.  See `util/commands-matching-re' and
`util/builtins-matching-re' for example of PREDS."
  (let (atoms)
    (mapatoms (lambda (x)
                (when (and (fboundp x)
                           (or (not preds)
                               (funcall (-andfn preds) x))
                           (string-match-p re (symbol-name x)))
                  (push x atoms))))
    atoms))

(defun util/commands-matching-re (re)
  "Return list of commands matching regexp RE."
  (util/functions-matching-re re (lambda (x) (commandp (symbol-function x)))))

;; FIXME: This is totally broken
;; (defun util/builtins-matching-re (re)
;;   "Return list of builtins matching regexp RE."
;;   (util/functions-matching-re re (lambda (x) (subrp (symbol-function x)))))

(defun util/org-remove-all-drawers (&optional buf)
  "Remove all drawers from buffer BUF.
If buf is not given or doesn't exist, defaults to
`current-buffer'."
  (let ((buf (or (and buf (get-buffer buf)) (current-buffer))))
    (when (eq major-mode 'org-mode)
      (with-current-buffer buf
        (goto-char (point-min))
        (org-with-wide-buffer
         (while (re-search-forward org-drawer-regexp nil t)
           (util/org-remove-drawer-at)))))))

(defun util/org-remove-drawer-at (&optional pos)
  "Remove a drawer at position POS.
POS may also be a marker."
  (with-current-buffer (if (markerp pos) (marker-buffer pos) (current-buffer))
    (unless pos
      (setq pos (point)))
    (org-with-wide-buffer
     (goto-char pos)
     (let ((drawer (org-element-at-point)))
       (when (memq (org-element-type drawer) '(drawer property-drawer))
	 (delete-region (org-element-property :begin drawer)
			(progn (goto-char (org-element-property :end drawer))
			       (skip-chars-backward " \r\t\n")
			       (forward-line)
			       (point))))))))

(defun util/org-remove-list-items-matching-re-from-buffer (re)
  "Remove list items from an `org-mode' buffer which match regexp RE.
The buffer to operate on must be an org buffer.  Optional RECURSE
is not used."
  (when (eq major-mode 'org-mode)
    (goto-char (point-min))
    (let (regions)
      (org-element-map (org-element-parse-buffer) 'item
        (lambda (el)
          (let ((beg (plist-get (cadr el) :contents-begin))
                (end (plist-get (cadr el) :contents-end))
                match-pt)
            (setq match-pt (and beg end (string-match-p re (buffer-substring-no-properties beg end))))
            (when (and match-pt (= match-pt 0))
              (goto-char beg)
              (let ((bol (point-at-bol)))
                (when (-none-p (lambda (x) (and (< (car x) bol) (>= (cdr x) end))) regions)
                  (push (cons bol end) regions)))))))
      (mapcar (lambda (x) (delete-region (car x) (cdr x))) regions))))

;; FIXME: This leaves the subtree in "show" mode. Should save subtree state
(defun util/org-narrow-to-heading-and-body ()
  "Narrow to the current heading and the body.
Unlike `org-narrow-to-subtree' any headings which are children of
the current heading are excluded."
  (let (pmin pmax)
    (org-narrow-to-subtree)
    (save-excursion
      (goto-char (point-min))
      (org-show-subtree)
      (beginning-of-line)
      (if (eq (point-min) (point))
          (setq pmin (point))
        (org-previous-visible-heading 1)
        (setq pmin (point)))
      (org-next-visible-heading 1)
      (setq pmax (point))
      (narrow-to-region pmin pmax))))

(defun util/org-kill-new-or-append-subtree ()
  "Kill or append to last kill current subtree.
`kill-new' a subtree if previous kill was not an org heading,
append to last kill otherwise.  With non-nil `current-prefix-arg'
remove the subtree from the buffer also."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (let ((str (buffer-string))
          (beg (point-min))
          (end (point-max))
          new)
      (if (string-prefix-p "*" (-first-item (split-string (car kill-ring) "\n")))
          (kill-append (concat "\n" str) nil)
        (kill-new str)
        (setq new t))
      (when current-prefix-arg
        (delete-region beg end))
      (message (if new "Killed %s " "Appended %s to last kill")
               (-first-item (split-string str "\n")))))
  (delete-blank-lines))

(defun util/org-remove-subtrees-matching-re (re &optional recurse)
  "Remove subtrees from an `org-mode' buffer whose headlines match regexp RE.
Remove only at one depth below the current subtree.  The buffer
to operate on must be an org buffer.  Optional RECURSE is not
used as of now."
  (when (eq major-mode 'org-mode)
    (goto-char (point-min))
    (let (regions)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (el)
          (let ((hbeg (plist-get (cadr el) :contents-begin))
                (hend (plist-get (cadr el) :contents-end)))
            (when (string-match-p re (plist-get (cadr el) :raw-value))
              (when (and hbeg hend)
                (push (cons hbeg hend) regions))))))
      (mapcar (lambda (x) (delete-region (car x) (cdr x))) regions))))

(defvar util/org-headings-cache nil
  "Cache of headings in designated buffers.")
(defvar util/org-heading-props-filter-p nil
  "Additional property filter to apply to headings while collecting them.
Used by `util/org-collect-headings'")
(defun util/org-collect-headings (predicate)
  "Return headings in an org buffer.
When optional unary PREDICATE is given, select only those
headings which satisfy it.  See
`util/org-default-heading-filter-p' for an example of such a
predicate.

Additionally `util/org-heading-props-filter-p' can be configured
to filter additional headings by `org-element' properties plist."
  (let ((el-predicate (or predicate util/org-heading-props-filter-p #'identity))
        headings)
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward org-complex-heading-regexp nil t)
        (let* ((el (org-element-at-point))
               (heading (org-element-property :title el))
               (author (or (org-element-property :AUTHOR el) ""))
               (pos (org-element-property :begin el))
               (buf (buffer-name)))
          ;; TODO: Not sure how many predicates to handle or how
          (when (and (funcall predicate heading) ; (funcall el-predicate el)
                     )
            (push `(,heading ,author ,buf ,pos) headings))))
      headings)))

(defvar util/org-multi-collect-headings-cache nil
  "Alist of collected headings cache.")
(defvar util/org-multi-collect-files nil
  "List of files from which to collect headings.")
(defvar util/org-multi-collect-modtimes nil
  "Modification times of buffers from which to collect headings.")
(defvar util/org-multi-collect-buffers nil
  "List of buffers from which to collect headings.")

(defun util/org-multi-collect-setup ()
  "Setup the `util/org-multi-collect-headings' variables."
  (interactive)
  (setq util/org-multi-collect-buffers
        (mapcar #'find-file-noselect util/org-multi-collect-files))
  (setq util/org-multi-collect-modtimes
        (mapcar
         (lambda (buf) (cons (buffer-name buf)
                             (file-attribute-modification-time
                              (file-attributes (buffer-file-name buf)))))
         util/org-multi-collect-buffers)))

;; TODO: Add to cache as current buffer updates.
(defun util/org-multi-collect-headings (predicate)
  "Return headings in an org buffer satisfying unary PREDICATE.
See `util/org-default-heading-filter-p' for an example of such a
predicate."
  ;; check mod times of buffers or files
  ;; If updated, add to cache
  ;; Search only in entry cache
  (unless (-all? #'buffer-live-p util/org-multi-collect-buffers)
    (util/org-multi-collect-setup))
  (let* ((modtimes (mapcar
                    (lambda (buf) (cons (buffer-name buf)
                                        (file-attribute-modification-time
                                         (file-attributes (buffer-file-name buf)))))
                    util/org-multi-collect-buffers))
         (bufnames (mapcar #'buffer-name util/org-multi-collect-buffers))
         (headings (mapcar (lambda (bufname)
                             (cons bufname
                                   (progn
                                     ;;  buf is missing from cache
                                     (when (or (not (a-get util/org-multi-collect-headings-cache bufname))
                                               ;;  cached-buf-modtime < current-buf-modtime
                                               (time-less-p (a-get util/org-multi-collect-modtimes
                                                                   bufname)
                                                            (a-get modtimes bufname)))
                                       (setq util/org-multi-collect-headings-cache
                                             ;; update cache
                                             (a-assoc util/org-multi-collect-headings-cache
                                                      bufname
                                                      (with-current-buffer bufname
                                                        (util/org-collect-headings predicate)))
                                             util/org-multi-collect-modtimes
                                             ;; update modtimes
                                             (a-assoc util/org-multi-collect-modtimes
                                                      bufname
                                                      (a-get modtimes bufname))))
                                     (a-get util/org-multi-collect-headings-cache bufname))))
                           bufnames)))
    headings))

(defun util/org-default-heading-filter-p (heading)
  "Default predicate for `util/org-collect-headings'.
Compares length of HEADING with
`util/org-min-collect-heading-length'."
  (> (length (split-string heading))
     util/org-min-collect-heading-length))

(defun util/non-stop-words-prefix (string n)
  "Return a prefix string of N words from STRING which are not stop words.
Stop words list is `util/stop-words'."
  (let (words)
    (-take-while (lambda (x)
                   (push (replace-regexp-in-string "\\(.+\\)[[:punct:]]$" "\\1" x) words)
                   (< (length (-difference (mapcar #'downcase words) util/stop-words)) n))
                 (split-string string))
    (string-join (reverse words) " ")))

(defun util/org-get-text-links (link-re narrow)
  "Get all the links matching LINK-RE in an org buffer.
When NARROW is non-nil, first narrow to subtree."
  (let (temp)
    (save-restriction
      (save-excursion
        (when narrow
          (org-narrow-to-subtree))
        (goto-char (point-min))
        ;; (when (outline-next-heading)
        ;;   (narrow-to-region (point-min) (point)))
        ;; (goto-char (point-min))
        (while (re-search-forward link-re nil t nil)
          (when (and (match-string 1) (match-string 2))
            (push (list (substring-no-properties (match-string 2))
                        (substring-no-properties (match-string 1)))
                  temp)))))
    temp))

(defvar util/org-insert-link-to-heading-prefix-behaviour '(((4) . subtree) ((16) . buffer))
  ;; '(subtree buffer research-files)
  "Behaviour of `current-prefix-arg' for `util/org-insert-link-to-heading'.
A list of symbols `subtree' `research-files' `buffer'.  The first
element in the list corresponds to single universal prefix
argument `C-u'.  The second element to two universal
prefix arguments and the last one to no argument.")

(defun util/org-insert-link-to-heading (&optional clip-func citation)
  "Insert a link to selected heading.
Description of the link is determined by optional CLIP-FUNC.
If not given, it defaults to `identity'.

If optional CITATION is non-nil, the headings are gathered for
only the current buffer and citations within the current
`doc-root' are searched. A `doc-root' is an org subtree with the
non-nil property DOC_ROOT.

For customizing how headings are gathered, change the function
`util/org-default-heading-filter-p'.

See also, `util/org-collect-headings' and
`util/org-multi-collect-headings'."
  (interactive)
  (let* ((read-from (or (and citation 'buffer)
                        (a-get util/org-insert-link-to-heading-prefix-behaviour
                               current-prefix-arg)
                        'research-files))
         (clip-func (or clip-func #'identity))
         (text-link-re (rx "[" "[" (group (seq (or (regexp "file.+::\\*") "*" "http") (+? any))) "]"
                           "[" (group (+? any)) "]" "]"))
         (headings (pcase read-from
                     ('buffer (util/org-collect-headings #'util/org-default-heading-filter-p))
                     ('research-files (apply #'-concat
                                             (a-vals
                                              (util/org-multi-collect-headings
                                               #'util/org-default-heading-filter-p))))
                     ('subtree (save-restriction
                                 (org-narrow-to-subtree)
                                 (util/org-collect-headings #'util/org-default-heading-filter-p)))))
         (doc-root (when citation
                     (save-excursion
                       (let (is-doc-root no-doc-root)
                         (while (and (not is-doc-root) (not no-doc-root))
                           (condition-case nil
                               (outline-up-heading 1 t)
                             (error (setq no-doc-root t)))
                           (setq is-doc-root (org-entry-get (point) "DOC_ROOT")))
                         (and is-doc-root (point))))))
         (subtree-text-links (when citation
                               (let ((temp (util/org-get-text-links text-link-re t)))
                                 (when temp
                                   (mapcar (lambda (x)
                                             (cons (concat (replace-regexp-in-string text-link-re "\\2" (car x))
                                                           " (subtree)")
                                                   x))
                                           (-uniq temp))))))
         (doc-root-text-links (when (and citation doc-root)
                                (save-excursion
                                  (goto-char doc-root)
                                  (let ((temp (util/org-get-text-links text-link-re t)))
                                    (when temp
                                      (mapcar (lambda (x)
                                                (cons (concat (replace-regexp-in-string text-link-re "\\2" (car x))
                                                              " (citations)")
                                                      x))
                                              (-uniq temp)))))))
         ;; subtree-text-links at the beginning of selections
         (selections (-concat (a-keys subtree-text-links)
                              (a-keys doc-root-text-links)
                              (mapcar (lambda (x)
                                        (string-join (pcase read-from
                                                       ((or 'buffer 'subtree) (-take 2 x))
                                                       ('research-files (-take 3 x))) " "))
                                      headings)))
         (prompt (pcase read-from
                   ('buffer "Insert link (cur-buf): ")
                   ('subtree "Insert link (subtree): ")
                   ('research-files "Insert link (files): ")))
         (selected (ido-completing-read prompt selections)))
    (cond ((string-suffix-p " (subtree)" selected)
           (insert (apply #'format "[[%s][%s]]" (reverse (a-get subtree-text-links selected)))))
          ((string-suffix-p " (citations)" selected)
           (insert (apply #'format "[[%s][%s]]" (reverse (a-get doc-root-text-links selected)))))
          (t (let* ((indx (- (-elem-index selected selections) (length subtree-text-links)
                             (length doc-root-text-links)))
                    (file (pcase read-from
                            ('research-files (format
                                              "file:%s::"
                                              (buffer-file-name
                                               (get-buffer (nth 2 (nth indx headings))))))
                            (_ "")))
                    (heading (pcase read-from
                               ('research-files (car (nth indx headings)))
                               (_ (car (nth indx headings))))))
               (insert (format "[[%s*%s][%s]]" file heading (funcall clip-func heading))))))))

(defun util/org-insert-citation-to-heading ()
  "Insert a citation to a heading.
Call `util/org-insert-link-to-heading' so that the description of
the link is is first two words of the heading.  The headings are
filtered by length and only headings greater than
`util/org-min-collect-heading-length' are searched."
  (interactive)
  (util/org-insert-link-to-heading (-rpartial #'util/non-stop-words-prefix 2) t))

(defun util/org-collect-duplicate-headings (&optional predicate ignore-case test)
  "Collect duplicate headings in an org buffer.
With optional boolean function PREDICATE, collect those which
only satisfy it.  With optional non-nil IGNORE-CASE, ignore case
while searching.  Optional TEST is which test function to use for
`cl-count'.  Defaults to `equal'."
  (let (headings dups)
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward org-complex-heading-regexp nil t)
        (let* ((el (org-element-at-point))
               (heading (org-element-property :title el))
               (pos (org-element-property :begin el))
               (test (or test 'equal)))
          (if predicate
              (when (funcall predicate heading)
                (push (cons heading pos) headings))
            (push (cons heading pos) headings))))
      (dolist (heading headings)
        (when (> (cl-count
                  ;; (car heading)
                  ;; (mapcar #'car headings)
                  (if ignore-case (downcase (car heading)) (car heading))
                  (mapcar (lambda (x) (if ignore-case (downcase (car x)) (car x))) headings)
                  :test test)
                 1)
          (push heading dups)))
      (sort dups (lambda (x y) (string-greaterp (car y) (car x)))))))

(defun util/org-helm-show-duplicate-headings (&optional pred ignore-case)
  "Show duplicate headings in an org buffer with `helm'.
With optional predicate PRED, show only those which satisfy the
predicate.  With optional non-nil IGNORE-CASE, ignore case while
searching."
  (interactive)
  (helm :sources (helm-build-sync-source "Duplicate headings"
                   :candidates (lambda ()
                                 (with-helm-current-buffer
                                   (util/org-collect-duplicate-headings pred ignore-case)))
                   :follow 1
                   :action (lambda (char)
                             (goto-char char)
                             (org-show-entry)
                             (save-excursion
                               (let ((bounds (org-get-property-block)))
                                 (when bounds
                                   ; HACK (car bounds) gives node-property which org complains is not a drawer
                                   (goto-char (- (car bounds) 1))
                                   (org-hide-drawer-toggle))))))))

(defun util/insert-heading-from-url (&optional url)
  "Fetch the title from an optional URL.
URL is copied from clipboard if not given."
  (interactive)
  (util/with-check-mode
   'org-mode nil
   (org-insert-heading-respect-content)
   (newline)
   (org-indent-line)
   (insert "- ")
   (yank)
   (org-edit-headline
    (string-trim (shell-command-to-string
                  (format "/home/joe/lib/ref-man/env/bin/python -c 'import requests; from bs4 import BeautifulSoup; headers={\"accept\": \"text/html,application/xhtml+xml,application/xml;\", \"accept-encoding\": \"gzip, deflate, br\", \"accept-language\": \"en-GB,en-US;q=0.9,en;q=0.8\", \"cache-control\": \"no-cache\", \"user-agent\": \"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko)\"}; print(BeautifulSoup(requests.get(\"%s\", headers=headers).content).title.text)'"
                          (org-element-property :raw-link (org-element-context))))))))

(defun util/run-python-file-from-str (str &optional python-path args)
  "Write the given python string STR to a temp file and run with python.
If optional PYTHON-PATH is given, that python executable is used
to run the script.  Optional ARGS is an list of arguments to
format the string."
  (let ((formatted-string (apply #'format (cons str args)))
        (python (or python-path (executable-find "python")))
        (tfile (make-temp-file "util-py-")))
    (with-temp-file tfile
      (insert formatted-string))
    ;; (make-process)
    ;; write temp file and run process with python
    ))

(provide 'util)

;;; util.el ends here


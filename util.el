;;; util.el --- Various utility functions. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020,2021
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Thursday 09 September 2021 01:55:13 AM IST>
;; Keywords:	utility
;; Version:     0.3.2
;; Package-Requires: ((helm) (a "0.1.1") (org "9.4.4") (dash "2.17.0") (bind-key "2.4") (find-file-in-project "6.0.6") (sphinx-doc "0.3.0") (tern "0.0.1"))

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
(require 'find-file-in-project)         ; Only used for `ffip-project-root'
(require 'grep)
(require 'gv)
(require 'ibuffer)
(require 'imenu)
(require 'org)
(require 'org-element)
(require 'package)
(require 'sphinx-doc)                   ; FIXME: Not used
(require 'tern)
(require 'time-stamp)

(defvar util/insert-heading-python-executable "/usr/bin/python"
  "The python executable for `util/insert-heading-from-url'")

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
(defvar util/file-link-re (rx "[" "[" (opt "file:") (group (+? any)) "]"
                              (opt (seq "[" (group (+? any)) "]")) "]"))
(defvar util/org-file-link-re (rx "[" "[" (group (seq (or (regexp "file.+?::\\*") "*") (+? any))) "]"
                                  "[" (group (+? any)) "]" "]"))
(defvar util/org-text-link-re (rx "[" "[" (group (or (seq (opt (regexp "file.+?::")) (or "*" "#") (+? any))
                                                     (regexp "http.+?"))) "]"
                                  "[" (group (+? any)) "]" "]"))


(declare-function org-hide-drawer-toggle "org")

(defsubst cdass (elem alist)
  "Short for (cdr (assoc ELEM) list).
Argument ALIST association list."
    (cdr (assoc elem alist)))

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

(defmacro util/with-org-mode (&rest body)
  "Call form `body' only if major-mode is `org-mode'."
  (declare (debug (body)))
  `(util/with-check-mode
    'org-mode nil
    ,@body))

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
                    (setq newlist (a-update newlist (car x)
                                            (lambda (y) (-flatten (list (cdr x) y)))))
                  (push (cons (car x) (cdr x)) newlist)))
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

(defun util/org-remove-all-time-stamps ()
  (util/with-org-mode
   (goto-char (point-min))
   (while (re-search-forward (util/generate-org-ts-regexp org-time-stamp-formats)
                             nil t)
     (replace-match ""))))

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
  (util/with-org-mode
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
       (org-reveal)))))

(defun util/org-occur-sorted-timestamps (&optional ts-regexp)
  "Run `occur' in the current org buffer for timestamps.
By default the `occur' is run for only the current subtree.  With
a universal argument, `\\[universal-argument]' run for full
buffer.  With optional TS-REGEXP, search is done for that
regexp.  Default is to generate the regexp from
`util/generate-org-ts-regexp'."
  (interactive)
  (util/with-org-mode
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
       (read-only-mode)))))

(defun util/org-num-finished ()
  "Message the number of FINISHED items in subtree."
  (interactive)
  (util/with-org-mode
   (save-restriction
     (org-narrow-to-subtree)
     (goto-char (point-min))
     (message (format "%s" (count-matches "\* FINISHED"))))))

(defun util/org-apply-to-buffer-headings (fn)
  "Apply the function `FN' to each heading in buffer.
Return the result of the application."
  (save-excursion
    (goto-char (point-min))
    (let (result)
      (while (outline-next-heading)
        (push (funcall fn) result))
      result)))

(defun util/org-apply-to-subtree-headings (fn all-levels)
  "Apply the function `FN' to each child heading of the current heading.
Return the result of the application.  When ALL-LEVELS is
non-nil, subheadings at all levels are return. Default is to
return only one level lower than the current heading."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (let ((level (org-outline-level))
            result)
        (while (outline-next-heading)
          (when (or all-levels (= (org-outline-level) (+ level 1)))
            (push (funcall fn) result)))
        result))))

(defun util/org-count-subtree-children ()
  "Return and display the number of headings of current subtree."
  (interactive)
  (util/with-org-mode
   (save-excursion
     (save-restriction
       (org-narrow-to-subtree)
       (goto-char (point-min))
       (let ((level (org-outline-level))
             (children 0))
         (while (outline-next-heading)
           (when (= (org-outline-level) (+ level 1))
             (cl-incf children)))
         (prog1 children
           (when (called-interactively-p 'any)
             (message (format "Subtree has %s children" children)))))))))

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
  (util/with-org-mode
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
         (message (format "Copied %s subtrees" count)))))))

(defun util/org-execute-simple-regexp-search (str)
  "Find the link for a search string STR with a simple `re-search-forward'.
When no function in `org-execute-file-search-functions' matches
`org-link-search' doeesn't always search correctly in non
`org-mode' files.  In a lot of cases a simple regexp search
suffices.  This function does just that.  Adapated from
`org-execute-file-search-in-bibtex'."
  ;; modes that we want to override
  (let ((case-fold-search t))
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
    t)))

(defvar util/org-execute-search-prefix-arg-behaviour nil
  "Behaviour of \\[universal-argument] in `util/org-execute-org-heading-search-*' functions.
For now only 'pdf is used and opens the PDF_FILE from properties.")

(defvar util/org-execute-search-ignore-case t
  "Ignore case for headings search.")

(defun util/org-execute-search-funcall (args)
  "Call appropriate function according for org search.
The value of `util/org-execute-search-prefix-arg-behaviour'
determines which function to call to value of.  ARGS is a plist
of arguments."
  (pcase util/org-execute-search-prefix-arg-behaviour
    ('pdf (util/org-execute-search-find-pdf-file (plist-get args :pdf-file)))
    (_ nil)))

(defun util/org-execute-search-find-pdf-file (pdf-file)
  (when (and pdf-file (f-exists? pdf-file))
    (find-file pdf-file)))

(defun util/org-execute-search-heading-length-subr (words comment-re cookie-re)
  "Return length of intersection of WORDS and org heading at point.
Ignore case if `util/org-execute-search-ignore-case' is non-nil.

WORDS are the search terms given to `util/org-execute-customid-or-max-heading-match-search'.
See the above function for COMMENT-RE and COOKIE-RE."
  (let ((words (if util/org-execute-search-ignore-case
                   (mapcar #'downcase words) words))
        (func (if util/org-execute-search-ignore-case #'downcase #'identity)))
      (length (-intersection words
                             (->> (org-get-heading t t t t)
                                  (substring-no-properties)
                                  (replace-regexp-in-string comment-re "")
                                  (replace-regexp-in-string cookie-re "")
                                  (funcall func)
                                  (split-string))))))

;; TODO: Maybe in case there are multiple matches, list all
(defun util/org-execute-customid-or-max-heading-match-search (str)
  "Return match for either custom-id or org heading.
If the link is for a custom-id then search for that else if it's
a fuzzy link then search for a match for minimum three words of
STR of org heading.  If heading contains less than 3 words, then
an exact match is searched.

After match, with non-nil \\[universal-argument], execute a function
 according to `util/org-execute-search-prefix-arg-behaviour'.

If a fuzzy heading search is performed, then the case match
behaviour is controlled by `util/org-execute-search-ignore-case'."
  (when (derived-mode-p 'org-mode)
    (let* ((buf (current-buffer))
           (case-fold-search t)
           (words (split-string (string-remove-prefix "*" str) " "))
           (todo-comment-re (if (derived-mode-p 'org-mode)
                                (rx (group (regexp org-todo-regexp)))
                              (rx (or (regexp org-comment-string)))))
           (custom-id-re (when (pcase (string-match-p "^#[a-zA-Z0-9_-]+$" str)
                                 (0 t)
                                 (_ nil))
                           (concat " *?:CUSTOM_ID: *?" (string-remove-prefix "#" str))))
           (title (if (< (length words) 3)
                      (string-join words " ")
                    (concat (mapconcat #'regexp-quote (-take 3 words) ".+") ".*")))
           (title-re (rx bol (+ "*") " "
                       (opt (seq (regexp todo-comment-re) " "))
                       (group (regexp title))
                       eol))
           (cookie-re "\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]")
           (comment-re org-comment-regexp)
           matches)
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (if custom-id-re
              (progn (re-search-forward custom-id-re)
                     (push (list (util/org-execute-search-heading-length-subr
                                  words comment-re cookie-re)
                                 (progn (outline-back-to-heading) (point))
                                 (org-entry-get (point) "PDF_FILE"))
                           matches))
            (while (re-search-forward title-re nil t)
              (push (list (util/org-execute-search-heading-length-subr
                           words comment-re cookie-re)
                          (point)
                          (org-entry-get (point) "PDF_FILE"))
                    matches))))
        (let* ((pt (and matches (cadr (-max-by (lambda (x y) (> (car x) (car y))) matches))))
               (pdf-prop (org-entry-get pt "PDF_FILE"))
               (pdf-file (when (and pdf-prop (string-match util/file-link-re pdf-prop))
                           (match-string 1 pdf-prop))))
          (if current-prefix-arg
              ;; NOTE: Call an appropriate function if prefix arg
              (util/org-execute-search-funcall `(:pt ,pt :pdf-file ,pdf-file))
            (goto-char pt)
            (beginning-of-line))))
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
         (cons 'exclude (->> expr
           (replace-regexp-in-string "\\*" "\\\\*")
           (replace-regexp-in-string (rx "/" (group (+ any)) eol) "./\\1")
           (format "\\! -path \\*%s\\*"))))))

(defun util/ffip-search-paths (pat)
  "Like `util/ffip-search' but search pattern PAT as path instead."
  (interactive
   (list (read-string "Enter the search term: ")))
  (util/ffip-search pat t))

(defvar util/ffip-debug nil)
(defvar util/ffip-debug-cmd nil)
(defun util/ffip-search (&optional pattern search-for-path)
  "Search for PATTERN and list files in the project in a dired buffer.
If optional VC or `current-prefix-arg' is non-nil and if the
project is version controlled (git only for now) then list all
the files in version control.

Optional SEARCH-FOR-PATH modifies the find behaviour to use
\"-ipath\" instead of \"-iname\"."
  (interactive "p")
  (let* ((ci (not pattern))
         (pattern (pcase pattern
                    ((pred stringp) pattern)
                    (1 (read-string "Enter the search term: "))
                    ('nil (user-error "Error. Pattern is required if not called interactively."))
                    (_ pattern)))
         (root (ffip-project-root))
         (gitignore (f-join root ".gitignore"))
         (gitignore-open (find-buffer-visiting gitignore))
         (prune-patterns (-uniq (-concat (mapcar
                                          (lambda (x) (replace-regexp-in-string "^\\*/\\(.+\\)" "/\\1/" x))
                                          ffip-prune-patterns)
                                         '("/env/" "/node_modules/" "/build/" "/dist/"))))
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

(defun util/read-if-not-nil (sym val &optional prompt)
  "Read from minibuffer if SYM is nil with default value VAL."
  (format "%s" (or sym
                   (read-from-minibuffer (format (or prompt "Enter the file pattern (default %s): ") val)
                                         nil nil t nil val))))

(defun util/ffip-grep-pattern (&optional file-pattern grep-pattern)
  "Grep for GREP-PATTERN in files returned by `util/ffip-search'.
The files are matched with FILE-PATTERN."
  (interactive "p")
  (let* ((phrase (thing-at-point 'symbol t))
         (file (concat "*." (car (last (split-string (buffer-file-name) "\\.")))))
         (file-pattern (util/read-if-not-nil file-pattern file))
         (grep-pattern (util/read-if-not-nil grep-pattern phrase))
         (files (util/ffip-search file-pattern))
         grep-save-buffers)
    (if files
        (grep (string-join (-concat `("grep --color -nH --null -i -E" ,grep-pattern) files) " "))
      (message "No files found matching pattern %s" grep-pattern))))

(defun util/ffip-grep-default ()
  "Grep for PATTERN in files of current mode."
  (interactive)
  (util/ffip-grep-pattern (concat "*." (car (last (split-string (buffer-file-name) "\\."))))))

(defalias 'util/ffip-gg 'util/ffip-grep-git-files)

(defun util/ffip-grep-git-files (&optional pattern)
  "Grep for PATTERN in git staged files."
  (interactive)
  (let* ((root (ffip-project-root))
         (phrase (thing-at-point 'symbol t))
         (files (-filter (lambda (x) (not (string-empty-p x)))
                         (split-string (shell-command-to-string (format "cd %s && git ls-files" root)))))
         (pattern (util/read-if-not-nil pattern phrase "Grep in git files: (default %s): "))
         (default-directory root)
         grep-save-buffers)
    (if files
        (grep (string-join (-concat `("grep --color -nH --null -i -E" ,pattern) files) " "))
      (message "No files found matching pattern %s" pattern))))

(defun util/rgrep-default-search (regexp)
  "`rgrep' for REGEXP in current directory for files with current extension.
The REGEXP pattern is asked on the prompt by default.

See also `util/ffip-grep-git-files' and `util/ffip-grep-default'."
  (interactive (list (let ((phrase (thing-at-point 'symbol t)))
                       (read-from-minibuffer (format "Regexp (default %s): " phrase)
                                             nil nil t nil phrase))))
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
  (let* ((max-val 0) (ind -1) (max 0))
    (cl-loop for x in seq
          do
          (progn
            (setq ind (+ 1 ind))
            (if x (if (> x max-val)
                      (progn (setq max-val x)
                             (setq max ind))))))
    max))

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

(defun util/org-heading-and-body-bounds ()
  "Return bounds of text body if present in org subtree.

Return value is a triple of '(beg end has-body) where beg is the
point after metadata, end is the point at end of subtree and
has-body indicates if any text is present."
  (let* ((beg (progn (unless (org-at-heading-p)
                       (outline-back-to-heading))
                     (point)))
         (end (progn
                (outline-next-heading)
                (point)))
         (has-body (not (string-empty-p
                         (string-trim
                          (buffer-substring-no-properties beg end))))))
    (list beg end has-body)))

;; FIXME: This leaves the subtree in "show" mode. Should save subtree state
(defun util/org-narrow-to-heading-and-body ()
  "Narrow to the current heading and the body.
Unlike `org-narrow-to-subtree' any headings which are children of
the current heading are excluded."
  (pcase-let ((`(,beg ,end ,has-body) (util/org-heading-and-body-bounds)))
    )
  ;; (let (pmin pmax)
  ;;   (org-narrow-to-subtree)
  ;;   (save-excursion
  ;;     (goto-char (point-min))
  ;;     (org-show-subtree)
  ;;     (beginning-of-line)
  ;;     (if (eq (point-min) (point))
  ;;         (setq pmin (point))
  ;;       (org-previous-visible-heading 1)
  ;;       (setq pmin (point)))
  ;;     (org-next-visible-heading 1)
  ;;     (setq pmax (point))
  ;;     (narrow-to-region pmin pmax)))
  )

(defun util/org-get-subtree-with-body-for-heading-matching-re (str)
  (let ((case-fold-search t)
        match)
    (goto-char (point-min))
    (and (re-search-forward (concat "^\\*+.+" (regexp-quote str)) nil t)
         (goto-char (match-beginning 0))
         (setq match (match-beginning 0)))
    (unless match
      (debug))
    (when match
      (util/org-narrow-to-heading-and-body)
      (buffer-string))))

(defun util/org-kill-new-or-append-subtree ()
  "Kill or append to last kill current subtree.
`kill-new' a subtree if previous kill was not an org heading,
append to last kill otherwise.  With non-nil `current-prefix-arg'
remove the subtree from the buffer also."
  (interactive)
  (util/with-org-mode
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
   (delete-blank-lines)))

(defun util/org-remove-subtrees-matching-re (re &optional recurse)
  "Remove subtrees from an `org-mode' buffer whose headlines match regexp RE.
Remove only at one depth below the current subtree.  The buffer
to operate on must be an org buffer.  Optional RECURSE is not
used as of now."
  (let (regions)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (el)
        (let ((hbeg (plist-get (cadr el) :contents-begin))
              (hend (plist-get (cadr el) :contents-end)))
          (when (string-match-p re (plist-get (cadr el) :raw-value))
            (when (and hbeg hend)
              (push (cons hbeg hend) regions))))))
    (mapcar (lambda (x) (delete-region (car x) (cdr x))) regions)))

(defun util/org-check-fix-custom-ids ()
  "Check for duplicate custom ids in an org buffer and fix them."
  (debug))

(defvar util/org-heading-props-filter-p nil
  "Additional property filter to apply to headings while collecting them.
Used by `util/org-collect-headings-subr'")
(defun util/org-collect-headings-subr (predicate)
  "Return headings in an org buffer.
Select only those headings which satisfy the unary PREDICATE.
See `util/org-default-heading-filter-p' for an example of such a
predicate."
  (let (headings)
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward org-complex-heading-regexp nil t)
        (let* ((el (org-element-at-point))
               (heading (or (org-element-property :title el)
                            (org-get-heading t t t t)))
               (author (or (org-element-property :AUTHOR el) ""))
               (custom-id (or (org-element-property :CUSTOM_ID el) ""))
               (pos (org-element-property :begin el))
               (buf (buffer-name)))
          (when (funcall predicate heading)
            (push `(,heading ,author ,buf ,custom-id ,pos) headings))))
      headings)))

(defvar util/org-collect-headings-cache nil
  "Alist of collected headings cache.")
(defvar util/org-collect-headings-files nil
  "List of files from which to collect headings.")
(defvar util/org-collect-files-modtimes nil
  "Modification times of buffers from which to collect headings.")
(defvar util/org-collect-buffers nil
  "List of buffers from which to collect headings.")

(defun util/org-collect-setup ()
  "Setup the `util/org-collected-headings' variables."
  (interactive)
  (setq util/org-collect-buffers
        (mapcar #'find-file-noselect util/org-collect-headings-files))
  (setq util/org-collect-files-modtimes
        (mapcar
         (lambda (buf) (cons (buffer-name buf)
                             (file-attribute-modification-time
                              (file-attributes (buffer-file-name buf)))))
         util/org-collect-buffers)))

;; TODO: Add to cache as current buffer updates.
;; TODO: Filter by bufname
(defun util/org-collected-headings (predicate &optional bufname no-refresh)
  "Return headings in an org buffer satisfying unary PREDICATE.
See `util/org-default-heading-filter-p' for an example of such a
predicate."
  ;; check mod times of buffers or files
  ;; If updated, add to cache
  ;; Search only in entry cache
  (when util/org-collect-headings-files
    (cond ((not util/org-collect-buffers)
           (util/org-collect-setup))
          ((not (-all? #'buffer-live-p util/org-collect-buffers))
           (util/org-collect-setup))))
  (let* ((modtimes (mapcar
                    (lambda (buf) (cons (buffer-name buf)
                                        (file-attribute-modification-time
                                         (file-attributes (buffer-file-name buf)))))
                    util/org-collect-buffers))
         (bufnames (if bufname
                       (list bufname)
                     (mapcar #'buffer-name util/org-collect-buffers)))
         (headings (mapcar (lambda (bufname)
                             (cons bufname
                                   (progn
                                     ;;  buf is missing from cache
                                     (unless no-refresh
                                       (when (or (not (a-get util/org-collect-headings-cache bufname))
                                                 ;;  cached-buf-modtime < current-buf-modtime
                                                 (time-less-p (a-get util/org-collect-files-modtimes
                                                                     bufname)
                                                              (a-get modtimes bufname)))
                                         (setq util/org-collect-headings-cache
                                               ;; update cache
                                               (a-assoc util/org-collect-headings-cache
                                                        bufname
                                                        (with-current-buffer bufname
                                                          (util/org-collect-headings-subr predicate)))
                                               util/org-collect-files-modtimes
                                               ;; update modtimes
                                               (a-assoc util/org-collect-files-modtimes
                                                        bufname
                                                        (a-get modtimes bufname)))))
                                     (a-get util/org-collect-headings-cache bufname))))
                           bufnames)))
    headings))

(defun util/org-default-heading-filter-p (heading)
  "Default predicate for `util/org-collect-headings-subr'.
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

(defun util/org-get-text-links (link-re narrow &optional file-prefix)
  "Get all the links matching LINK-RE in an org buffer.
When NARROW is non-nil, first narrow to subtree.  When optional
FILE-PREFIX is non-nil, insert the file path for fuzzy links for
current buffer so that it becomes an absolute link."
  (let (temp)
    (save-restriction
      (save-excursion
        (when narrow
          (org-narrow-to-subtree))
        (goto-char (point-min))
        (while (re-search-forward link-re nil t nil)
          (when (and (match-string 1) (match-string 2))
            (let ((elem (list (substring-no-properties (match-string 2))
                              (if file-prefix
                                  (let ((sub (substring-no-properties (match-string 1))))
                                    (if (and (string-match-p "^\\*" sub)
                                             (not (string-match-p "^file" sub)))
                                        (concat "file:" (buffer-file-name) "::" sub)
                                      sub))
                                (substring-no-properties (match-string 1))))))
              (push elem temp))))))
    temp))

;; Was initially '(subtree buffer research-files)
(defvar util/org-insert-link-to-heading-prefix-behaviour '(((4) . buffer) ((16) . subtree))
  "Behaviour of `current-prefix-arg' for `util/org-insert-link-to-heading'.

A list of symbols `subtree' `research-files' `buffer'.  The first
element in the list corresponds to single universal prefix
argument `C-u'.  The second element to two universal prefix
arguments and the last one to no argument.")

(defun util/org-heading-first-subheading ()
  "Return the point of first subheading if heading has subheadings.
If no subheadings exist, return nil."
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (outline-next-heading))))

(defun util/org-get-tree-prop (prop &optional heading)
  "Return point up the tree checking for property PROP.
PROP is check at heading or its current parent recursively.  With
non-nil HEADING, return heading at the point instead of point."
  (save-excursion
    (outline-back-to-heading)
    (if (org-entry-get (point) prop)
        (if heading
            (substring-no-properties (org-get-heading t t t t))
          (point))
      (let (is-doc-root no-doc-root)
        (while (and (not is-doc-root) (not no-doc-root))
          (condition-case nil
              (outline-up-heading 1 t)
            (error (setq no-doc-root t)))
          (setq is-doc-root (org-entry-get (point) prop)))
        (and is-doc-root (if heading
                             (substring-no-properties (org-get-heading t t t t))
                           (point)))))))

(defun util/org-heading-matching-re (re &optional subtree)
  "Goto first heading matching regexp RE.
If optional SUBTREE is non-nil, search only in current subtree."
  (save-restriction
    (save-excursion
      (when subtree
        (org-narrow-to-subtree))
      (let ((case-fold-search t)
            ref)
        (while (outline-next-heading)
          (when (string-match-p re
                                (string-trim (org-get-heading t t t t)))
            (setq ref (point))))
        ref))))

(defun util/org-insert-link-to-heading (&optional clip-func citation refs cache-only)
  "Insert a link to selected heading.
Description of the link is determined by optional CLIP-FUNC.
If not given, it defaults to `identity'.

If optional CITATION is non-nil, the headings are gathered for
only the current buffer and citations within the current
`doc-root' are searched. A `doc-root' is an org subtree with the
non-nil property DOC_ROOT.

When optional REFS is non-nil and an immediate sub-heading of
`doc-root' named \"References\" exists, then those are also
offered as options.

For customizing how headings are gathered, change the function
`util/org-default-heading-filter-p'.

See also, `util/org-collect-headings-subr' and
`util/org-collected-headings'."
  (interactive)
  (let* ((read-from (or (a-get util/org-insert-link-to-heading-prefix-behaviour
                               current-prefix-arg)
                        'research-files))
         (clip-func (or clip-func #'identity))
         (text-link-re util/org-text-link-re)
         (headings (pcase read-from
                     ('buffer (cdar (util/org-collected-headings
                                     #'util/org-default-heading-filter-p
                                     (buffer-name) cache-only)))
                     ('research-files (apply #'-concat
                                             (a-vals
                                              (util/org-collected-headings
                                               #'util/org-default-heading-filter-p
                                               nil cache-only))))
                     ('subtree (save-restriction
                                 (org-narrow-to-subtree)
                                 (util/org-collect-headings-subr #'util/org-default-heading-filter-p)))))
         (doc-root (when citation (or (util/org-get-tree-prop "DOC_ROOT")
                                      (save-excursion (outline-back-to-heading (point))))))
         ;; org links in current subtree text
         (subtree-text-links (when citation
                               (let ((temp (util/org-get-text-links text-link-re t)))
                                 (when temp
                                   (mapcar (lambda (x)
                                             (cons (concat (replace-regexp-in-string text-link-re "\\2" (car x))
                                                           " (subtree)")
                                                   x))
                                           (-uniq temp))))))
         ;; org links in text from the document root
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
         ;; Get links from subtree in references section of doc root if it
         ;; exists
         (references (when refs
                       (save-excursion
                         (if doc-root
                             (goto-char doc-root)
                           (outline-back-to-heading))
                         (let* ((refs (util/org-heading-matching-re "^references$" t))
                                (sub (when refs
                                       (goto-char refs)
                                       (util/org-heading-first-subheading))))
                           (and refs sub
                                (save-restriction
                                  (org-narrow-to-subtree)
                                  (narrow-to-region sub (point-max))
                                  (util/org-collect-headings-subr #'identity)))))))
         ;; `subtree-text-links' at the beginning of selections
         ;; then `doc-root-text-links'
         ;; then `references'
         ;; then rest of headings
         (selections (-concat (a-keys subtree-text-links)
                              (a-keys doc-root-text-links)
                              (mapcar (lambda (x)
                                        (concat (string-join (-take 2 x) " ") " (references)"))
                                      references)
                              (mapcar (lambda (x)
                                        (string-join (pcase read-from
                                                       ((or 'buffer 'subtree) (-take 2 x))
                                                       ('research-files (-take 3 x)))
                                                     " "))
                                      headings)))
         (prompt (pcase read-from
                   ('buffer (format "Insert %s (cur-buf): " (if cache-only "from cache" "link")))
                   ('subtree (format "Insert %s (subtree): " (if cache-only "from cache" "link")))
                   ('research-files (format "Insert %s (files): " (if cache-only "from cache" "link")))))
         (selected (ido-completing-read prompt selections)))
    (cond ((string-suffix-p " (subtree)" selected)
           (insert (apply #'format "[[%s][%s]]" (reverse (a-get subtree-text-links selected)))))
          ((string-suffix-p " (citations)" selected)
           (insert (apply #'format "[[%s][%s]]" (reverse (a-get doc-root-text-links selected)))))
          ((string-suffix-p " (references)" selected)
           (let* ((indx (- (-elem-index selected selections) (length subtree-text-links)
                           (length doc-root-text-links)))
                  (file (format "file:%s::" (buffer-file-name (current-buffer))))
                  (heading (car (nth indx references)))
                  (custom-id (nth 3 (nth indx references))))
             (if (not (string-empty-p custom-id))
                 (insert (format "[[%s#%s][%s]]" file custom-id (funcall clip-func heading)))
               (insert (format "[[%s*%s][%s]]" file heading (funcall clip-func heading))))))
          (t (let* ((indx (- (-elem-index selected selections) (length subtree-text-links)
                             (length doc-root-text-links) (length references)))
                    (file (pcase read-from
                            ('research-files (format
                                              "file:%s::"
                                              (buffer-file-name
                                               (get-buffer (nth 2 (nth indx headings))))))
                            (_ (buffer-file-name (current-buffer)))))
                    (heading (pcase read-from
                               ('research-files (car (nth indx headings)))
                               (_ (car (nth indx headings)))))
                    (custom-id (nth 3 (nth indx headings))))
               (if (not (string-empty-p custom-id))
                   (insert (format "[[%s#%s][%s]]" file custom-id (funcall clip-func heading)))
                 (insert (format "[[%s*%s][%s]]" file heading (funcall clip-func heading)))))))))

(defun util/org-insert-citation-to-heading ()
  "Insert a citation to a heading.
Call `util/org-insert-link-to-heading' so that the description of
the link is is first two words of the heading.  The headings are
filtered by length and only headings greater than
`util/org-min-collect-heading-length' are searched."
  (interactive)
  (util/org-insert-link-to-heading (-rpartial #'util/non-stop-words-prefix 2) t t))

(defun util/org-insert-citation-to-heading-from-cache ()
  "Insert a citation to a heading from cache only.
Like `util/org-insert-citation-to-heading' except doesn't rebuild
cache on buffer modification."
  (interactive)
  (util/org-insert-link-to-heading (-rpartial #'util/non-stop-words-prefix 2) t t t))

(defmacro util/org-collect-duplicate-subr (heading pos headings dups strings predicate ignore-case test)
  "Subroutine for `util/org-collect-duplicate-headings'.
See `util/org-collect-duplicate-headings' for details.  `defun'
doesn't work when passing references to headings and dups for
some reason."
  `(let ((item (cons heading pos))
         (check (if ignore-case (downcase heading) heading)))
     (if predicate
         (when (funcall predicate heading)
           (push item headings)
           (push check strings)
           (when (> (cl-count check strings :test test) 1)
             (let* ((old-pos (cl-position check (reverse strings) :test test))
                    (old-heading (nth old-pos (reverse headings))))
               (unless (cl-member old-heading dups :test test)
                 (push old-heading dups))
               (push item dups))))
       (push item headings)
       (push check strings)
       (when (> (cl-count check strings :test test) 1)
         (let* ((old-pos (cl-position check (reverse strings) :test test))
                (old-heading (nth old-pos (reverse headings))))
           (unless (cl-member old-heading dups :test test)
             (push old-heading dups))
           (push item dups))))))

(defvar util/org-helm-use-headings-cache t
  "Whether to use headings from `util/org-headings-cache'.
Cache is returned from `util/org-collected-headings' and is
auto updated if the file has changed on disk.  See
`util/org-collected-headings'.")

(defvar util/org-duplicate-headings nil
  "Global variable to easily access duplicate headings found in buffer.")

;; TODO: match headings also which differ in at most 2 words
;; TODO: match headings in which one is substring of other
(defun util/org-collect-duplicate-headings (&optional predicate ignore-case test)
  "Collect duplicate headings in an org buffer.
With optional boolean function PREDICATE, collect those which
only satisfy it.

With optional non-nil IGNORE-CASE, ignore case while searching.
Optional TEST is which test function to use for `cl-count'.
Defaults to `equal'."
  (let* ((test (or test 'equal))
         (in-cache (when util/org-helm-use-headings-cache
                     (member (buffer-name) (a-keys (util/org-collected-headings
                                                    #'util/org-default-heading-filter-p
                                                    (buffer-name))))))
         headings dups strings)
    (if in-cache
        (let ((cache (a-get util/org-collect-headings-cache (buffer-name))))
          (dolist (elem cache)
            (let ((heading (car elem))
                  (pos (-last-item elem)))
              (util/org-collect-duplicate-subr heading pos headings dups strings
                                               predicate ignore-case test))))
      (save-excursion
        (goto-char (point-max))
        (while (re-search-backward org-complex-heading-regexp nil t)
          (let* ((el (org-element-at-point))
                 (heading (org-element-property :title el))
                 (pos (org-element-property :begin el)))
            (util/org-collect-duplicate-subr heading pos headings dups strings
                                             predicate ignore-case test)))))
    (setq util/org-duplicate-headings
          (sort dups (lambda (x y) (string-greaterp (car y) (car x)))))
    util/org-duplicate-headings))

(defun util/org-helm-show (char)
  (interactive)
  (goto-char char)
  (recenter-top-bottom)
  (org-show-entry)
  (save-excursion
    (let ((bounds (org-get-property-block)))
      (when bounds
        ;; HACK (car bounds) gives node-property which org complains is not a drawer
        (goto-char (- (car bounds) 1))
        (org-hide-drawer-toggle)))))

(defun util/org-helm-delete (&rest args)
  "Cut the subtree at point in current helm buffer."
  (interactive)
  (with-helm-current-buffer
    (if (> (util/org-count-subtree-children) 0)
        (message "Cannot delete node with children")
      (org-copy-subtree nil t))))

(defun util/org-helm-show-outline (&rest args)
  "Print the outline path for heading at point in helm"
  (interactive)
  (with-helm-current-buffer
    (message (org-format-outline-path (org-get-outline-path)))))

;; TODO: If they're in the same subtree, just delete one of them
;; TODO: Subtract the number of characters from each POS after POINT from DUPS
;; TODO: Don't delete self.
(defun util/org-helm-replace-heading-as-list-item (&rest args)
  "Delete a duplicate org heading at point and insert a link to original.
Inserts a link to the first org heading in
`util/org-duplicate-headings'.  `util/org-duplicate-headings' is
set and returned by `util/org-collect-duplicate-headings'.  See
that function for implementation details."
  (interactive "P")
  (with-helm-current-buffer
    (let* ((case-fold-search t)
           (heading (org-get-heading t t t t))
           (first (-first-item
                   ;; NOTE: Actually the duplicates depend on the predicate
                   ;;       While here I'm just doing a `string-match-p'
                   (-filter (lambda (x) (string-match-p (car x) heading))
                            util/org-duplicate-headings)))
           (children (util/org-count-subtree-children)))
      (cond ((> children 0)
             (message "Cannot delete node with children"))
            ((eq (point) (cdr first))
             (message "Cannot delete first heading"))
            (t
             (org-copy-subtree nil t)
             (outline-up-heading 1 t)
             (org-end-of-meta-data)
             (open-line 1)
             (org-indent-line)
             (let* ((instr (format "- [[%s::*%s][%s]]" (buffer-file-name) (car first) (car first)))
                    (inslen (1+ (length instr)))
                    (pt (point))
                    (offset (+ (length org-subtree-clip) inslen)))
               (insert instr)
               (goto-char (point))
               (outline-up-heading 1)
               ;; Call helm again with different sources
               ;; TODO: This is broken:
               ;;       1. The positions post update are not correct
               ;;       2. Need to update sources correctly
               ;; (helm :sources (helm-build-sync-source "Duplicate headings"
               ;;                  :candidates (lambda ()
               ;;                                (with-helm-current-buffer
               ;;                                  (-keep (lambda (x)
               ;;                                           (when (not (string-match-p (car x) heading))
               ;;                                             (if (> (cdr x) pt)
               ;;                                                 (cons (car x) (- (cdr x) offset))
               ;;                                               x)))
               ;;                                         util/org-duplicate-headings)))
               ;;                  :follow 1
               ;;                  :action 'util/org-helm-show-dup-actions
               ;;                  :keymap util/helm-org-map)))
               ))))))

(defvar util/org-helm-show-dup-actions
  (helm-make-actions
   "Show Entry" 'util/org-helm-show
   "Delete Entry" 'util/org-helm-delete
   "Show Entry Path" 'util/org-helm-show-outline
   "Replace with link" 'util/org-helm-replace-heading-as-list-item))

(defvar util/helm-org-map
  (let ((new-map (copy-keymap helm-map)))
    (define-key new-map (kbd "C-k") 'util/org-helm-delete)
    (define-key new-map (kbd "C-l") 'util/org-helm-show-outline)
    (define-key new-map (kbd "C-R") 'util/org-helm-replace-heading-as-list-item)
    new-map)
  "Keymap for `helm-org-rifle'.")

;; TODO: Should the names be util/org-helm-* or util/helm-org-* ?
(defun util/org-helm-show-duplicate-headings (&optional pred ignore-case)
  "Show duplicate headings in an org buffer with `helm'.
With optional predicate PRED, show only those which satisfy the
predicate.  With optional non-nil IGNORE-CASE, ignore case while
searching."
  (interactive)
  (util/with-org-mode
   (helm :sources (helm-build-sync-source "Duplicate headings"
                    :candidates (lambda ()
                                  (with-helm-current-buffer
                                    (util/org-collect-duplicate-headings pred ignore-case)))
                    :follow 1
                    :action 'util/org-helm-show-dup-actions
                    :keymap util/helm-org-map))))

(defun util/org-collect-duplicate-customids (&optional predicate test)
  "Check the buffer for duplicate customids.
With optional boolean function PREDICATE, check only those org
entries which satisfy it."
  (let ((custom-id-re (rx (*? " ") ":CUSTOM_ID:" (*? " ") (group (+ (any "a-zA-Z0-9_-")))))
        (test (or test 'equal))
        headings dups strings)
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward custom-id-re nil t)
        (let* ((id (match-string 1))
               (el (progn (outline-back-to-heading)
                          (org-element-at-point)))
               (heading (org-element-property :title el))
               (pos (org-element-property :begin el))
               (item (cons heading pos)))
          (if predicate
              (when (funcall predicate heading)
                (push item headings)
                (push id strings)
                (when (> (cl-count item strings :test test) 1)
                  (let* ((old-pos (cl-position item (reverse strings) :test test))
                         (old-heading (nth old-pos (reverse headings))))
                    (unless (cl-member old-heading dups :test test)
                      (push old-heading dups))
                    (push item dups))))
            (push item headings)
            (push id strings)
            (when (> (cl-count id strings :test test) 1)
              (let* ((old-pos (cl-position id (reverse strings) :test test))
                     (old-heading (nth old-pos (reverse headings))))
                    (unless (cl-member old-heading dups :test test)
                      (push old-heading dups))
                    (push item dups)))))))
    dups))


(defun util/org-helm-show-duplicate-customids (&optional pred)
  (interactive)
  (util/with-org-mode
   (helm :sources (helm-build-sync-source "Duplicate Custom IDs"
                    :candidates (lambda ()
                                  (with-helm-current-buffer
                                    (util/org-collect-duplicate-customids pred)))
                    :follow 1
                    :action 'util/org-helm-show-dup-actions
                    :keymap util/helm-org-map))))

(defun util/org-helm-headings-subr (&optional pred)
  (let ((in-cache (when util/org-helm-use-headings-cache
                    (member (buffer-name) (a-keys (util/org-collected-headings
                                                   #'util/org-default-heading-filter-p
                                                   (buffer-name))))))
        (pred (or pred #'identity))
        headings)
    (if in-cache
        (let ((cache (a-get util/org-collect-headings-cache (buffer-name))))
          (setq headings (-filter #'identity
                                  (mapcar (lambda (x) (when (funcall pred (car x))
                                                        (cons (car x) (-last-item x))))
                                          cache))))
      (save-excursion
        (goto-char (point-max))
        (while (re-search-backward org-complex-heading-regexp nil t)
          (let* ((el (org-element-at-point))
                 (heading (org-element-property :title el))
                 (pos (org-element-property :begin el)))
            (when (funcall pred heading)
              (push (cons heading pos) headings))))))
    headings))

(defun util/org-helm-headings (&optional pred)
  "Navigate through headings in an org buffer with `helm'.
With optional predicate PRED, show only those headings which satisfy the
predicate."
  (interactive)
  (util/with-org-mode
   (helm :sources (helm-build-sync-source "Org Headings"
                    :candidates (lambda ()
                                  (with-helm-current-buffer
                                    (util/org-helm-headings-subr pred)))
                    :follow 1
                    :action (helm-make-actions
                             "Show Entry" 'util/org-helm-show)))))

(defun util/insert-heading-from-url (&optional with-header)
  "Fetch the title from an optional URL.
URL is copied from clipboard if not given.

Requires python, and python packages \"bs4\", \"requests\" and
\"brotli\" to be installed in the python env."
  (interactive)
  (util/with-org-mode
   (org-insert-heading-respect-content)
   (newline)
   (org-indent-line)
   (insert "- ")
   (yank)
   (let ((headers (if with-header "headers={\"accept\": \"text/html,application/xhtml+xml,application/xml;\", \"accept-encoding\": \"gzip, deflate\", \"accept-language\": \"en-GB,en-US;q=0.9,en;q=0.8\", \"cache-control\": \"no-cache\", \"user-agent\": \"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko)\"}" "")))
     (org-edit-headline
      (string-trim (shell-command-to-string
                    (format "%s -c 'import requests; from bs4 import BeautifulSoup; print(BeautifulSoup(requests.get(\"%s\" %s).content).title.text)'"
                            util/insert-heading-python-executable
                            (org-element-property :raw-link (org-element-context))
                            headers)))))))

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

(provide 'util)

;;; util.el ends here


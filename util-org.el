;;; util-org.el --- `org-mode' utilty functions. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Thursday 14 October 2021 18:50:15 PM IST>
;; Keywords:	org, utility

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
;;
;; A bunch of functions and commands I wrote while using `org-mode'. Some of
;; them have become quite powerful and useful so they warrant a separate
;; package. Perhaps even separate them from this repo.
;;
;; The utility functions I've used begin with `util/' prefix and I've kept the
;; convention going here. You'll have to load them as "(require 'util/org util-org)"
;; for it to load correctly; notice the file name in the end.

;;; Code:

(require 'org)
(require 'org-element)
(require 'util)

;; (declare-function org-hide-drawer-toggle "org")

(defvar util/org-file-link-re
  (rx "[" "[" (group (seq (opt "file:") (or "/" "~") (+? any))) "]"
      (opt (seq "[" (group (+? any)) "]")) "]")
  "Matches any org file link.")

(defvar util/org-fuzzy-link-re
  (rx "[" "[" (group (seq (or (regexp "file.+?::\\*") "*") (+? nonl))) "]"
      "[" (group (+? nonl)) "]" "]")
  "Regexp for matching an org fuzzy link.")

(defvar util/org-fuzzy-or-custom-id-link-re
  (rx "[" "[" (group (seq (opt (opt "file:") (opt "//") (or "/" "~") (regexp ".+?::"))
                          (or "*" "#") (+? any)))
      "]" "[" (group (+? any)) "]" "]")
  "Regexp for matching an org fuzzy or custom-id text link.
First group match gives the link and the second the description.")

(defvar util/org-text-link-re
  (rx "[" "[" (group (or (seq (opt (opt "file:") (opt "//") (or "/" "~") (regexp ".+?::"))
                              (or "*" "#") (+? any))
                         (regexp "http.+?")))
      "]" "[" (group (+? any)) "]" "]")
  "Regexp for matching a org text link.
Matches square bracket links or http links in org text.  First
group match gives the link and the second the description.")

(defvar util/org-min-collect-heading-length 1)

(defvar util/org-simple-regexp-search-modes
  '(emacs-lisp-mode lisp-mode python-mode javascript-mode
                    rjsx-mode fundamental-mode text-mode)
  "Modes for which org should do a simple regexp search.
Used by `util/org-execute-simple-regexp-search'.")

(defmacro util/with-org-mode (&rest body)
  "Call form `body' only if major-mode is `org-mode'."
  ;; (declare (debug (form body)))
  `(util/with-check-mode
    'org-mode nil
    ,@body))

(defun util/org-remove-all-time-stamps ()
  (util/with-org-mode
   (goto-char (point-min))
   (while (re-search-forward (util/generate-org-ts-regexp org-time-stamp-formats)
                             nil t)
     (replace-match ""))))

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

(defvar util/org-execute-search-prefix-arg-behaviour-alist nil
  "`util/org-execute-org-heading-search-*' functions to call for \\[universal-argument].

This should be an alist of type '(number . function), where the
number corresponds to the prefix argument converted to integer.")

(defvar util/org-execute-search-ignore-case t
  "Ignore case for headings search.")


;; FIXME: This is a bad hack. This should be fixed from `org-link-open'
(defun util/org-execute-search-other-window (args)
  "Execute search in other window."
  (let ((buf (plist-get args :buffer))
        (pt (plist-get args :pt))
        (orig-mark (car org-mark-ring))
        (win (util/get-or-create-window-on-side)))
    (set-window-buffer win buf)
    (goto-char pt)
    (setq win (util/get-or-create-window-on-side))
    (set-window-buffer win (marker-buffer orig-mark))))

(defun util/org-execute-search-find-pdf-file (args)
  "Open PDF or DJVU file for link if it exists."
  (let ((pdf-file (plist-get args :pdf-file))
        (pt (plist-get args :pt)))
    (if (and pdf-file (f-exists? pdf-file))
        (if (string-suffix-p ".djvu" pdf-file)
            (let ((async-shell-command-buffer 'new-buffer)
                  (async-shell-command-display-buffer nil))
              (async-shell-command (format "evince \"%s\"" pdf-file)))
          (find-file pdf-file))
      (message "No pdf file for link")
      (goto-char pt)
      (beginning-of-line)
      (org-reveal))))

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
;;
;; FIXME: While this functionality is correct, the behaviour that I want, that
;;        is, for `C-u-C-u-C-o' to open the link from even same file in a
;;        different window (which doesn't work right now)
(defun util/org-execute-customid-or-max-heading-match-search (str)
  "Return match for either custom-id or org heading.
If the link is for a custom-id then search for that else if it's
a fuzzy link then search for a match for minimum three words of
STR of org heading.  If heading contains less than 3 words, then
an exact match is searched.

After match, with non-nil \\[universal-argument], execute a function
 according to `util/org-execute-search-prefix-arg-behaviour-alist'.

If a fuzzy heading search is performed, then the case match
behaviour is controlled by `util/org-execute-search-ignore-case'."
  (when (derived-mode-p 'org-mode)
    (let* ((buf (current-buffer))
           (case-fold-search t)
           (words (split-string (string-remove-prefix "*" str) " "))
           ;; FIXME: I'm not sure this comment and todo regexp is correct
           (todo-comment-re (if (derived-mode-p 'org-mode)
                                (format "\\(?:%s\\|%s\\)?" org-todo-regexp org-comment-string)
                              (format "\\(?:%s\\)?" org-comment-string)))
           (custom-id-re (when (pcase (string-match-p "^#[a-zA-Z0-9_-]+$" str)
                                 (0 t)
                                 (_ nil))
                           (concat " *?:CUSTOM_ID: *?" (string-remove-prefix "#" str))))
           (title (if (< (length words) 3)
                      (string-join words " ")
                    (concat (mapconcat #'regexp-quote (-take 3 words) ".+") ".*")))
           (title-re
            ;; NOTE: Changed from rx to format for compatibility reasons
            ;; (rx bol (+ "*") " "
            ;;            (opt (seq (regexp todo-comment-re) " "))
            ;;            (group (regexp title))
            ;;            eol)
            (format "^\\*+ \\(?:\\(?:%s\\) \\)?\\(%s\\)$" todo-comment-re title))
           (cookie-re "\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]")
           (comment-re org-comment-regexp)
           matches)
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (if custom-id-re
              (when (re-search-forward custom-id-re)
                (org-reveal)
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
               (pdf-file (when (and pdf-prop (string-match util/org-file-link-re pdf-prop))
                           (match-string 1 pdf-prop)))
               (parg (pcase current-prefix-arg
                       ((and (pred (listp)) x) (car x))
                       ((and (pred (integerp)) x) x)
                       (_ nil)))
               (func (a-get util/org-execute-search-prefix-arg-behaviour-alist parg)))
          (when pt
            (if func
                (funcall func `(:buffer ,buf :arg ,parg :pt ,pt :pdf-file ,pdf-file))
              (goto-char pt)
              (beginning-of-line)
              (org-reveal)))))
      matches)))

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

(defun util/org-remove-links (predicate)
  "Remove all links from org buffer which satisfy PREDICATE."
  (when (eq major-mode 'org-mode)
    (goto-char (point-min))
    (while (re-search-forward (format " +%s" org-link-any-re) nil t)
      (let ((match (match-string 0)))
        (when (save-match-data (funcall predicate match))
          (replace-match " "))))))

(defun util/org-remove-all-file-links ()
  "Remove all file links from org buffer."
  (util/org-remove-links (-partial #'string-match-p util/org-file-link-re)))

(defun util/org-heading-and-body-bounds (&optional no-metadata)
  "Return bounds of text body if present in org subtree.

Return value is a triple of '(beg end has-body) where beg is the
point at heading, end is the point at end of subtree and
has-body indicates if any text is present.

If optional NO-METADATA is non-nil then 'beg points to beginning
of line after metadata."
  (let* ((beg (progn (save-excursion
                       (unless (org-at-heading-p)
                         (outline-back-to-heading))
                       (when no-metadata
                         (org-end-of-meta-data))
                       (point))))
         (end (save-excursion
                (progn
                  (outline-next-heading)
                  (max beg (- (point) 1)))))
         (has-body (not (string-empty-p
                         (string-trim
                          (buffer-substring-no-properties beg end))))))
    (list beg end has-body)))

;; FIXME: This leaves the subtree in "show" mode. Should save subtree state
(defun util/org-narrow-to-heading-and-body ()
  "Narrow to the current heading and the body.
Unlike `org-narrow-to-subtree' any headings which are children of
the current heading are excluded.

If optional NO-METADATA is non-nil then return text after
metadata."
  (pcase-let ((`(,beg ,end ,has-body) (util/org-heading-and-body-bounds)))
    (when (and has-body beg end)
      (narrow-to-region beg end))))

(defun util/org-narrow-to-text-body ()
  "Narrow to the current heading's text body only."
  (pcase-let ((`(,beg ,end ,has-body) (util/org-heading-and-body-bounds t)))
    (when (and has-body beg end)
      (narrow-to-region beg end))))

(defun util/org-get-subtree-with-body-for-heading-matching-re (str)
  "Get subtree with text body if heading matches STR."
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

(defun util/org-get-subtree-with-body-for-custom-id (str)
  "Get subtree with text body if CUSTOM_ID matches STR."
  (let ((case-fold-search t)
        match)
    (goto-char (point-min))
    (and (re-search-forward (concat " *?:CUSTOM_ID: *?" (string-remove-prefix "#" str)) nil t)
         (goto-char (match-beginning 0))
         (setq match (match-beginning 0)))
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

(defun util/org-get-headings-from-cache (bufname)
  (when (member bufname
                (a-keys (util/org-collected-headings
                         #'util/org-default-heading-filter-p
                         bufname)))
    (a-get util/org-collect-headings-cache bufname)))

;; TODO: This should be a hash table
;;       - Optionally collect headings at startup
;;       - Decouple cache from functions so same function
;;         can be used with multiple caches
(defvar util/org-collect-headings-cache nil
  "Alist of collected headings cache.
The cache is of the form `(,heading ,author ,buf ,custom-id ,pos).")
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
(defun util/org-collected-headings (predicate &optional bufname no-refresh)
  "Return headings in an org buffer satisfying unary PREDICATE.
See `util/org-default-heading-filter-p' for an example of such a
predicate.

With optional BUFNAME, return headings only for that buffer.

Optional non-nil NO-REFRESH implies to retrieve only from cache
if `util/org-use-headings-cache' is non-nil."
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

(defun util/org-delete-file-under-point ()
  "Delete file for link under point.
When in org mode, delete the link text also."
  (interactive)
  (pcase-let* ((link (get-text-property (point) 'htmlize-link))
               (context (org-element-context))
               (`(,beg ,end) (if context (list (plist-get (cadr context) :begin)
                                               (plist-get (cadr context) :end))
                               (list nil nil)))
               (uri (plist-get link :uri))
               (uri (and link uri (f-exists? uri) uri)))
    (if (not uri)
        (message "Nothing to do here")
      (f-delete uri)
      (message "Deleted file %s" uri)
      (if (and beg end)
          (delete-region beg end)
        (message "Could not delete the link text")))))

(defun util/org-rename-file-under-point (old new &optional name-only)
  "Rename the file under point from OLD to given NAME.
Update the org link also when in org mode.

When optional NAME-ONLY is non-nil, the link description is
contracted to file name only."
  (save-excursion
    (when (derived-mode-p 'org-mode)
      (let ((beg (cond ((or (looking-back "\\[\\[\\(.+?\\)" 1)
                            (looking-at "\\[\\[\\(.+?\\)"))
                        (match-beginning 0))
                       ((looking-at "\\(.+?\\)]]")
                        (- (match-beginning 0) 2))
                       (t nil)))
            end desc)
        (cond (beg
               (when (looking-at "\\(.+?\\)\\]]")
                 (setq end (match-end 0))
                 (setq desc (substring-no-properties
                             (replace-regexp-in-string "\\[\\[\\|.+?]\\[" "" (match-string 1))))
                 (when name-only
                   (setq desc (f-filename desc))))
               (delete-region beg end)
               (goto-char beg)
               (if (string= old new)
                   (message "New path is same as old path %s" old)
                 (rename-file old new)
                 (message "Renamed to %s" new))
               (if (and desc (not (string= desc old)))
                   (insert (format "[[%s][%s]]" new desc))
                 (insert (format "[[%s]]" new))))
              (t (user-error "Not at an org file link")))))))

;; TODO: Keep old name in some undo history, perhaps in a hash table
(defun util/org-move-file-under-point (call-method &optional newname)
  "Move file for a file link under point on the disk.
Update the org link also when in org mode."
  (interactive "p")
  (pcase-let* ((link (get-text-property (point) 'htmlize-link))
               (context (org-element-context))
               (`(,beg ,end) (if context (list (plist-get (cadr context) :begin)
                                               (plist-get (cadr context) :end))
                               (list nil nil)))
               (uri (plist-get link :uri))
               (uri (when uri (replace-regexp-in-string "file:" "" (plist-get link :uri))))
               (uri (and link uri (f-exists? uri) uri))
               (dir (when uri
                      (f-expand (concat (if (f-directory? uri)
                                            (string-remove-suffix "/" uri)
                                          (f-dirname uri))
                                        "/"))))
               (should-rename))
    (if (not uri)
        (message "Nothing to do here")
      (if newname
          (y-or-n-p (format "Move file to %s? " newname))
        (setq newname (f-expand (read-file-name "New name for file: " dir dir))))
      (let ((src-dir (f-directory? uri))
            (target-exists (f-exists? newname))
            (target-dir (f-directory? newname)))
        (cond ((and src-dir target-exists)
               (message "Cannot move source directory if target exists")
               (setq should-rename nil))
              ;; NOTE: move to target dir if target is a directory
              ((and src-dir target-dir)
               (message "Cannot move to target if both source and target are directories.")
               (setq should-rename nil))
              ((and target-dir (f-file? uri))
               (setq newname (f-join newname (f-filename uri)))
               (setq should-rename t))
              (target-exists
               (message "maybe overwrite")
               (setq should-rename nil))
              (t (setq should-rename t)))
        (when should-rename
          (util/org-rename-file-under-point uri newname (= call-method 4)))))))

(defun util/org-get-tree-prop (prop &optional heading)
  "Return point up the tree checking for property PROP.
PROP is checked at heading or its current parent recursively.
With optional non-nil HEADING, return heading at the point
instead of point."
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

The headings are cached and updated when one of the files in
`util/org-collect-headings-files' is modified.

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
                    (file (format
                           "file:%s::"
                           (pcase read-from
                             ('research-files (buffer-file-name
                                               (get-buffer (nth 2 (nth indx headings)))))
                             (_ (buffer-file-name (current-buffer))))))
                    (heading (pcase read-from
                               ('research-files (car (nth indx headings)))
                               (_ (car (nth indx headings)))))
                    (custom-id (nth 3 (nth indx headings))))
               (if (not (string-empty-p custom-id))
                   (insert (format "[[%s#%s][%s]]" file custom-id (funcall clip-func heading)))
                 (insert (format "[[%s*%s][%s]]" file heading (funcall clip-func heading)))))))))

;; TODO: these two should be `ref-man' functions
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

(defun util/org-filter-from-headings-cache (cache-name predicate &optional file-or-buffer)
  "Filter headings from `util/org-collect-headings-cache' with  CACHE-NAME.

The PREDICATE is run on each entry of the cache.

Optionally search only in entries for FILE-OR-BUFFER.
FILE-OR-BUFFER must be in `util/org-collect-buffers'."
  (let ((cache (if file-or-buffer
                   (a-get util/org-collect-headings-cache
                          (pcase (or (get-buffer file-or-buffer)
                                     (and (f-exists? file-or-buffer)
                                          (find-file-noselect file-or-buffer)))
                            ((and bufname) bufname)))
                 util/org-collect-headings-cache)))
    (-filter (lambda (x) (funcall predicate x))
             (-concat (a-vals util/org-collect-headings-cache)))))

(defmacro util/org-collect-duplicate-subr (heading pos headings dups strings predicate ignore-case test)
  "Subroutine for `util/org-collect-duplicate-headings'.
See `util/org-collect-duplicate-headings' for details.  `defun'
doesn't work when passing references to headings and dups for
some reason."
  (declare (debug (symbolp heading symbolp pos listp headings listp dups
                           listp strings symbolp predicate symbolp ignore-case symbolp test)))
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

(defmacro util/org-collect-duplicate-subr-other (item check headings dups strings predicate ignore-case test)
  "Subroutine for `util/org-collect-duplicate-headings'.
See `util/org-collect-duplicate-headings' for details.  `defun'
doesn't work when passing references to headings and dups for
some reason."
  `(if predicate
       (when (funcall predicate check)
         (push item headings)
         (push check strings)
         (when (> (cl-count check strings :test test) 1)
           (let* ((old-pos (cl-position check (reverse strings) :test test))
                  (old-heading (nth old-pos (reverse headings))))
             (unless (cl-member old-heading dups :test test)
               (push old-heading dups)))
           (push item dups)))
     (push item headings)
     (push check strings)
     (when (> (cl-count check strings :test test) 1)
       (let* ((old-pos (cl-position check (reverse strings) :test test))
              (old-heading (nth old-pos (reverse headings))))
         (unless (cl-member old-heading dups :test test)
           (push old-heading dups))
         (push item dups)))))

(defvar util/org-use-headings-cache t
  "Whether to use headings from `util/org-headings-cache'.
Cache is returned from `util/org-collected-headings' and is
auto updated if the file has changed on disk.  See
`util/org-collected-headings'.")

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
         (in-cache (when util/org-use-headings-cache
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
    (sort dups (lambda (x y) (string-greaterp (car y) (car x))))))

(defun util/org-collect-duplicate-headings-buffers (buffers &optional predicate ignore-case)
  "Collect duplicate headings from multiple org buffers.

BUFFERS is a list of org buffers on which to operate.

With optional boolean function PREDICATE, collect those which
only satisfy it.
With optional non-nil IGNORE-CASE, ignore case while searching.
Optional TEST is which test function to use for `cl-count'.
Defaults to `equal'."
  (let* ((test #'equal) ; (lambda (x y) (equal (butlast (split-string x)) (butlast (split-string (car y)))))
         (in-cache (when util/org-use-headings-cache
                     (a-keys (util/org-collected-headings
                              #'util/org-default-heading-filter-p))))
         headings dups strings)
    (seq-do
     (lambda (buf)
       (with-current-buffer buf
         (let ((bufname (buffer-name buf)))
           (if (member bufname in-cache)
               (let ((cache (a-get util/org-collect-headings-cache bufname)))
                 (dolist (el cache)
                   (let* ((heading (concat (car el) " " bufname))
                          (pos (-last-item el))
                          (item (list heading pos bufname))
                          (check (if ignore-case (downcase (car el))
                                   (car el))))
                     (util/org-collect-duplicate-subr-other item check headings dups strings
                                                            predicate ignore-case test))))
             (save-excursion
               (goto-char (point-max))
               (while (re-search-backward org-complex-heading-regexp nil t)
                 (let* ((el (org-element-at-point))
                        (heading (org-element-property :title el))
                        (pos (org-element-property :begin el))
                        (item `(heading . ,(list pos bufname)))
                          (check (if ignore-case (downcase (car el))
                                   (car el))))
                   (util/org-collect-duplicate-subr-other item check headings dups strings
                                                          predicate ignore-case test))))))
         ;; (setq offset (- (length dups) offset))
         ;; (setq copies (-concat (mapcar (lambda (x) `(,(concat (car x) " " (buffer-name buf)) . ,(list (cdr x) buf)))
         ;;                               (-slice dups 0 offset))
         ;;                       copies))
         ))
     buffers)
    (sort dups (lambda (x y) (string-greaterp (car y) (car x))))))

;; TODO: Perhaps can add a timer to warn user if copying link after long time so
;;       that on first copy `util/org-copy-link-append' should be nil. Or the
;;       timer could set `util/org-copy-link-append' automatically to nil.
(defvar util/org-copy-link-append nil)
(defun util/org-copy-link-to-heading (&optional pref-arg)
  "Copy link to current heading.

Prefer custom-id but default to fuzzy link.  Optional PREF-ARG
is for checking interactive usage."
  (interactive "p")
  (when (eq pref-arg 4)
    (setq util/org-copy-link-append nil))
  (let* ((case-fold-search t)
         (heading (org-get-heading t t t t))
         (props (org-entry-properties))
         (custom-id (a-get props "CUSTOM_ID"))
         (option (if custom-id (concat "#" custom-id) (concat "*" heading)))
         (filename (buffer-file-name))
         (link (format "[[%s::%s][%s]]" filename option heading)))
    (if pref-arg
        (if util/org-copy-link-append
            (progn (kill-append (concat "\n" link) nil)
                   (message "Appened link %s to last kill" heading))
          (kill-new link)
          (setq util/org-copy-link-append t)
          (message "Killed new link to %s" heading))
      link)))

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

(provide 'util/org)

;;; util-org.el ends here

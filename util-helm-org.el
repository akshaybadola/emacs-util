;;; util-helm-org.el --- `helm' Utilty functions for `org' buffers. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Thursday 14 October 2021 18:50:15 PM IST>
;; Keywords:	helm, org, utility

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
;; Some helm functions for org mode.
;;
;; The utility functions I've used begin with `util/' prefix and I've kept the
;; convention going here. You'll have to load them as "(require 'util/helm-org util-helm-org)"
;; for it to load correctly; notice the file name in the end.

;;; Code:

(require 'org)
(require 'helm)
(require 'util)
(require 'util/org "util-org")

(defvar util/helm-org-duplicate-headings nil
  "Global variable to easily access duplicate headings found in buffer.")

(defun util/helm-org-show (char)
  "Show the org entry including property block at point CHAR."
  (interactive)
  (let ((buf (or (helm-get-attr 'buffer) helm-current-buffer)))
    (with-current-buffer buf
      (switch-to-buffer buf)
      (goto-char char)
      (recenter-top-bottom)
      (org-show-entry)
      (save-excursion
        (let ((bounds (org-get-property-block)))
          (when bounds
            ;; HACK (car bounds) gives node-property which org complains is not a drawer
            (goto-char (- (car bounds) 1))
            (org-hide-drawer-toggle)))))))

(defun util/helm-org-show-with-path (pos)
  "Show the org entry including property block at POS.
POS is a list of '(point buffer)."
  (interactive)
  (let ((char (car pos))
        (buf (cadr pos)))
    (with-current-buffer buf
      (switch-to-buffer buf)
      (goto-char char)
      (recenter-top-bottom)
      (org-show-entry)
      (save-excursion
        (let ((bounds (org-get-property-block)))
          (when bounds
            ;; HACK (car bounds) gives node-property which org complains is not a drawer
            (goto-char (- (car bounds) 1))
            (org-hide-drawer-toggle)))))))

(defun util/helm-org-delete (&rest args)
  "Cut the subtree at point in current helm buffer."
  (interactive)
  (with-helm-current-buffer
    (if (> (util/org-count-subtree-children) 0)
        (message "Cannot delete node with children")
      (org-copy-subtree nil t))))

(defun util/helm-org-show-outline (&rest args)
  "Print the outline path for heading at point in helm"
  (interactive)
  (with-helm-current-buffer
    (message (org-format-outline-path (org-get-outline-path)))))

(defun util/helm-org-copy-persistent ()
  "Copy link to org heading keeping helm session."
  (interactive)
  (with-helm-alive-p
    (helm-set-attr 'copy-link-to-entry '(util/helm-org-copy-link-to-entry . never-split))
    (helm-execute-persistent-action 'copy-link-to-entry)))
(put 'util/helm-org-copy-persistent 'helm-only t)

(defun util/helm-org-show-outline-persistent ()
  "Show org heading keeping helm session."
  (interactive)
  (with-helm-alive-p
    (helm-set-attr 'copy-link-to-entry '(util/helm-org-copy-link-to-entry . never-split))
    (helm-execute-persistent-action 'copy-link-to-entry)))
(put 'util/helm-org-copy-persistent 'helm-only t)

;; TODO: If they're in the same subtree, just delete one of them
;; TODO: Subtract the number of characters from each POS after POINT from DUPS
;; TODO: Don't delete self.
(defun util/helm-org-replace-heading-as-list-item (&rest args)
  "Delete a duplicate org heading at point and insert a link to original.
Inserts a link to the first org heading in
`util/helm-org-duplicate-headings'.  `util/helm-org-duplicate-headings' is
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
                            util/helm-org-duplicate-headings)))
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
               ;;                                         util/helm-org-duplicate-headings)))
               ;;                  :follow 1
               ;;                  :action 'util/helm-org-show-dup-actions
               ;;                  :keymap util/helm-org-map)))
               ))))))

(defvar util/helm-org-kill-append nil)

(defun util/helm-org-copy-link-to-entry (&rest args)
  "Copy the link to current entry in helm buffer.
This doesn't store the link but copies it to the kill ring.  In
case multiple links are copied in the same session they are
appended to the kill ring separated with a newline."
  (interactive "P")
  (with-helm-current-buffer
    (let ((link (util/org-copy-link-to-heading)))
      (if util/helm-org-kill-append
          (kill-append (concat "\n" link) nil)
        (kill-new link)
        (setq util/helm-org-kill-append t)))))

(defvar util/helm-org-show-dup-actions
  (helm-make-actions
   "Show Entry" 'util/helm-org-show
   "Delete Entry" 'util/helm-org-delete
   "Show Entry Path" 'util/helm-org-show-outline
   "Replace with link" 'util/helm-org-replace-heading-as-list-item)
  "Helm actions for `util/helm-org-show-duplicate-headings'.")

(defvar util/helm-org-show-dup-map
  (let ((new-map (copy-keymap helm-map)))
    (define-key new-map (kbd "C-k") 'util/helm-org-delete)
    (define-key new-map (kbd "C-l") 'util/helm-org-show-outline)
    (define-key new-map (kbd "C-R") 'util/helm-org-replace-heading-as-list-item)
    new-map)
  "Keymap for `helm-org-rifle'.")

(defun util/helm-org-show-duplicate-headings (&optional pred ignore-case)
  "Show duplicate headings in an org buffer with `helm'.
With optional predicate PRED, show only those which satisfy the
predicate.  With optional non-nil IGNORE-CASE, ignore case while
searching."
  (interactive)
  (util/with-org-mode
   (message "Gathering headings")
   (helm :sources (helm-build-sync-source "Duplicate headings"
                    :candidates (lambda ()
                                  (with-helm-current-buffer
                                    (util/org-collect-duplicate-headings pred ignore-case)))
                    :follow 1
                    :action 'util/helm-org-show-dup-actions
                    :keymap util/helm-org-show-dup-map))))

(defvar util/helm-org-show-dup-files-actions
  (helm-make-actions
   "Show Entry" 'util/helm-org-show-with-path
   "Delete Entry" 'util/helm-org-delete
   "Show Entry Path" 'util/helm-org-show-outline
   "Replace with link" 'util/helm-org-replace-heading-as-list-item)
  "Helm actions for `util/helm-org-show-duplicate-headings-files'.")

(defun util/helm-org-show-duplicate-headings-files (&optional pred ignore-case)
  "Show duplicate headings in an org buffer with `helm'.
With optional predicate PRED, show only those which satisfy the
predicate.  With optional non-nil IGNORE-CASE, ignore case while
searching."
  (interactive)
  (message "Gathering headings")
  (let ((buffers (mapcar #'find-buffer-visiting util/org-collect-headings-files)))
    (helm :sources (helm-build-sync-source "Duplicate headings"
                     :candidates (lambda ()
                                   (util/org-collect-duplicate-headings-buffers
                                    buffers pred ignore-case))
                     :follow 1
                     :action 'util/helm-org-show-dup-files-actions
                     :keymap util/helm-org-show-dup-map))))

(defun util/helm-org-show-duplicate-customids (&optional pred)
  "Show org headings with duplicate custom ids.
Optional PRED is used to filter the headings."
  (interactive)
  (util/with-org-mode
   (message "Gathering headings")
   (helm :sources (helm-build-sync-source "Duplicate Custom IDs"
                    :candidates (lambda ()
                                  (with-helm-current-buffer
                                    (util/org-collect-duplicate-customids pred)))
                    :follow 1
                    :action 'util/helm-org-show-dup-actions
                    :keymap util/helm-org-show-dup-map))))

;; FIXME: There's redundancy in collection functions
(defun util/helm-org-headings-subr (&optional pred)
  "Return all headings with position satisfying PRED.

Primarily a subroutine for `util/org-helm-headings'.  Optional
PRED defaults to `identity'."
  (let ((in-cache (member (buffer-name)
                          (a-keys (util/org-collected-headings
                                   #'util/org-default-heading-filter-p
                                   (buffer-name)))))
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

(defvar util/helm-org-headings-map
  (let ((new-map (copy-keymap helm-map)))
    (define-key new-map (kbd "C-w") 'util/helm-org-copy-link-to-entry)
    new-map)
  "Keymap for `util/org-helm-headings'.")

(defun util/helm-org-headings (&optional pred)
  "Navigate through headings in an org buffer with `helm'.
With optional predicate PRED, show only those headings which satisfy the
predicate."
  (interactive)
  (util/with-org-mode
   (setq util/helm-org-kill-append nil)
   (helm :sources (helm-build-sync-source "Org Headings"
                    :candidates (lambda ()
                                  (with-helm-current-buffer
                                    (util/helm-org-headings-subr pred)))
                    :follow 1
                    :action (helm-make-actions
                             "Show Entry" 'util/org-helm-show
                             "Copy Link to Entry" 'util/helm-org-copy-persistent)
                    :keymap util/helm-org-headings-map))))

(defun util/helm-org-get-source-for-buf (buf &optional pred)
  "Subroutine to get helm sources from an org buffer BUF.
Optional PRED is used to filter the headings."
  (let ((source (helm-build-sync-source (buffer-name buf)
                  :candidates (lambda ()
                                (with-current-buffer buf
                                  (util/helm-org-headings-subr pred)))
                  :follow 1
                  :action (helm-make-actions
                           "Show Entry" 'util/org-helm-show
                           "Copy Link to Entry" 'util/helm-org-copy-persistent)
                  :keymap util/helm-org-headings-map)))
    (helm-set-attr 'buffer buf source)
    source))

(defun util/helm-org-sources-agenda-files ()
  "Subroutine to gather helm sources from agenda files."
  (let* ((bufs (mapcar #'find-buffer-visiting (org-agenda-files)))
         (sources (mapcar #'util/helm-org-get-source-for-buf bufs)))
    sources))

(defun util/helm-org-headings-agenda-files (&optional pred)
  "Navigate through headings in an org buffer with `helm'.
With optional predicate PRED, show only those headings which satisfy the
predicate."
  (interactive)
  (setq util/helm-org-kill-append nil)
  (helm :sources (util/helm-org-sources-agenda-files)))

(provide 'util/helm-org)

;;; util-helm-org.el ends here

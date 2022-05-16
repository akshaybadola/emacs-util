;;; util-all.el --- Meta pacakge for loading all `util' functions. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020,2021,2022
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Wednesday 23 February 2022 20:42:07 PM IST>
;; Keywords:	utility, convenience, emacs-lisp, org, helm
;; Version:     0.4.0

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

;; Loads all symbols from all util files

;;; Code:

(require 'util/core "util-core.el")
(require 'util/ffip "util-ffip.el")
(require 'util/helm-org "util-helm-org.el")
(require 'util/misc "util-misc.el")
(require 'util/org "util-org.el")

(provide 'util/all)

;;; util-all.el ends here

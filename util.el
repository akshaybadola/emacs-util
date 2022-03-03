;;; util.el --- Various utility functions. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020,2021
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Thursday 03 March 2022 09:16:42 AM IST>
;; Keywords:	utility, convenience, emacs-lisp, org, helm
;; Version:     0.4.2

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
;;
;; They've been split into multiple files and should be loaded as such. This
;; file only imports core functionality from `util/core'.

;;; Code:

(require 'util/core "util-core.el")

(provide 'util)

;;; util.el ends here


;; litprog.el --- General Literate Programing Mode based on multi-mode.el 
;;
;; Copyright (C) 2012 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.
;;
;; Filename: ess-tracebug.el
;; Author: Vitalie Spinu
;; Maintainer: ESS-core
;; Created: Jul 07 2012
;; Keywords: tools, languages
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.
;;

;;; Commentary:
;;  Based on noweb.el by Dave Love.


(load (expand-file-name  "contrib/multi-mode.el"  ess-lisp-directory))
;; (require 'multi-mode)

(eval-when-compile
  (require 'cl)
  (require 'easy-mmode))

(defgroup LitProg nil
  "Editing literate programing documents"
  :link '(emacs-commentary-link "LitProg")
  :group 'ESS)


(provide 'litprog)
;;; litprog.el ends here

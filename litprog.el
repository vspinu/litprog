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


(defvar litprog-doc-mode  nil
  "Major mode for documentation chunks.")
(make-variable-buffer-local 'litprog-doc-mode)

(defvar litprog-code-mode nil
  "Default major mode for code chunks.
There can be several code modes.
")
(make-variable-buffer-local 'litprog-code-mode)



(defcustom litprog-select-doc-mode-hook nil
  "Hook run after the base mode is selected."
  :group 'LitProg
  :type 'hook)

(defcustom litprog-select-code-mode-hook nil
  "Hook run after the code mode is selected."
  :group 'LitProg
  :type 'hook)

(defcustom litprog-select-header-mode-hook nil
  "Hook run after the header is selected."
  :group 'LitProg
  :type 'hook)

(defvar litprog-chunk-start-pattern nil
  "Regexp matching the start of a chunk header.")
(make-variable-buffer-local 'litprog-chunk-header-pattern)

(defvar litprog-chunk-end-pattern nil
  "Regexp matching the start of a chunk header.")
(make-variable-buffer-local 'litprog-chunk-header-pattern)


(defvar litprog-font-lock-matcher nil
  "Matcher used in code buffers to set chunk header fontification.
If nil `litprog-chunk-start-pattern'\\|`litprog-chunk-end-pattern' is used.
See `font-lock-keywords'.
")

(defvar litprog-font-lock-syntactic-matcher nil
  "Matcher used in code buffers to set chunk header to comment syntax.
If nil `litprog-chunk-start-pattern'\\|`litprog-chunk-end-pattern' is used.
See `font-lock-syntactic-keywords'.
")


(defvar litprog-font-lock-literal-syntactic-matcher nil
  "Matcher used in doc buffers to set in-line literals to string syntax.
")

(defvar litprog-font-lock-keywords nil
  "A list of font lock keywords for headers")
(make-variable-buffer-local 'litprog-font-lock-keywords)


(defcustom litprog-prefix-key "\M-n"
  "Prefix key for the litprog mode keymap.
Not effective after loading the LitProg library."
  :group 'LitProg
  :type '(choice string vector))


(defvar litprog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map litprog-prefix-key
      (let ((map (make-sparse-keymap)))
	(define-key map "\C-n" 'litprog-goto-next)
	(define-key map "\C-p" 'litprog-goto-prev)
	;; (define-key map "\M-n" 'litprog-goto-next)
	;; (define-key map "\M-p" 'litprog-goto-prev)
	;; (define-key map "c" 'litprog-code-next)
	;; (define-key map "C" 'litprog-code-prev)
	;; (define-key map "d" 'litprog-doc-next)
	;; (define-key map "D" 'litprog-doc-prev)
        ;; Use imenu.
        ;; 	(define-key map "\C-g" 'litprog-goto-chunk)

        (define-key map "\M-k" 'litprog-kill-chunk)
        (define-key map "\M-K" 'litprog-kill-chunk-pair)
        (define-key map "\M-m" 'litprog-mark-chunk)
        (define-key map "\M-M" 'litprog-mark-chunk-pair)
        (define-key map "\M-n" 'litprog-narrow-to-chunk)
        (define-key map "\M-N" 'litprog-narrow-to-chunk-pair)
        (define-key map "\C-t" 'litprog-toggle-narrowing)
	(define-key map "\M-i" 'litprog-new-chunk)

	(if (bound-and-true-p litprog-electric-<)
	    (define-key litprog-mode-map "<" #'litprog-electric-<))
	(if (bound-and-true-p litprog-electric-@)
	    (define-key litprog-mode-map "@" #'litprog-electric-@))
	map))
    (define-key map [menu-bar LitProG]
      (cons "LitProG"
	    (let ((map (make-sparse-keymap "LitProG")))
	      (define-key-after map [goto-prev]
		'(menu-item "Previous matching chunk" litprog-goto-prev))
	      (define-key-after map [mark]
		'(menu-item "Mark chunk" litprog-mark-chunk))
	      (define-key-after map [kill]
		'(menu-item "Kill chunk" litprog-kill-chunk))
	      (define-key-after map [new]
		'(menu-item "New chunk" litprog-new-chunk))
	      map)))
    map))


(defvar litprog-buffer nil
  "Internal use.")
(defvar litprog-doc-buffer nil
  "Internal use.")
(defvar litprog-code-buffer nil
  "Internal use.")

(defun litprog-set-local-variables ()
  (set (make-local-variable 'litprog-code-buffer) litprog-code-buffer)
  (set (make-local-variable 'litprog-doc-buffer) litprog-doc-buffer)
  (set (make-local-variable 'litprog-buffer) litprog-buffer)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (unless (eq litprog-buffer (current-buffer))
    (litprog-minor-mode 1)
    (font-lock-set-defaults))
  ;; (add-hook (make-local-variable 'multi-select-mode-hook)
  ;;           'litprog-select-litprog-mode-hook)
  )


;; Used to propagate the bindings to the indirect buffers.
(define-minor-mode litprog-minor-mode
  "LitProg minor mode, used in code and doc chunks."
  nil " LP" litprog-mode-map)


(defun litprog-check-essential-vars ()
  "Error if any of these are not set"
  (dolist (var '(litprog-doc-mode litprog-chunk-start-pattern litprog-chunk-end-pattern))
    (dbg var)
    (unless (symbol-value var)
      (error (symbol-name var) " is not set"))))

(defcustom litprog-default-code-mode-alist  '((R-mode "Rnw" "Rtex" "Tnw"))
  "Mode associations with file extensions.
Each element is of the form '(mode \"ext1\" \"ext2\" ...)."
  :type 'alist
  :group 'LitProg
  )



(defun litprog-guess-code-mode-from-name (name)
    (car
     (rassoc* (upcase (file-name-extension name)) litprog-default-code-mode-alist
              :test (lambda (el L)
                      (member* el L :test 'equal :key 'upcase)))))

(defun litprog-select-mode (pos)
  "Mode-selecting function for use in `multi-mode-alist'."
  (error "litprog-select-mode is not defined yet")
)

(defvar litprog-select-mode-function 'litprog-select-mode
  "")
(make-variable-buffer-local litprog-select-mode-function)

(defvar litprog-settings nil
  "An nested plist of mode and backend configuration
  options.

It is a nested plist of backends, which are plists of plists
representing options per mode. There is a special backend
'defaults' which hold default options for each mode.

The names of modes are short names, that is, they are not
prefixed by lp- or postfixed by -mode. lp-foo-mode is simply
registered as foo.

Use `litprog-customize' function to set mode specific, or backend
specific options.

Each configuration plist can contain a special key
'litprog-inherit' which is a list of modes or backends this
options inherits from. All inherited options are automatically
installed during initialization of the mode or backend. To
specify a mode use just it's name. To specify a backend use a
cons (backend . mode).

All modes and backends implicitly inherit from the defaults of
litprog mode. Thus, before a backend is installed all the
settings are reverted to the default state.

")

;; (defun litprog-short-mname (symb)
;;   "Shorten mode name"
;;   (if (symbolp symb)
;;       (let ((name (symbol-name symb)))
;;         (setq name (replace-regexp-in-string "lp-\\|-mode" "" name))
;;         (intern name))
;;     (error "Elements of what souled be unquoted symbols")))

(defmacro litprog-customize (what &rest body)
  "Customize litprog modes and backends.
WHAT can be a name of a mode or a pair (backend mode-name).

Customization pairs SYM and VAL are just as in `setq'.

\(fn what [SYM VAL] ...)"

  (declare (indent defun))
  (unless (listp what)
    (setq what `(defaults ,what)))
  `(let* ((backend ',(car what))
          (mode  ',(cadr what))
          (BE (plist-get litprog-settings backend))
          (M (plist-get BE mode)))
     (loop for x on ',body by 'cddr
           do (setq M (plist-put M (car x) (eval (cadr x)))))
     (setq litprog-settings
           (plist-put litprog-settings backend (plist-put BE mode M)))))

(defmacro litprog-setq (what)
  "Install customizations of modes and backends.
WHAT can be a name of a mode or a pair (backend mode-name).

If the plist associated with the mode contains litprog-inherit
name than `litprog-setq' is first called on those. This
implements a basic inheritance mechanism of configuration
options. Note that there is no explicit check for recursion.
"
  (declare (indent defun))
  (unless (listp what)
    (setq what `(defaults ,what)))
  `(let* ((backend (plist-get litprog-settings ',(car what)))
          (mcust   (plist-get backend ',(cadr what))))
     ;; set all the inherited modes
     (mapcar 'litprog-setq (plist-get mcust 'litprog-inherit))
     (dbg mcust)
     (loop for x on mcust by 'cddr
           do (set (car x)  (cadr x)))))
  
  
;; (litprog-customize (test lp-my-mode)
;;   fff (+ 45 444)
;;   ppp nil nnn
;;   )


(litprog-customize litprog-mode
  litprog-doc-mode                      888
  litprog-code-mode                     nil
  litprog-select-mode-function          'litprog-select-modennn
  litprog-chunk-start-pattern           nil
  litprog-chunk-end-pattern             nil
  litprog-font-lock-keywords            nil
  litprog-font-lock-matcher             nil
  litprog-font-lock-syntactic-matcher   nil
  litprog-font-lock-literal-syntactic-matcher  nil
  )

;; litprog-settings

(defun linprog-install-modes ()
  ;; Extract values of local variables now, so we know the doc and
  ;; code modes.  Nullify litprog-mode2 while we process possible
  ;; `mode: noweb' line to avoid infinite regress.
  (flet ((litprog-mode ()))
    (hack-local-variables)
    (litprog-check-essential-vars)
    (setq litprog-code-mode
            (or litprog-code-mode
                (litprog-guess-code-mode-from-name (buffer-file-name))
                'fundamental-mode))
    (let ((multi-mode-alist
           (list (cons 'litprog-mode            litprog-select-mode-function)
                 (cons litprog-doc-mode         nil)
                 (cons litprog-code-mode        nil))))
      (multi-mode-install-modes)))
  (let ((litprog-doc-buffer (cdr (assq litprog-doc-mode
                                       multi-indirect-buffers-alist)))
        (litprog-code-buffer (cdr (assq litprog-code-mode
                                       multi-indirect-buffers-alist)))
        (litprog-buffer (cdr (assq 'litprog-mode
                                   multi-indirect-buffers-alist)))
        (chunk-pattern (concat litprog-chunk-start-pattern "\\|"
                               litprog-chunk-end-pattern)))
    (with-current-buffer litprog-code-buffer
      (litprog-set-local-variables)
      ;; Add font-lock stuff for chunk uses in code.  Add syntactic
      ;; keywords to treat them as comments with
      ;; `parse-sexp-lookup-properties' on (see
      ;; `litprog-set-local-variables') to try to avoid disturbing the
      ;; mode's idea of syntax.  (This used to be strings instead of
      ;; comments, but that doesn't work well with C, for instance.)
      ;; Then fontify the uses like their definitions, using
      ;; overriding patterns.
      (set (make-local-variable 'font-lock-syntactic-keywords)
           (append (font-lock-eval-keywords font-lock-syntactic-keywords)
                   (list (or litprog-font-lock-syntactic-matcher
                             (cons chunk-pattern '(0 "!"))))))
      (set (make-local-variable 'font-lock-keywords)
           (append (font-lock-eval-keywords font-lock-keywords)
                   (list (or litprog-font-lock-matcher
                             (cons chunk-pattern '(0 'font-lock-keyword-face t)))))))
    (with-current-buffer litprog-doc-buffer
      (litprog-set-local-variables)
      (when litprog-font-lock-literal-syntactic-matcher
        (set (make-local-variable 'font-lock-syntactic-keywords)
             (append (font-lock-eval-keywords font-lock-syntactic-keywords)
                     (list litprog-font-lock-literal-syntactic-matcher)))))
    ;; in base buffer 
    (litprog-set-local-variables)
    ;; Use Imenu to navigate chunks.
    ;; (set (make-local-variable 'imenu-generic-expression)
    ;;      litprog-imenu-generic-expression)
    ;; (imenu-add-menubar-index)
    (when litprog-font-lock-keywords 
      (set (make-local-variable 'font-lock-defaults)
           '(litprog-font-lock-keywords nil nil nil nil))
      ;; Fixme:  Why is this is necessary in Emacs 22+ to get
      ;; font-lock-keywords defined?
      (font-lock-set-defaults))
    ;; Single level of outline.
    (set (make-local-variable 'outline-regexp) chunk-pattern)
    (set (make-local-variable 'outline-level) (lambda () 1))
    ))

(define-derived-mode litprog-mode fundamental-mode "LitProG"
  "Mode for editing LitProg documents.
Supports differnt major modes for doc and code chunks using multi-mode.

Note: This is the major mode of the base buffer. 
"
  (litprog-setq litprog-mode)
)


(provide 'litprog)
;;; litprog.el ends here

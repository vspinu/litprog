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


(defvar litprog-fl-matcher nil
  "Matcher used in code buffers to set chunk header fontification.
If nil `litprog-chunk-start-pattern'\\|`litprog-chunk-end-pattern' is used.
See `font-lock-keywords'.
")

(defvar litprog-fl-syntactic-matcher nil
  "Matcher used in code buffers to set chunk header to comment syntax.
If nil `litprog-chunk-start-pattern'\\|`litprog-chunk-end-pattern' is used.
See `font-lock-syntactic-keywords'.
")


(defvar litprog-fl-literal-syntactic-matcher nil
  "Matcher used in doc buffers to set in-line literals to string syntax.
")

(defvar litprog-fl-keywords nil
  "A list of font lock keywords for headers")
(make-variable-buffer-local 'litprog-fl-keywords)


(defcustom litprog-prefix-key "\M-n"
  "Prefix key for the litprog mode keymap.
Not effective after loading the LitProg library."
  :group 'LitProg
  :type '(choice string vector))

(defvar litprog-buffer nil
  "Internal use.")
(defvar litprog-doc-buffer nil
  "Internal use.")
(defvar litprog-code-buffer nil
  "Internal use.")





(defun litprog-check-essential-vars ()
  "Error if any of these are not set"
  (dolist (var '(litprog-doc-mode litprog-chunk-start-pattern litprog-chunk-end-pattern))
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


;;; NARROWING
(defvar litprog--narrowing nil
  "Whether paired narrowing is in place.")

(defun litprog-narrow-to-chunk-pair ()
  "todo:"
  )


;;; NAVIGATION

(defun litprog-next-header (&optional n)
  "Goto to the Nth code chunk from point."
  (interactive "p")
  (when litprog-narrowing
    (whiden))
  (unless (re-search-forward  litprog-chunk-start-pattern nil t n)
    (message "No more chunk headers found."))
  (if litprog-narrowing
      (litprog-narrow-to-chunk-pair)))

(defun litprog-previous-header (&optional n)
  (interactive "p")
  (litprog-next-header (- n))
  )



;;; CONFIGURATION

(defvar litprog-options nil
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
          (BE (plist-get litprog-options backend))
          (M (plist-get BE mode)))
     (loop for x on ',body by 'cddr
           do (setq M (plist-put M (car x) (eval (cadr x)))))
     (setq litprog-options
           (plist-put litprog-options backend (plist-put BE mode M)))))

(defmacro litprog-set-options (what)
  "Install customizations of modes and backends.
WHAT can be a name of a mode or a pair (backend mode-name).

If the plist associated with the mode contains litprog-inherit
name than `litprog-set-options' is first called on those. This
implements a basic inheritance mechanism of configuration
options. Note that there is no explicit check for recursion.
"
  (declare (indent defun))
  (unless (listp what)
    (setq what `(defaults ,what)))
  `(let* ((backend (plist-get litprog-options ',(car what)))
          (mcust   (plist-get backend ',(cadr what))))
     ;; set all the inherited modes
     (mapcar 'litprog-set-options (plist-get mcust 'litprog-inherit))
     (loop for x on mcust by 'cddr
           do (set (car x)  (cadr x)))))
  
  
;; (litprog-customize (test lp-my-mode)
;;   fff (+ 45 444)
;;   ppp nil nnn
;;   )


(litprog-customize litprog-mode
  litprog-doc-mode                      nil
  litprog-code-mode                     nil
  litprog-get-mode-at-point-function         'litprog-get-mode-at-point
  litprog-chunk-start-pattern           nil
  litprog-chunk-end-pattern             nil
  litprog-fl-keywords            nil
  litprog-fl-matcher             nil
  litprog-fl-syntactic-matcher   nil
  litprog-fl-literal-syntactic-matcher  nil
  )


;;; MAIN
(defun litprog-get-mode-at-point-default (pos)
  "Mode-selecting function for use in `multi-mode-alist'."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pos)
      (let* ((reg (concat "\\(?1:\\(" litprog-chunk-end-pattern "\\)\\)\\|\\(?2:\\("
                          litprog-chunk-start-pattern "\\)\\)"))
             (found (re-search-backward reg nil t))
             (start-doc (or (null found) (match-end 1)))
             (start (goto-char (if found (match-end 0) (point-min))))
             (end0 (if (re-search-forward reg nil t)
                       (match-beginning 0)))
             (end1 (and end0 (match-end 0)))
             )
        (if (or (null end0) (< pos end0))
            (if start-doc
                (multi-make-list litprog-doc-mode start (or end0 (point-max)))
              (multi-make-list litprog-code-mode start (or end0 (point-max))))
          (multi-make-list 'litprog-mode end0 end1))))))


(defun litprog-set-local-variables ()
  (setq litprog-doc-mode doc-mode)
  (setq litprog-code-mode code-mode)
  (set (make-local-variable 'litprog-code-buffer) code-buffer)
  (set (make-local-variable 'litprog-doc-buffer) doc-buffer)
  (set (make-local-variable 'litprog-buffer) base-buffer)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (unless (eq litprog-buffer (current-buffer))
    (litprog-minor-mode 1)
    (font-lock-set-defaults))
  ;; (add-hook (make-local-variable 'multi-select-mode-hook)
  ;;           'litprog-select-litprog-mode-hook)
  )

(defun litprog-install-modes ()
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
           (list (cons 'litprog-mode            litprog-get-mode-at-point-function)
                 (cons litprog-doc-mode         nil)
                 (cons litprog-code-mode        nil))))
      (multi-mode-install-modes)))
  (let ((doc-buffer (cdr (assq litprog-doc-mode
                                       multi-indirect-buffers-alist)))
        (code-buffer (cdr (assq litprog-code-mode
                                       multi-indirect-buffers-alist)))
        (base-buffer (cdr (assq 'litprog-mode
                                   multi-indirect-buffers-alist)))
        (chunk-pattern (concat litprog-chunk-start-pattern "\\|"
                               litprog-chunk-end-pattern))
        (doc-mode litprog-doc-mode)
        (code-mode litprog-code-mode))
    (with-current-buffer code-buffer
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
                   (list (or litprog-fl-syntactic-matcher
                             (cons chunk-pattern '(0 "!"))))))
      (set (make-local-variable 'font-lock-keywords)
           (append (font-lock-eval-keywords font-lock-keywords)
                   (list (or litprog-fl-matcher
                             (cons chunk-pattern '(0 'font-lock-keyword-face t)))))))
    (with-current-buffer doc-buffer
      (litprog-set-local-variables)
      (when litprog-fl-literal-syntactic-matcher
        (set (make-local-variable 'font-lock-syntactic-keywords)
             (append (font-lock-eval-keywords font-lock-syntactic-keywords)
                     (list litprog-fl-literal-syntactic-matcher)))))
    ;; in base buffer 
    (litprog-set-local-variables)
    ;; Use Imenu to navigate chunks.
    ;; (set (make-local-variable 'imenu-generic-expression)
    ;;      litprog-imenu-generic-expression)
    ;; (imenu-add-menubar-index)
    (when litprog-fl-keywords 
      (set (make-local-variable 'font-lock-defaults)
           '(litprog-fl-keywords nil nil nil nil))
      ;; Fixme:  Why is this is necessary in Emacs 22+ to get
      ;; font-lock-keywords defined?
      (font-lock-set-defaults))
    ;; Single level of outline.
    (set (make-local-variable 'outline-regexp) chunk-pattern)
    (set (make-local-variable 'outline-level) (lambda () 1))
    ))



(defvar litprog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map litprog-prefix-key
      (let ((map (make-sparse-keymap)))
	(define-key map "\C-n" 'litprog-next-header)
	(define-key map "\C-p" 'litprog-previous-header)
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

	;; (if (bound-and-true-p litprog-electric-<)
	;;     (define-key litprog-mode-map "<" #'litprog-electric-<))
	;; (if (bound-and-true-p litprog-electric-@)
	;;     (define-key litprog-mode-map "@" #'litprog-electric-@))
	map))
    (define-key map [menu-bar LitProG]
      (cons "LitProG"
	    (let ((map (make-sparse-keymap "LitProG")))
              (define-key-after map [goto-prev]
		'(menu-item "Next chunk header" litprog-next-header))
	      (define-key-after map [goto-prev]
		'(menu-item "Previous chunk header" litprog-previous-header))
	      (define-key-after map [mark]
		'(menu-item "Mark chunk" litprog-mark-chunk))
	      (define-key-after map [kill]
		'(menu-item "Kill chunk" litprog-kill-chunk))
	      (define-key-after map [new]
		'(menu-item "New chunk" litprog-new-chunk))
	      map)))
    map)
  "A keymap for LitProG mode.")


(define-derived-mode litprog-mode fundamental-mode "LitProG"
  "Mode for editing LitProg documents.
Supports differnt major modes for doc and code chunks using multi-mode.

Note: This is the major mode of the base buffer. 
"
  (litprog-set-options litprog-mode)
)

;; Used to propagate the bindings to the indirect buffers.
(define-minor-mode litprog-minor-mode
  "LitProg minor mode, used in code and doc chunks."
  nil " LP" litprog-mode-map)

(provide 'litprog)
;;; litprog.el ends here

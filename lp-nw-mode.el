


(defcustom lp-nw-select-doc-mode-hook nil
  "Hook run after the base mode is selected."
  :group 'LitProg
  :type 'hook)

(defcustom lp-nw-select-code-mode-hook nil
  "Hook run after the code mode is selected."
  :group 'LitProg
  :type 'hook)

(defcustom lp-nw-select-header-mode-hook nil
  "Hook run after the header is selected."
  :group 'LitProg
  :type 'hook)


(defvar litprog-defaults:nw
  '((litprog-doc-mode                   . 'latex-mode)
    (litprog-select-mode-function       . 'litprog-select-mode:nw)
    (litprog-chunk-start-pattern        . "^<<\\(.*\\)>>=")
    (litprog-chunk-end-pattern          . "^\\(@ +%def .*\\)$\\|\\(@[ \n]\\)")
    (litprog-font-lock-keywords         . `(("^\\(<<\\)\\(.*\\)\\(>>=\\)$" (1 'font-lock-keyword-face)
                                             (2 'font-lock-variable-name-face) (3 'font-lock-keyword-face))
                                            ("^\\(@ +%def\\) +\\(.+\\)"
                                             (1 'font-lock-keyword-face)
                                             (2 'font-lock-variable-name-face))
                                            "^@"))
    (litprog-font-lock-matcher          .  '("\\(?:$\\|[^@]\\|\\`\\)\\(<<\\)\\([^>]+\\)\\(>>\\)"
                                             (1 'font-lock-keyword-face t)
                                             (2 'font-lock-variable-name-face t)
                                             (3 'font-lock-keyword-face t)))
    (litprog-font-lock-syntactic-matcher . '("\\(?:$\\|[^@]\\)\\(<\\)<[^>]+>\\(>\\)" (1 "!") (2 "!")))
    (litprog-font-lock-literal-syntactic-matcher . '("\\(\\[\\)\\[[^]]+]\\(]\\)" (1 "|") (2 "|")))
    ))

(define-derived-mode lp-nw-mode litprog-mode "LitProg-NW"
  "Noweb mode"

  (setq
   litprog-doc-mode             'latex-mode
   litprog-select-mode-function 'litprog-select-mode:nw
   litprog-chunk-start-pattern  "^<<\\(.*\\)>>="
   litprog-chunk-end-pattern    "^\\(@ +%def .*\\)$\\|\\(@[ \n]\\)"
   litprog-font-lock-keywords   `(("^\\(<<\\)\\(.*\\)\\(>>=\\)$" (1 'font-lock-keyword-face)
                                   (2 'font-lock-variable-name-face) (3 'font-lock-keyword-face))
                                  ("^\\(@ +%def\\) +\\(.+\\)"
                                   (1 'font-lock-keyword-face)
                                   (2 'font-lock-variable-name-face))
                                  "^@")
   litprog-font-lock-matcher    '("\\(?:$\\|[^@]\\|\\`\\)\\(<<\\)\\([^>]+\\)\\(>>\\)"
                                  (1 'font-lock-keyword-face t)
                                  (2 'font-lock-variable-name-face t)
                                  (3 'font-lock-keyword-face t))
   litprog-font-lock-syntactic-matcher  '("\\(?:$\\|[^@]\\)\\(<\\)<[^>]+>\\(>\\)" (1 "!") (2 "!"))
   litprog-font-lock-literal-syntactic-matcher  '("\\(\\[\\)\\[[^]]+]\\(]\\)" (1 "|") (2 "|"))
   )
  (linprog-install-modes)
  )


(defun litprog-select-mode:nw (pos)
  "Mode-selecting function for use in `multi-mode-alist'."
  (save-excursion
    (save-restriction
      (goto-char pos)
      (beginning-of-line)
      (cond
       ((looking-at noweb-chunk-header-pattern) ; on a header line
	;; A %def line or a code header line means neither doc or code.
	(cond
	 ((match-beginning 2)
	  (multi-make-list 'litprog-mode (match-beginning 0) (match-end 0)))
	 ((match-beginning 1)
	  (let (start)
	    (while (looking-at "@ +%def")
	      (setq start (match-beginning 0))
	      (forward-line -1))
	    (forward-line)
	    (while (looking-at "@ +%def")
	      (forward-line))
	    (multi-make-list 'litprog-mode start (line-end-position 0))))
	 (t 
	  ;; Else start of a doc chunk.  Mode is doc unless we're at bol.
	  (if (< pos (match-end 0))
	      (multi-make-list 'litprog-mode (match-beginning 0)
			       (1- (match-end 0)))
	    (goto-char (match-end 0))
	    (multi-make-list
	     litprog-doc-mode (point)
	     (if (re-search-forward noweb-chunk-header-pattern nil t)
		 (1- (match-beginning 0))
	       (point-max)))))))
       ((re-search-backward noweb-chunk-header-pattern nil t)
	(let ((start (if (match-beginning 3)
			 (match-end 0)
		       (1+ (match-end 0))))
	      (mode (if (match-beginning 2)
			litprog-code-mode
		      litprog-doc-mode))
	      (end (progn
		     (goto-char pos)
		     (if (re-search-forward noweb-chunk-header-pattern
					    nil t)
			 (1- (match-beginning 0))
		       (point-max)))))
	  (multi-make-list mode start end)))
       (t
	(multi-make-list
	 litprog-doc-mode (point-min)
	 (progn
	   (goto-char pos)
	   (if (re-search-forward noweb-chunk-header-pattern nil t)
	       (1- (match-beginning 0))
	     (point-max)))))))))

(add-to-list 'auto-mode-alist '("\\.Tnw$" . lp-nw-mode))

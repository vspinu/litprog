

;;; noweb


(defcustom lp-noweb-select-doc-mode-hook nil
  "Hook run after the base mode is selected."
  :group 'LitProg
  :type 'hook)

(defcustom lp-noweb-select-code-mode-hook nil
  "Hook run after the code mode is selected."
  :group 'LitProg
  :type 'hook)

(defcustom lp-noweb-select-header-mode-hook nil
  "Hook run after the header is selected."
  :group 'LitProg
  :type 'hook)



(litprog-customize lp-noweb-mode
  lp-doc-mode                    'latex-mode
  ;; lp-select-mode-function       . 'lp-select-mode:nw
  lp-chunk-start-pattern         "^<<\\(.*\\)>>="
  lp-chunk-end-pattern           "^\\(@ +%def .*\\)$\\|\\(@[ \n]\\)"
  lp-font-lock-keywords         `(("^\\(<<\\)\\(.*\\)\\(>>=\\)$" (1 'font-lock-keyword-face)
					(2 'font-lock-variable-name-face) (3 'font-lock-keyword-face))
				       ("^\\(@ +%def\\) +\\(.+\\)"
					(1 'font-lock-keyword-face)
					(2 'font-lock-variable-name-face))
				       "^@")
  lp-font-lock-matcher          '("\\(?:$\\|[^@]\\|\\`\\)\\(<<\\)\\([^>]+\\)\\(>>\\)"
				       (1 'font-lock-keyword-face t)
				       (2 'font-lock-variable-name-face t)
				       (3 'font-lock-keyword-face t))
  lp-font-lock-syntactic-matcher  '("\\(?:$\\|[^@]\\)\\(<\\)<[^>]+>\\(>\\)" (1 "!") (2 "!"))
  lp-font-lock-literal-syntactic-matcher '("\\(\\[\\)\\[[^]]+]\\(]\\)" (1 "|") (2 "|"))
  )

(define-derived-mode lp-noweb-mode litprog-mode "LP-Noweb"
  "Litprog noweb mode"
  (litprog-set-options noweb)
  (litprog-install-modes)
  )

;; for tests only
(add-to-list 'auto-mode-alist '("\\.Tnw$" . lp-noweb-mode))

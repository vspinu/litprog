(defun litprog-process-sentinel (process event)
  (let ((msg (format "%s %s." (process-name process) (substring event 0 -1)))
        (successp (string-match "^finished" event))
        (key (with-current-buffer magit-process-client-buffer
               (key-description (car (where-is-internal
                                      'magit-display-process))))))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert msg "\n")
        (message (if successp msg
                   (format "%s Hit %s or see buffer %s for details."
                           msg key (current-buffer)))))
      (unless (memq (process-status process) '(run open))
        (dired-uncache default-directory)))
    (setq magit-process nil)
    (magit-set-mode-line-process nil)
    (magit-refresh-buffer magit-process-client-buffer)))



(defvar litprog-process nil)
(defvar litprog-process-buffer-name "*litprog-process*")

(defun litprog-run-async (program &rest args)
  (message "Running %s %s" program (mapconcat 'identity args " "))
  (litprog-run (append (cons magit-git-executable
			     magit-git-standard-options)
		       args)
	       nil nil nil t))

;; riped from magit
(defun litprog-run (cmd args
		    &optional noerase noerror nowait input)
  (if (and litprog-process
           (get-buffer litprog-process-buffer-name))
      (error "Litprog process is already running"))
  (let (;; (cmd (car cmd-and-args))
        ;; (args (cdr cmd-and-args))
        (dir default-directory)
        (buf (get-buffer-create litprog-process-buffer-name))
        (successp nil))
    ;; (magit-set-mode-line-process
    ;;  (magit-process-indicator-from-command cmd-and-args))
    (setq litprog-process-client-buffer (current-buffer))
    (with-current-buffer buf
      (view-mode 1)
      (set (make-local-variable 'view-no-disable-on-exit) t)
      (setq view-exit-action
            (lambda (buffer)
              (with-current-buffer buffer
                (bury-buffer))))
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (setq default-directory dir)
        (if noerase
            (goto-char (point-max))
          (erase-buffer))
        (insert "$ " (mapconcat 'identity cmd-and-args " ") "\n")
	(if nowait
	    (progn
	      (setq litprog-process (apply 'start-file-process cmd buf args))
	      (set-process-sentinel litprog-process 'litprog-process-sentinel)
	      ;; (set-process-filter litprog-process 'litprog-process-filter)
	      (setq successp t))
	  ;; async
	  (while (equal (process-status litprog-process) 'run)
	    (sit-for 0.1 t))
	  (setq successp
		(equal (process-exit-status litprog-process) 0))))
      (or successp
          (error
           "%s ... [Hit %s or see buffer %s for details]"
           (or (with-current-buffer (get-buffer litprog-process-buffer-name)
                 (when (re-search-backward
                        (concat "^error: \\(.*\\)" paragraph-separate) nil t)
                   (match-string 1)))
               "Litprog process failed")
           (with-current-buffer litprog-process-client-buffer
             (key-description (car (where-is-internal
                                    'litprog-display-process))))
           litprog-process-buffer-name))
      successp)))


(defun litprog-select-backend ()
  (interactive)
  (let ((b-ends (loop for B in litprog-options
		     collect (when (plist-get B major-mode)
			       (car B))))
	(b-ens (delq 'defaults (delq nil b-ends)))
	(b-end (and b-ends
		    (completing-read "Select backend: " b-ends nil t))))
    (litprog-install-backend b-end)))


(defun litprog-install-backend (backend)
  (litprog-set-options litprog-mode) ;; reset all to defaults
  (eval `(litprog-set-options ,major-mode)) ;; major-mode defaults
  (eval `(litprog-set-options (,backend ,major-mode))) ;;backend 
  (run-hooks (intern (format "lp-%s-install-hook" (symbol-name b-end)))))


(defmacro litprog-define-backend (name &optional doc)
  "Define NAME backend.
DOC iis an optional documentation string.

 Creates litprog-[name]-backend variable 
 Crates two hooks lp-[name]-install-hook and
lp-[name]-uninstall-hook.
"
  (declare (indent defun))
  (let ((in-name (intern (format "lp-%s-install-hook" (symbol-name name))))
	(un-name (intern (format "lp-%s-uninstall-hook" (symbol-name name)))))
    (plist-put litprog-options name nil)
    `(progn
       (defvar ,(intern (format "litprog-%s-backend" name))
	 ,doc)
       (defvar ,in-name nil
	,(format "Hook run when backend '%s' is installed" name))
       (defvar ,un-name nil
	 ,(format "Hook run when backend '%s' is uninstalled" name)))))
	
  


;;; Sweave
(defvar lp-latex-use-auctex-p t
  "Try to use auctex interface if available.")

(defvar lp-latex-use-latexmk-p t
  "Try to use latexmk if available.")



(litprog-define-backend latex)

(litprog-define-backend Sweave)
(litprog-customize (Sweave litprog-noweb-mode)
  
    litprog-code-mode				'R-mode
    litprog-fl-literal-syntactic-matcher	'("\\(\\\\Sexpr{\\)[^}]*\\(}\\)" (1 "|") (2 "|"))
    
    )



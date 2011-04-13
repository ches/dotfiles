;; -----------------------------------------------------------------
;; Dired
;; -----------------------------------------------------------------

;; Load dired-x - extra dired features
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
;;	    (setq dired-x-hands-off-my-keys nil)
	    (setq dired-guess-shell-gnutar "tar")
	    ))

;; Omit uninteresting files from dired listing. Can show with \M-o
(add-hook 'dired-mode-hook
	  (lambda ()
            ;; vanilla dired
            (define-key dired-mode-map [delete] 'dired-do-delete)
	    (define-key dired-mode-map [C-return] 'dired-find-file-other-window)
            ;; dired-x
	    (setq dired-omit-files-p t)
	    (setq dired-omit-files "^#\\|^\\.$\\|~$\\|^\\..+$")
	    ))

;; Sort dired listings with directories first
(add-hook 'dired-after-readin-hook 
	  (lambda ()
	    (save-excursion
	      (let (buffer-read-only)
		(sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
	    (set-buffer-modified-p nil)))

;; Current emacs should already define autoloads so these work
;; before Dired has been invoked in the session
(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)


(load "ches/dired")
(load "ches/python")
(load "ches/ruby")

;; make #! scripts executable after saving them
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Multiple Major Modes
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)


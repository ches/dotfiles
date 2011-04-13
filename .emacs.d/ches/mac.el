;; Make Carbon Emacs behave like console version
(setq mac-option-modifier 'meta)

;; Use command as the meta key
(setq ns-command-modifier (quote meta))

;; Fullscreen with meta-return. This may be Carbon Emacs-specific
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                           'fullboth)))

(global-set-key [(meta return)] 'toggle-fullscreen)

;; For (Cocoa) Emacs 23 (patch needed?)
;;(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

;; iTunes control? Okay.
(setq itunes-key [f5])
(require 'osx-itunes)


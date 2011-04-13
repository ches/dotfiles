(add-to-list 'load-path "~/.emacs.d")

;; Pristine vendor code
(add-to-list 'load-path "~/.emacs.d/vendor")

;; where to keep emacs' generated customizations file
(setq custom-file "~/.emacs.d/ches/custom.el")
(when (file-exists-p custom-file) (load custom-file))

(load "ches/global")
(load "ches/functions")
(load "ches/bindings")
(load "ches/modes")
(load "ches/mac")

(vendor 'color-theme)
(vendor 'pair-mode)

;; ELSE mode - a type of abbrev expansion
(vendor 'else-mode)

(vendor 'mmm-mode)
(vendor 'mmm-auto)

;; Yet Another snippet mode. They even called it that. This might replace ELSE.
;; http://code.google.com/p/yasnippet/
(vendor 'yasnippet-bundle)

;; This is the full way to load, if not compiled into a bundle (slow):
;(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet")
;(require 'yasnippet)
;(yas/initialize)
;(yas/load-directory "~/.emacs.d/site-lisp/yasnippet/snippets")

;; Fancier git functionality than the standard vc-git lib provides
; (add-to-list 'load-path "~/.emacs.d/site-lisp/git-emacs")
(vendor 'git-emacs)

;; Enable a backtrace if customizations make emacs shit the bed
; (setq debug-on-error t)


(pair-mode t)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Use ssh by default for Tramp
(require 'tramp)
(setq tramp-default-method "ssh")


;; DoReMi stuff -- I won't use this all the time
(require 'ring+)
(require 'doremi-cmd)
(require 'doremi-frm)

;; 'Open Recent...'
(require 'recentf)
(recentf-mode 1)

;; Smooth scrolling
(require 'smooth-scrolling)

;; nxml mode
;; It's big and slow to load, but we might be able to byte-compile new versions
;;(load "/Applications/Development/Emacs.app/Contents/Resources/site-lisp/nxhtml/autostart.el")

;; Setting up icicles. Lots of completion and shortcut-teaching.
;; (require 'icicles)
;; (icicle-mode 1) ; Turn on Icicle mode.

;; Setting up Abbrevs
;; (setq abbrev-file-name "~/.emacs.d/abbrev_defs") ;tell emacs where to
;;                                      ;read abbrev definitions from

;; (setq save-abbrevs t)                        ;save abbrevs when files are
;;                                      ;saved

;; (read-abbrev-file "~/.emacs.d/abbrev_defs")  ;read abbreviations
;;                                              ;file at startup

;; Emacs Code Browser
;(add-to-list 'load-path "~/.emacs.d/site-lisp/ecb-2.32")
;(require 'ecb)

;; redefining the make-backup-file-name function in order to get
;; backup files in ~/.backups/ rather than scattered around all over
;; the filesystem. Note that you must have a directory ~/.backups/
;; made.  This function looks first to see if that folder exists.  If
;; it does not the standard backup copy is made.
(defun make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name'."
  (require 'dired)
  (if (file-exists-p "~/.backup/emacs/")
      (concat (expand-file-name "~/.backup/emacs/")
              (replace-regexp-in-string "/" "!" file-name))
    (concat file-name "~")))

;; redefining the make-auto-save-file-name function in order to get
;; autosave files sent to a single directory.  Note that this function
;; looks first to determine if you have a ~/.autosaves/ directory.  If
;; you do not it proceeds with the standard auto-save procedure.
(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.."
  (if buffer-file-name
      (if (file-exists-p "~/.autosave/emacs/")
          (concat (expand-file-name "~/.autosave/emacs/") "#"
                  (replace-regexp-in-string "/" "!" buffer-file-name)
                  "#")
         (concat
          (file-name-directory buffer-file-name)
          "#"
          (file-name-nondirectory buffer-file-name)
          "#"))
    (expand-file-name
     (concat "#%" (buffer-name) "#"))))


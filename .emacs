;; Make Carbon Emacs behave like console version
(setq mac-option-modifier 'meta)

;; My location for external packages.
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; DoReMi stuff -- I won't use this all the time
(require 'ring+)
(require 'doremi-cmd)
(require 'doremi-frm)

;; On Mac OS X, Emacs launched from a bundle
;; needs paths to be set explicitly
;(add-to-list 'exec-path "/usr/local/bin")
;(add-to-list 'exec-path "/usr/local/mysql/bin")
;(add-to-list 'exec-path (getenv "PATH"))
(setq exec-path (split-string "/Library/Frameworks/Python.framework/Versions/Current/bin:/Users/ches/bin:/bin:/sbin:/usr/local/bin:/usr/bin:/usr/sbin" path-separator))
(setenv "PATH" (mapconcat 'identity exec-path ":"))
(setenv "MANPATH" "/usr/local/share/man:/usr/share/man:/usr/X11R6/man:/man")
(setenv "INFOPATH" "/usr/local/info:/usr/local/share/info:/usr/share/info")

;; Just a few miscellaneous bits
(pc-selection-mode t)
(delete-selection-mode t)
(show-paren-mode t)
(transient-mark-mode t)
(line-number-mode t)
(column-number-mode t)
(setq-default default-truncate-lines t)

;; smart pairs for quotes and brackets
(require 'pair-mode)
(pair-mode t)

;; Syntax highlighting
(global-font-lock-mode 1)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Nice quick buffer switching
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(require 'ido)
(ido-mode t)

;; 'Open Recent...'
(require 'recentf)
(recentf-mode 1)

;; Look at the pretty colors!
(require 'color-theme)
(color-theme-initialize)
;; I used this theme as a starting point but it should be largely unneeded now.
;; TODO: finish using Custom to define colors, and make a color theme with results
(color-theme-clarity)

;; default to better frame titles
(setq frame-title-format
(concat "%b - emacs@" (system-name)))

;; hide toolbar
(tool-bar-mode -1)

;; Smooth scrolling
(require 'smooth-scrolling)

;; Fullscreen with meta-return. This may be Carbon Emacs-specific
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                           'fullboth)))

(global-set-key [(meta return)] 'toggle-fullscreen)

;; make #! scripts executable after saving them
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Multiple Major Modes
(add-to-list 'load-path "~/.emacs.d/site-lisp/mmm-mode")
(require 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(global-set-key [f8] 'mmm-parse-buffer)
(setq mmm-submode-decoration-level 2)
;; These are terrible, I just need obvious examples to start customizing...
(set-face-background 'mmm-output-submode-face  "royalblue")
(set-face-background 'mmm-code-submode-face    "royalblue")
(set-face-background 'mmm-comment-submode-face "darkgrey")
(set-face-background 'mmm-default-submode-face "DimGrey")

;; nxml mode
;; It's big and slow to load, but we might be able to byte-compile new versions
(load "/Applications/Development/Emacs.app/Contents/Resources/site-lisp/nxhtml/autostart.el")

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

;; Python mode
;; This first commented out stuff is for the old python-mode --
;; hanging on to it for awhile.
;(add-to-list 'load-path "~/.emacs.d/site-lisp/python-mode-1.0")
;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
;(autoload 'python-mode "python-mode" "Python editing mode." t)
;(require 'pycomplete)
(defun my-python-hook ()
  ;; ipython breaks this... why?
  ;(define-key python-mode-map "\C-m" 'newline-and-indent)
  (eldoc-mode 1))

(add-hook 'python-mode-hook 'my-python-hook)

;; Python Doctest mode and MMM
;; TODO: try to adjust background color of docstrings in MMM
(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
(autoload 'doctest-mode "doctest-mode" "doctest mode" t)
(autoload 'doctest-register-mmm-classes "doctest-mode")
(doctest-register-mmm-classes t t)

;; Use the ipython shell
(setq ipython-command "/usr/local/bin/ipython")
(require 'ipython)
;(autoload 'ipython "ipython" "IPython interactive shell." t)

;; Mako mode
;; TODO: nxhtml how has MuMaMo for multi major modes; figure out set up for Mako & ERB?
(require 'mmm-mako)
(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)

;; Use ssh by default for Tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;; ELSE mode - a type of abbrev expansion
(add-to-list 'load-path "~/.emacs.d/site-lisp/else")
(require 'else-mode)

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


;; -----------------------------------------------------------------
;; Ruby and Rails stuff
;; -----------------------------------------------------------------
;; TODO: handle .html.erb, rspec tests
(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(setq auto-mode-alist  (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.rhtml$" . html-mode) auto-mode-alist))

(modify-coding-system-alist 'file "\\.rb$" 'utf-8)
(modify-coding-system-alist 'file "\\.rhtml$" 'utf-8)

(require 'snippet)
(require 'rails)

(defun try-complete-abbrev (old)
   (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))

(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace)
                           )))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "IMENU")
            (require 'ruby-electric)
            (ruby-electric-mode t)
            ))

;; -----------------------------------------------------------------
;; Now for Erb
;; -----------------------------------------------------------------
;; TODO: as for Mako, figure out MuMaMo for nxhtml, maybe
(mmm-add-classes
 '((erb-code
    :submode ruby-mode
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[#=]?"
    :back "-?%>"
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )))
(add-hook 'html-mode-hook
          (lambda ()
            (setq mmm-classes '(erb-code))
            (mmm-mode-on)))
(add-to-list 'auto-mode-alist '(".rhtml$" . html-mode))


;; Finally, put 'custom' stuff in its own file
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

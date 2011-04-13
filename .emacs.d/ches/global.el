;; Don't show the startup screen
(setq inhibit-startup-message t)

;; hide toolbar
(tool-bar-mode -1)

;; On Mac OS X, Emacs launched from a bundle
;; needs paths to be set explicitly
;(add-to-list 'exec-path "/usr/local/bin")
;(add-to-list 'exec-path "/usr/local/mysql/bin")
;(add-to-list 'exec-path (getenv "PATH"))
(setq exec-path (split-string "/Library/Frameworks/Python.framework/Versions/Current/bin:/Users/ches/bin:/bin:/sbin:/usr/local/bin:/usr/bin:/usr/sbin" path-separator))
(setenv "PATH" (mapconcat 'identity exec-path ":"))
(setenv "MANPATH" "/usr/local/share/man:/usr/share/man:/usr/X11R6/man:/man")
(setenv "INFOPATH" "/usr/local/info:/usr/local/share/info:/usr/share/info")

;; Highlight marked region and give it some extra magic
(transient-mark-mode t)

;; Just a few miscellaneous bits
(pc-selection-mode t)
(delete-selection-mode t)
(show-paren-mode t)
(line-number-mode t)
(column-number-mode t)
(setq visible-bell t)  ;; as in NOT audible :-)
(setq-default default-truncate-lines t)
(prefer-coding-system 'utf-8)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Syntax highlighting
(global-font-lock-mode 1)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; default to better frame titles
(setq frame-title-format
  (concat "%b - emacs@" (system-name)))


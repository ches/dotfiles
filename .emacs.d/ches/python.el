;; Python mode
;; This first commented out stuff is for the old python-mode --
;; hanging on to it for awhile.
;(add-to-list 'load-path "~/.emacs.d/vendor/python-mode-1.0")
;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
;(autoload 'python-mode "python-mode" "Python editing mode." t)
;(require 'pycomplete)

(add-to-list 'load-path "~/.emacs.d/vendor/mmm-mode")

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


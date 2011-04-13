;; For loading libraries in from the vendor directory, with custom user
;; extensions required if present based on naming convention.
;;
;; Snagged from Chris Wanstrath:
;;   https://github.com/defunkt/emacs
;;
;; Modified to support autoload for startup speed:
;;   https://github.com/rmm5t/dotfiles
(defun vendor (library &rest autoload-functions)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/vendor/" file))
         (suffix (concat normal ".el"))
         (mine (concat "~/.emacs.d/ches/" file))
	 (found nil))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (set 'found t))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (set 'found t))
     ((file-exists-p suffix) (set 'found t)))
    (when found
      (if autoload-functions
	  (dolist (autoload-function autoload-functions)
	    (autoload autoload-function (symbol-name library) nil t))
	(require library)))
    (when (file-exists-p (concat mine ".el"))
      (load mine))))


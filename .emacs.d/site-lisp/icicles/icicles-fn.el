;;; icicles-fn.el --- Non-interactive functions for Icicles
;;
;; Filename: icicles-fn.el
;; Description: Non-interactive functions for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2008, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:53 2006
;; Version: 22.0
;; Last-Updated: Tue Apr  1 13:42:55 2008 (Pacific Daylight Time)
;;           By: dradams
;;     Update #: 7215
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-fn.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `cl', `color-theme', `cus-face',
;;   `easymenu', `ffap', `ffap-', `hexrgb', `icicles-opt',
;;   `icicles-var', `thingatpt', `thingatpt+', `wid-edit',
;;   `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  non-interactive functions.  See `icicles.el' for documentation.
;;
;;  Non-interactive functions defined here:
;;
;;    `assq-delete-all', `icicle-abbreviate-or-expand-file-name',
;;    `icicle-any-candidates-p', `icicle-apropos-any-candidates-p',
;;    `icicle-apropos-any-file-name-candidates-p',
;;    `icicle-apropos-candidates', `icicle-assoc-delete-all',
;;    `icicle-barf-if-outside-Completions',
;;    `icicle-barf-if-outside-Completions-and-minibuffer',
;;    `icicle-barf-if-outside-minibuffer', `icicle-bind-isearch-keys',
;;    `icicle-call-then-update-Completions', `icicle-candidate-set-1',
;;    `icicle-case-insensitive-string-less-p',
;;    `icicle-case-string-less-p', `icicle-choose-completion-string',
;;    `icicle-clear-minibuffer', `icicle-color-blue-lessp',
;;    `icicle-color-green-lessp', `icicle-color-hue-lessp',
;;    `icicle-color-name-w-bg', `icicle-color-red-lessp',
;;    `icicle-color-saturation-lessp', `icicle-color-value-lessp',
;;    `icicle-command-abbrev-save',
;;    `icicle-command-abbrev-used-more-p',
;;    `icicle-command-names-alphabetic-p', `icicle-completing-p',
;;    `icicle-completing-read', `icicle-completing-read-history',
;;    `icicle-completion-setup-function', `icicle-custom-type',
;;    `icicle-delete-if', `icicle-delete-if-not',
;;    `icicle-delete-whitespace-from-string', `icicle-dirs-last-p',
;;    `icicle-display-completion-list', `icicle-display-Completions',
;;    `icicle-display-candidates-in-Completions',
;;    `icicle-expanded-common-match',
;;    `icicle-expanded-common-match-1', `icicle-expand-file-name',
;;    `icicle-face-valid-attribute-values', `icicle-file-directory-p',
;;    `icicle-file-name-apropos-candidates',
;;    `icicle-file-name-directory-w-default',
;;    `icicle-file-name-input-p', `icicle-file-name-nondirectory',
;;    `icicle-file-name-prefix-candidates', `icicle-file-readable-p',
;;    `icicle-file-remote-p', `icicle-file-writable-p',
;;    `icicle-files-within', `icicle-filter-alist',
;;    `icicle-filter-wo-input', `icicle-first-matching-candidate',
;;    `icicle-fit-Completions-window', `icicle-fix-default-directory',
;;    `icicle-frames-on', `icicle-fuzzy-candidates',
;;    `icicle-get-alist-candidate',
;;    `icicle-highlight-candidate-in-Completions',
;;    `icicle-highlight-complete-input',
;;    `icicle-highlight-initial-whitespace',
;;    `icicle-highlight-input-noncompletion',
;;    `icicle-highlight-input-noncompletion-rest',
;;    `icicle-highlight-lighter', `icicle-historical-alphabetic-p',
;;    `icicle-increment-cand-nb+signal-end',
;;    `icicle-insert-candidates',
;;    `icicle-insert-Completions-help-string',
;;    `icicle-last-modified-first-p',
;;    `icicle-lisp-vanilla-completing-read', `icicle-key-description',
;;    `icicle-make-face-candidate',
;;    `icicle-maybe-sort-and-strip-candidates',
;;    `icicle-minibuffer-contents',
;;    `icicle-minibuffer-contents-from-minibuffer',
;;    `icicle-minibuffer-prompt-end', `icicle-most-recent-first-p',
;;    `icicle-msg-maybe-in-minibuffer', `icicle-next-candidate',
;;    `icicle-part-1-lessp', `icicle-part-2-lessp',
;;    `icicle-part-3-lessp', `icicle-part-4-lessp',
;;    `icicle-part-N-lessp', `icicle-place-cursor',
;;    `icicle-place-overlay', `icicle-prefix-any-candidates-p',
;;    `icicle-prefix-any-file-name-candidates-p',
;;    `icicle-prefix-candidates', `icicle-prefix-keys-first-p',
;;    `icicle-proxy-candidate-first-p', `icicle-put-alist-candidate',
;;    `icicle-put-at-head', `icicle-read-char-exclusive',
;;    `icicle-read-face-name', `icicle-read-file-name',
;;    `icicle-read-from-minibuffer',
;;    `icicle-read-from-minibuf-nil-default', `icicle-read-number',
;;    `icicle-read-string', `icicle-read-string-completing',
;;    `icicle-recompute-candidates',
;;    `icicle-redefine-standard-options',
;;    `icicle-redefine-std-completion-fns', `icicle-remove-dots',
;;    `icicle-remove-duplicates', `icicle-remove-property',
;;    `icicle-restore-standard-commands',
;;    `icicle-restore-standard-options',
;;    `icicle-restore-std-completion-fns', `icicle-reversible-sort',
;;    `icicle-save-or-restore-input', `icicle-scatter',
;;    `icicle-scatter-match', `icicle-scroll-or-update-Completions',
;;    `icicle-set-difference', `icicle-set-intersection',
;;    `icicle-set-union', `icicle-special-candidates-first-p',
;;    `icicle-start-of-candidates-in-Completions',
;;    `icicle-strip-ignored-files-and-sort',
;;    `icicle-subst-envvar-in-file-name',
;;    `icicle-substring-no-properties', `icicle-unhighlight-lighter',
;;    `icicle-unsorted-apropos-candidates',
;;    `icicle-unsorted-file-name-apropos-candidates',
;;    `icicle-unsorted-file-name-prefix-candidates',
;;    `icicle-unsorted-prefix-candidates',
;;    `icicle-update-completions', `icicle-value-satisfies-type-p',
;;    `icicle-var-inherits-type-p', `icicle-var-is-of-type-p',
;;    `icicle-var-matches-type-p', `icicle-var-val-satisfies-type-p',
;;    `old-completing-read', `old-choose-completion-string',
;;    `old-completion-setup-function', `old-read-file-name'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `completing-read'              - (See below and doc string.)
;;  `display-completion-list'      - (See below and doc string.)
;;  `face-valid-attribute-values'  - (See below and doc string.)
;;  `read-file-name'               - (See below and doc string.)
;;  `read-from-minibuffer'         - (See below and doc string.)
;;  `read-string'                  - (See below and doc string.)
;;
;;
;;  ***** NOTE: The following functions defined in `simple.el' have
;;              been REDEFINED HERE:
;;
;;  `choose-completion-string' -
;;     Don't exit minibuffer after `lisp-complete-symbol' completion.
;;  `completion-setup-function' - 1. Put faces on inserted string(s).
;;                                2. Help on help.
;;  `repeat-complex-command' - Use `completing-read' to read command.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Redefined standard functions")
;;  (@> "Icicles functions - completion display (not cycling)")
;;  (@> "Icicles functions - prefix completion cycling")
;;  (@> "Icicles functions - apropos completion cycling")
;;  (@> "Icicles functions - common helper functions")
;;  (@> "Icicles functions - sort functions")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; case
                                  ;; plus, for Emacs < 21: dolist, push, pop
                                  ;; plus, for Emacs < 20: when, unless

(when window-system (require 'hexrgb nil t))
  ;; (no error if not found): hexrgb-color-name-to-hex, hexrgb-(red|green|blue|hue|saturation|value)
(require 'wid-edit+ nil t) ;; (no error if not found):
                           ;; redefined color widget (for icicle-var-is-of-type-p)
(require 'icicles-opt)
  ;; icicle-Completions-display-min-input-chars, icicle-cycle-into-subdirs-flag,
  ;; icicle-expand-input-to-common-match-flag, icicle-highlight-historical-candidates-flag,
  ;; icicle-highlight-input-initial-whitespace-flag, icicle-ignore-space-prefix-flag,
  ;; icicle-incremental-completion-delay, icicle-incremental-completion-flag,
  ;; icicle-incremental-completion-threshold, icicle-init-value-flag, icicle-list-end-string,
  ;; icicle-list-join-string, icicle-mark-position-in-candidate, icicle-point-position-in-candidate,
  ;; icicle-regexp-quote-flag, icicle-require-match-flag,
  ;; icicle-show-Completions-help-flag, icicle-sort-function, icicle-special-candidate-regexp,
  ;; icicle-transform-function, icicle-use-~-for-home-dir-flag
(require 'icicles-var)
  ;; icicle-candidate-nb, icicle-candidate-action-fn, icicle-candidate-properties-alist,
  ;; icicle-cmd-calling-for-completion, icicle-common-match-string, icicle-complete-input-overlay,
  ;; icicle-completing-p, icicle-completion-candidates, icicle-current-completion-mode,
  ;; icicle-current-input, icicle-current-raw-input, icicle-default-directory, icicle-edit-update-p,
  ;; icicle-extra-candidates, icicle-ignored-extensions-regexp, icicle-incremental-completion-p,
  ;; icicle-initial-value, icicle-last-completion-candidate, icicle-last-input,
  ;; icicle-must-match-regexp, icicle-must-not-match-regexp, icicle-must-pass-predicate,
  ;; icicle-nb-of-other-cycle-candidates, icicle-prompt, icicle-re-no-dot,
  ;; icicle-reverse-sort-p, icicle-saved-completion-candidates

;; This requirement is real, but leads to recursion.
;; You should, in any case, just load everything by loading `icicles.el'.
;; (require 'icicles-mode) ;; icicle-mode


;; Byte-compiling this file, you will likely get some error or warning
;; messages. All of the following are benign.  They are due to
;; differences between different versions of Emacs.
;;
;; Compiling in Emacs 22:
;;
;; Warning: `directory-sep-char' is an obsolete variable (as of Emacs 21.1); do not use it.
;; Warning: `make-local-hook' is an obsolete function (as of Emacs 21.1); not necessary any more.
;;
;; Compiling in Emacs 20:
;;
;; The following functions are not known to be defined:
;;     minibufferp, minibuffer-prompt-end, field-string, minibuffer-completion-contents,
;;     display-mouse-p, propertize, delete-dups, completing-read-multiple, test-completion,
;;     x-font-family-list, internal-lisp-face-attribute-values, fit-window-to-buffer,
;;     minibuffer-contents-no-properties, delete-minibuffer-contents


;;; Defvars to quiet byte-compiler

(when (< emacs-major-version 22)
  (defvar completion-common-substring)
  (defvar completion-root-regexp)
  (defvar directory-sep-char)
  (defvar minibuffer-completing-symbol)
  (defvar minibuffer-prompt-properties)
  (defvar partial-completion-mode)
  (defvar read-file-name-completion-ignore-case))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Redefined standard functions")

;;; Redefined standard functions -------------------------------------


;;; REPLACE ORIGINAL `choose-completion-string' in `simple.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Don't exit minibuffer if this is just a `lisp-complete-symbol' completion.
;;; Go to point-max before insert choice.  Respect `icicle-dir-candidate-can-exit-p'.
;;;
;;; Free variable `completion-reference-buffer' is defined in `simple.el'.
;;;
(or (fboundp 'old-choose-completion-string)
(fset 'old-choose-completion-string (symbol-function 'choose-completion-string)))

;;;###autoload
(cond ((> emacs-major-version 21)       ; Emacs 22
       (defun icicle-choose-completion-string (choice &optional buffer base-size)
         "Switch to BUFFER and insert the completion choice CHOICE.
BASE-SIZE, if non-nil, says how many characters of BUFFER's text
to keep.  If it is nil, we call `choose-completion-delete-max-match'
to decide what to delete.
If BUFFER is the minibuffer, then exit the minibuffer, unless one of
the following is true:
   - it is reading a file name, CHOICE is a directory, and
     `icicle-dir-candidate-can-exit-p' is nil
   - `completion-no-auto-exit' is non-nil
   - this is just a `lisp-complete-symbol' completion."
         (let* ((buffer (or buffer completion-reference-buffer))
                (mini-p (minibufferp buffer)))
           ;; If BUFFER is a minibuffer, barf unless it's currently active.
           (if (and mini-p (or (not (active-minibuffer-window))
                               (not (equal buffer (window-buffer (active-minibuffer-window))))))
               (error "Minibuffer is not active for completion")
             ;; Set buffer so buffer-local `choose-completion-string-functions' works.
             (set-buffer buffer)
             (unless (run-hook-with-args-until-success 'choose-completion-string-functions
                                                       choice buffer mini-p base-size)
               ;; Insert the completion into the buffer where completion was requested.
               (if base-size
                   (delete-region (+ base-size (if mini-p (minibuffer-prompt-end) (point-min)))
                                  (if mini-p (point-max) (point)))
                 (choose-completion-delete-max-match choice))
               (goto-char (point-max))
               (insert choice)
               (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil))
               ;; Update point in the window that BUFFER is showing in.
               (let ((window (get-buffer-window buffer 0)))
                 (set-window-point window (point)))
               ;; If completing for the minibuffer, exit it with this choice,
               ;; unless this was a `lisp-complete-symbol' completion.
               (and (not completion-no-auto-exit)
                    (equal buffer (window-buffer (minibuffer-window)))
                    (or minibuffer-completion-table
                        (and icicle-mode (or icicle-extra-candidates icicle-proxy-candidates)))
                    (not (eq 'lisp-complete-symbol icicle-cmd-calling-for-completion))
                    ;; Exit the minibuffer if `icicle-dir-candidate-can-exit-p',
                    ;; or not reading a file name, or chosen file is not a directory.
                    (if (or icicle-dir-candidate-can-exit-p
                            (not (eq minibuffer-completion-table 'read-file-name-internal))
                            (not (file-directory-p (field-string (point-max)))))
                        (exit-minibuffer)
                      (let ((mini (active-minibuffer-window)))
                        (select-window mini)
                        (when minibuffer-auto-raise (raise-frame (window-frame mini)))))))))))

      ((> emacs-major-version 20)       ; Emacs 21
       (defun icicle-choose-completion-string (choice &optional buffer base-size)
         "Switch to BUFFER and insert the completion choice CHOICE.
BASE-SIZE, if non-nil, says how many characters of BUFFER's text
to keep.  If it is nil, we call `choose-completion-delete-max-match'
to decide what to delete.
If BUFFER is the minibuffer, then exit the minibuffer, unless one of
the following is true:
   - it is reading a file name, CHOICE is a directory, and
     `icicle-dir-candidate-can-exit-p' is nil
   - `completion-no-auto-exit' is non-nil
   - this is just a `lisp-complete-symbol' completion."
         (let ((buffer (or buffer completion-reference-buffer))
               (mini-p (save-match-data
                         (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer)))))
           ;; If BUFFER is a minibuffer, barf unless it's currently active.
           (if (and mini-p
                    (or (not (active-minibuffer-window))
                        (not (equal buffer
                                    (window-buffer (active-minibuffer-window))))))
               (error "Minibuffer is not active for completion")
             ;; Insert the completion into the buffer where completion was requested.
             (set-buffer buffer)
             (if base-size
                 (delete-region (+ base-size (if mini-p (icicle-minibuffer-prompt-end) (point-min)))
                                (if mini-p (point-max) (point)))
               (choose-completion-delete-max-match choice))
             (goto-char (point-max))
             (insert choice)
             (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil))
             ;; Update point in the window that BUFFER is showing in.
             (let ((window (get-buffer-window buffer 0)))
               (set-window-point window (point)))
             ;; If completing for the minibuffer, exit it with this choice,
             ;; unless this was a `lisp-complete-symbol' completion.
             (and (not completion-no-auto-exit)
                  (equal buffer (window-buffer (minibuffer-window)))
                  (or minibuffer-completion-table
                      (and icicle-mode (or icicle-extra-candidates icicle-proxy-candidates)))
                  (not (eq 'lisp-complete-symbol icicle-cmd-calling-for-completion))
                  ;; Exit the minibuffer if `icicle-dir-candidate-can-exit-p',
                  ;; or not reading a file name, or chosen file is not a directory.
                  (if (or icicle-dir-candidate-can-exit-p
                          (not (eq minibuffer-completion-table 'read-file-name-internal))
                          (not (file-directory-p (field-string (point-max)))))
                      (exit-minibuffer)
                    (let ((mini (active-minibuffer-window)))
                      (select-window mini)
                      (when minibuffer-auto-raise (raise-frame (window-frame mini))))))))))

      (t                                ; Emacs 20
       (defun icicle-choose-completion-string (choice &optional buffer base-size)
         "Switch to BUFFER and insert the completion choice CHOICE.
 BASE-SIZE, if non-nil, says how many characters of BUFFER's text
 to keep.  If it is nil, we call `choose-completion-delete-max-match'
 to decide what to delete.
 If BUFFER is the minibuffer, then exit the minibuffer, unless one of
 the following is true:
    - it is reading a file name, CHOICE is a directory, and
      `icicle-dir-candidate-can-exit-p' is nil
    - `completion-no-auto-exit' is non-nil
    - this is just a `lisp-complete-symbol' completion."
         (let ((buffer (or buffer completion-reference-buffer))
               (mini-p (save-match-data
                         (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer)))))
           ;; If BUFFER is a minibuffer, barf unless it's currently active.
           (when (and mini-p
                      (or (not (active-minibuffer-window))
                          (not (equal buffer (window-buffer (active-minibuffer-window))))))
             (error "Minibuffer is not active for completion"))
           ;; Insert the completion into the buffer where completion was requested.
           (set-buffer buffer)
           (if base-size
               (delete-region (+ base-size (point-min)) (if mini-p (point-max) (point)))
             (choose-completion-delete-max-match choice))
           (goto-char (point-max))
           (insert choice)
           (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil))
           ;; Update point in the window that BUFFER is showing in.
           (let ((window (get-buffer-window buffer 0)))
             (set-window-point window (point)))
           ;; If completing for the minibuffer, exit it with this choice,
           ;; unless this was a `lisp-complete-symbol' completion.
           (and (not completion-no-auto-exit)
                (equal buffer (window-buffer (minibuffer-window)))
                (or minibuffer-completion-table
                    (and icicle-mode (or icicle-extra-candidates icicle-proxy-candidates)))
                (not (eq 'lisp-complete-symbol icicle-cmd-calling-for-completion))
                ;; Exit the minibuffer if `icicle-dir-candidate-can-exit-p',
                ;; or not reading a file name, or chosen file is not a directory.
                (if (or icicle-dir-candidate-can-exit-p
                        (not (eq minibuffer-completion-table 'read-file-name-internal))
                        (not (file-directory-p (buffer-string))))
                    (exit-minibuffer)
                  (select-window (active-minibuffer-window))))))))


;;; REPLACE ORIGINAL `completion-setup-function' in `simple.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Don't print the help lines here.  Do that in `icicle-display-completion-list' instead.
;;; That's so we can fit the *Completions* window to the buffer, including the help lines.
;;;
(or (fboundp 'old-completion-setup-function)
(fset 'old-completion-setup-function (symbol-function 'completion-setup-function)))

;;;###autoload
(when (< emacs-major-version 22)
  (defun icicle-completion-setup-function ()
    "Set up for completion.  This goes in `completion-setup-hook'
so it is called after completion-list buffer text is written."
    (save-excursion
      (let* ((mainbuf (current-buffer))
             (mbuf-contents (icicle-minibuffer-contents-from-minibuffer))
             ;; $$$$$ Should we `expand-file-name' mbuf-contents first?
             (dir-of-input (file-name-directory mbuf-contents)))
        ;; If reading file name and either icicle-comp-base-is-default-dir-p is nil or this is a
        ;; completion command, then set `default-directory' so it will be copied into *Completions*.
        (when (and minibuffer-completing-file-name dir-of-input
                   (or (and (symbolp last-command) (get this-command 'icicle-completing-command))
                       (not icicle-comp-base-is-default-dir-p)))
          (with-current-buffer mainbuf (setq default-directory dir-of-input)))
        (with-current-buffer standard-output
          (completion-list-mode)
          (set (make-local-variable 'completion-reference-buffer) mainbuf)
          (setq completion-base-size
                (cond ((and (eq minibuffer-completion-table 'read-file-name-internal)
                            icicle-comp-base-is-default-dir-p
                            (length default-directory)))
                      ((eq minibuffer-completion-table 'read-file-name-internal)
                       ;; For file name completion, use the number of chars before
                       ;; the start of the file name component at point.
                       (with-current-buffer mainbuf
                         (save-excursion
                           (skip-chars-backward (format "^%c" directory-sep-char))
                           (- (point) (icicle-minibuffer-prompt-end)))))
                      ((save-match-data (string-match "\\` \\*Minibuf-[0-9]+\\*\\'"
                                                      (buffer-name mainbuf)))
                       ;; Otherwise, in minibuffer, the whole input is being completed.
                       0))))))))

;;;###autoload
(when (>= emacs-major-version 22)
  (defun icicle-completion-setup-function ()
    "Set up for completion.  This goes in `completion-setup-hook'
so it is called after completion-list buffer text is written."
    (save-excursion
      (let* ((mainbuf (current-buffer))
             (mbuf-contents (minibuffer-completion-contents))
             ;; $$$$$ Should we `expand-file-name' mbuf-contents first?  Vanilla Emacs does that.
             (dir-of-input (file-name-directory mbuf-contents))
             common-string-length)
        ;; If reading file name and either icicle-comp-base-is-default-dir-p is nil or this is a
        ;; completion command, then set `default-directory' so it will be copied into *Completions*.
        (when (and minibuffer-completing-file-name dir-of-input
                   (or (and (symbolp this-command) (get this-command 'icicle-completing-command))
                       (not icicle-comp-base-is-default-dir-p)))
          (with-current-buffer mainbuf (setq default-directory dir-of-input)))
        (with-current-buffer standard-output
          (completion-list-mode)
          (set (make-local-variable 'completion-reference-buffer) mainbuf)
          (setq completion-base-size
                (cond ((and minibuffer-completing-file-name
                            icicle-comp-base-is-default-dir-p
                            (length default-directory)))
                      ((and (symbolp minibuffer-completion-table)
                            (get minibuffer-completion-table 'completion-base-size-function))
                       ;; To compute base size, a function can use the global value of
                       ;; `completion-common-substring' or `minibuffer-completion-contents'.
                       (with-current-buffer mainbuf
                         (funcall (get minibuffer-completion-table 'completion-base-size-function))))
                      (minibuffer-completing-file-name
                       ;; For file name completion, use the number of chars before
                       ;; the start of the file name component at point.
                       (with-current-buffer mainbuf
                         (save-excursion
                           (skip-chars-backward completion-root-regexp)
                           (- (point) (minibuffer-prompt-end)))))
                      ((and (boundp 'minibuffer-completing-symbol) minibuffer-completing-symbol) nil)
                      ;; Otherwise, in minibuffer, the base size is 0.
                      ((minibufferp mainbuf) 0)))
          (setq common-string-length
                (cond (completion-common-substring (length completion-common-substring))
                      (completion-base-size (- (length mbuf-contents) completion-base-size))))
          ;; Put faces on first uncommon characters and common parts.
          (when (and (integerp common-string-length) (>= common-string-length 0))
            (let ((element-start (point-min))
                  (maxp (point-max))
                  element-common-end)
              (while (and (setq element-start (next-single-property-change element-start
                                                                           'mouse-face))
                          (< (setq element-common-end (+ element-start common-string-length))
                             maxp))
                (when (get-char-property element-start 'mouse-face)
                  (if (and (> common-string-length 0)
                           (get-char-property (1- element-common-end) 'mouse-face))
                      (put-text-property element-start element-common-end
                                         'font-lock-face 'completions-common-part))
                  (if (get-char-property element-common-end 'mouse-face)
                      (put-text-property element-common-end (1+ element-common-end)
                                         'font-lock-face 'completions-first-difference)))))))))))

(defun icicle-insert-Completions-help-string ()
  "Add or remove help in *Completions*.
This is controlled by `icicle-show-Completions-help-flag'.  If that
option is nil, remove help; else, add it."
  (if icicle-show-Completions-help-flag
      (let ((instruction2 (or (and icicle-mode (substitute-command-keys
                                                (concat "(\\<minibuffer-local-completion-map>"
                                                        "\\[icicle-completion-help]: help) ")))
                              ""))
            instruction1)
        (cond ((< emacs-major-version 22)
               (setq instruction1 (if window-system ; We have a mouse.
                                      (substitute-command-keys "Click \\<completion-list-mode-map>\
\\[mouse-choose-completion] on a completion to select it.  ")
                                    (substitute-command-keys ; No mouse.
                                     "In this buffer, type \\<completion-list-mode-map>\
\\[choose-completion] to select the completion near point.  "))))
              ((>= emacs-major-version 22)
               (setq instruction1 (if (display-mouse-p) ; We have a mouse.
                                      (substitute-command-keys
                                       "Click \\<completion-list-mode-map>\
\\[mouse-choose-completion] or type \\[choose-completion] on a completion to select it.  ")
                                    (substitute-command-keys ; No mouse.
                                     "In this buffer, type \\<completion-list-mode-map>\
\\[choose-completion] to select the completion near point.  ")))))
        (goto-char (point-min))
        (put-text-property 0 (length instruction1) 'face 'icicle-Completions-instruction-1
                           instruction1)
        (put-text-property 0 (length instruction2) 'face 'icicle-Completions-instruction-2
                           instruction2)
        (insert instruction1 instruction2 "\n"))

    ;; Not showing help.  Remove standard Emacs help string.
    (goto-char (point-min))
    (re-search-forward "Possible completions are:\n")
    (delete-region (point-min) (point))))

(defun icicle-read-from-minibuf-nil-default (prompt &optional initial-contents keymap read hist
                                             default-value inherit-input-method)
  "Like `read-from-minibuffer', but return nil for empty input.
Args are as for `read-from-minibuffer'.
If nothing is input, then nil is returned."
  (let ((input (read-from-minibuffer prompt initial-contents keymap nil hist default-value
                                     inherit-input-method)))
    (if (string= "" input) nil (if read (car (read-from-string input)) input))))

(defun icicle-completing-read-history (prompt &optional hist pred init-input def inherit-i-m)
  "Lax `completing-read' against entries in history HIST.
Arguments are as for `completing-read'.  HIST is a symbol that is a
history variable.  It defaults to `minibuffer-history'.  Completion is
lax: a match is not required."
  (setq hist (or hist 'minibuffer-history))
  (let ((hist-val (icicle-remove-duplicates (symbol-value hist))))
    (when (and (consp hist-val) (not (stringp (car hist-val)))) ; Convert, e.g. `comand-history'.
      (setq hist-val (mapcar (lambda (v) (format "%s" v)) hist-val)))
    (completing-read prompt (mapcar #'list hist-val) pred nil init-input hist def inherit-i-m)))

;; Pretty much a straight transcription into Lisp of the C code that defines `completing-read'.
(defun icicle-lisp-vanilla-completing-read (prompt collection &optional predicate require-match
                                            initial-input hist def inherit-input-method)
  "Lisp version of vanilla Emacs `completing-read'."
  (let ((pos 0)
        val histvar histpos position init)
    (setq init                            initial-input
          minibuffer-completion-table     collection
          minibuffer-completion-predicate predicate
          minibuffer-completion-confirm   (if (eq require-match t) nil require-match))
    (setq position nil)
    (when init
      (when (consp init) (setq position (cdr init)   init (car init)))
      (unless (stringp init)
        (error "icicle-lisp-vanilla-completing-read, INIT must be a string: %S" init))
      (if (not position)
          (setq pos (1+ (length init))) ; Default is to put cursor at end of INITIAL-INPUT.
        (unless (integerp position)
          (error "icicle-lisp-vanilla-completing-read, POSITION must be an integer: %S" position))
        (setq pos (1+ position)))) ; Convert zero-based to one-based.
    (if (symbolp hist)
        (setq histvar hist    histpos nil)
      (setq histvar (car-safe hist)
            histpos (cdr-safe hist)))
    (unless histvar (setq histvar 'minibuffer-history))
    (unless histpos (setq histpos 0))
    (setq val (read-from-minibuffer
               prompt
               (cons init pos)          ; initial-contents
               (if (not require-match)  ; key map
                   (if (or (not minibuffer-completing-file-name)
                           (eq minibuffer-completing-file-name 'lambda)
                           (not (boundp 'minibuffer-local-filename-completion-map)))
                       minibuffer-local-completion-map
                     minibuffer-local-filename-completion-map)
                 (if (or (not minibuffer-completing-file-name)
                         (eq minibuffer-completing-file-name 'lambda)
                         (not (boundp 'minibuffer-local-filename-must-match-map)))
                     minibuffer-local-must-match-map
                   minibuffer-local-filename-must-match-map))
               nil histvar def inherit-input-method))
    (when (and (stringp val) (string= val "") def) (setq val def))
    val))


;;; REPLACE ORIGINAL `completing-read' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Allows for completion candidates that are lists of strings.
;;; Allows for reading and returning completion candidates that are strings with properties.
;;; Adds completion status indicator to minibuffer and mode-line lighter.
;;; Removes *Completions* window.
;;;
;;; We use M@R%M=X!L$S+P&L^T*Z instead of HIST, to avoid name capture by
;;; `minibuffer-history-variable's value.  If we didn't need to be Emacs 20-compatible, then we
;;; could employ `#1=#:hist'...`#1#'...`#1' read syntax to use an uninterned symbol.
;;;
(or (fboundp 'old-completing-read)
(fset 'old-completing-read (symbol-function 'completing-read)))

;;;###autoload
(defun icicle-completing-read (prompt table &optional predicate require-match
                               initial-input m@r%m=x!l$s+p&l^t*z def inherit-input-method)
  "Read string in minibuffer, with completion and cycling of completions.
Prefix completion via \\<minibuffer-local-completion-map>\
`\\[icicle-prefix-word-complete]' (word) and `\\[icicle-prefix-complete]' (full).
Apropos (regexp) completion via `\\[icicle-apropos-complete]'.

Prefix cycling of candidate completions via `\\[icicle-previous-prefix-candidate]' and \
`\\[icicle-next-prefix-candidate]'.
Apropos cycling of candidate completions via `\\[icicle-previous-apropos-candidate]' and \
`\\[icicle-next-apropos-candidate]'.

Cycling of past minibuffer inputs via `\\[previous-history-element]' and \
`\\[next-history-element]'.
Searching through input history via `\\[previous-matching-history-element]' \
and `\\[next-matching-history-element]'.

Case is ignored if `completion-ignore-case' is non-nil.
Position of the cursor (point) and the mark during completion cycling
  is determined by `icicle-point-position-in-candidate' and
  `icicle-mark-position-in-candidate', respectively.
Highlighting of the matched part of completion candidates during
  cycling is determined by `icicle-match-highlight-minibuffer',
  `icicle-match-highlight-Completions', and
  `icicle-common-match-highlight-Completions'.

Use `\\[icicle-completion-help]' during completion for more information on completion and key
bindings in Icicle mode.

Args: PROMPT, TABLE, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST:

PROMPT is a string to prompt with; normally ends in a colon and space.

TABLE is an alist whose elements' cars are strings, or an obarray.
It can also be a function that performs the completion itself.
In Emacs 22 or later, it can also be a hash table or list of strings.

In Icicle mode, the car of an alist entry can also be a list of
strings.  In this case, the completion candidate is a
multi-completion.  The strings are joined pairwise with
`icicle-list-join-string' to form the completion candidate seen by the
user, which is terminated by `icicle-list-end-string'.  You can use
variable `icicle-candidate-properties-alist' to control the appearance
of multi-completions in buffer *Completions*.  You can use variables
`icicle-list-use-nth-parts' and `icicle-list-nth-parts-join-string' to
control the minibuffer behavior of multi-completions.  See the Icicles
documentation for more information.

PREDICATE limits completion to a subset of TABLE.

See `try-completion' and `all-completions' for more details on
completion, TABLE, and PREDICATE.

If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
the input is (or completes to) an element of TABLE or is null.  If it
is also not t, then hitting `\\[exit-minibuffer]' does not exit if it performs
non-null completion.  If the input is null, `completing-read' returns
DEF, or an empty string if DEF is nil, regardless of the value
of REQUIRE-MATCH.

If option `icicle-require-match-flag' is non-nil, it overrides the
value of REQUIRE-MATCH.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially,
with point positioned at the end.  If it is (STRING . POSITION), the
initial input is STRING, but point is placed at zero-indexed position
POSITION in STRING.  (This is different from `read-from-minibuffer'
and related functions, which use one-indexing for POSITION.)

INITIAL-INPUT is considered deprecated by vanilla Emacs, but not by
Icicles.  If you pass nil for INITIAL-INPUT and supply non-nil DEF,
the user can yank DEF into the minibuffer using
`next-history-element'.

HIST, if non-nil, specifies a history list, and optionally the initial
position in the list.  It can be a symbol, which is the history list
variable to use, or it can be a cons cell (HISTVAR . HISTPOS).  In
that case, HISTVAR is the history list variable to use, and HISTPOS is
the initial position (the position in the list used by the minibuffer
history commands).  For consistency, you should also specify that
element of the history as the value of INITIAL-INPUT.  Positions are
counted starting from 1 at the beginning of the list.  The variable
`history-length' controls the maximum length of a history list.

DEF, if non-nil, is the default value.

Non-nil `icicle-init-value-flag' means that if DEF is non-nil and
INITIAL-INPUT is nil or \"\", then DEF (or its name, if it is a
symbol) is inserted in the minibuffer as the INITIAL-INPUT.  The
particular non-nil value determines whether or not the value is
preselected and where the cursor is left (at the beginning or end of
the value).

If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits the
current input method and the setting of `enable-multibyte-characters'."
  (unless (stringp icicle-initial-value) (setq icicle-initial-value ""))
  (unless initial-input (setq initial-input icicle-initial-value))
  (if (consp initial-input)
      (setq icicle-initial-value (car initial-input))
    (setq initial-input        (format "%s" initial-input) ; Convert symbol to string
          icicle-initial-value initial-input))
  (setq icicle-nb-of-other-cycle-candidates 0)

  ;; Maybe use DEF for INITIAL-INPUT also.
  (when (and icicle-init-value-flag def (stringp initial-input) (string= "" initial-input))
    (setq initial-input (if (symbolp def) (symbol-name def) def))
    (when (memq icicle-init-value-flag '(insert-start preselect-start))
      (setq initial-input (cons initial-input 0))))

  ;; Override REQUIRE-MATCH as needed.
  (setq require-match (case icicle-require-match-flag
                        ((nil) require-match)
                        (no-match-required nil)
                        (partial-match-ok t)
                        (full-match-required 'full-match-required))
        icicle-require-match-p require-match)
  (icicle-highlight-lighter)
  (let* ((minibuffer-allow-text-properties t) ; This is nil for completion in vanilla Emacs.
         (minibuffer-completion-table table)
         result)

    ;; For an alist TABLE with a string or multi-completion candidate:
    ;; 
    ;; 1. We copy the TABLE, so we don't change the original alist in any way.
    ;;    We change each entry (A . B) to (S A . B), where S is a copy of A.
    ;;    This way, the cdr of each entry is the original alist candidate, (A . B).
    ;; 2. If S is a multi-completion (X Y Z...), we change the entry to (S' A . B),
    ;;    where S' is the display string for the multi-completion.
    ;; 3. If `icicle-whole-candidate-as-text-prop-p', we put the cdr, (A . B), as a text
    ;;    property, on the car (S or S'), so we can get back the original (A . B) from the car.

    ;; 1 and 2: Copy alist and its keys, and convert each multi-completion key to a single string.
    (when (and (consp table) (consp (car table))
               (or (stringp (caar table))
                   (and (consp (caar table)) (stringp (car (caar table))))))
      (setq minibuffer-completion-table
            (setq table (if (and (consp (caar table)) (stringp (car (caar table))))
                            (mapcar (lambda (entry)
                                      (cons (concat (mapconcat #'identity (car entry)
                                                               icicle-list-join-string)
                                                    icicle-list-end-string)
                                            entry))
                                    table)
                          (mapcar (lambda (entry) (cons (copy-sequence (car entry)) entry))
                                  table))))                              

      ;; 3. Put original alist candidates on display candidates (strings), as a text property.
      (when icicle-whole-candidate-as-text-prop-p
        (dolist (cand table)
          (when (and (consp cand) (stringp (car cand))) (icicle-put-alist-candidate cand)))))
    (cond ((not icicle-mode)
           (setq icicle-prompt prompt)
           (setq result (icicle-lisp-vanilla-completing-read
                         icicle-prompt table predicate require-match initial-input
                         m@r%m=x!l$s+p&l^t*z def inherit-input-method)))
          (t
           (setq icicle-prompt
                 (if (fboundp 'propertize) ; Emacs 21+ only
                     (concat (propertize
                              (if icicle-candidate-action-fn "+" ".")
                              'face
                              (cond ((and icicle-candidate-action-fn icicle-require-match-p)
                                     '(icicle-multi-command-completion
                                       icicle-mustmatch-completion))
                                    (icicle-candidate-action-fn 'icicle-multi-command-completion)
                                    (icicle-require-match-p
                                     '(icicle-completion
                                       icicle-mustmatch-completion))
                                    (t 'icicle-completion)))
                             " "
                             (propertize prompt 'face 'minibuffer-prompt))
                   (concat (and icicle-candidate-action-fn "+ ") prompt)))
           (let ((minibuffer-prompt-properties
                  (and (boundp 'minibuffer-prompt-properties) ; Emacs 21+ only
                       (icicle-remove-property 'face minibuffer-prompt-properties))))
             (setq result (catch 'icicle-read-top
                            (icicle-lisp-vanilla-completing-read
                             icicle-prompt table predicate require-match initial-input
                             m@r%m=x!l$s+p&l^t*z def inherit-input-method))))))
    ;; HACK.  Without this, when REQUIRE-MATCH is non-nil, *Completions* window
    ;; does not disappear.
    (when require-match (icicle-remove-Completions-window))
    result))

;;;###autoload
(defun icicle-read-file-name (prompt &optional dir default-filename
                              require-match initial-input predicate)
  "Read file name, prompting with PROMPT and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Default the name to DEFAULT-FILENAME if user exits the minibuffer with
the same non-empty string that was inserted by this function.
 (If DEFAULT-FILENAME is omitted, the visited file name is used,
  but if INITIAL-INPUT is specified, that combined with DIR is used.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can only happen if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)
Fourth arg REQUIRE-MATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL-INPUT specifies text to start with.
If optional sixth arg PREDICATE is non-nil, possible completions and
 the resulting file name must satisfy `(funcall predicate NAME)'.
 This argument is only available starting with Emacs 22.
DIR should be an absolute directory name.  It defaults to the value of
`default-directory'.

Non-nil `icicle-init-value-flag' means that if DEFAULT-FILENAME is
non-nil and INITIAL-INPUT is nil or \"\", then DEFAULT-FILENAME is
inserted in the minibuffer as the INITIAL-INPUT.  The particular
non-nil value determines whether or not the value is preselected and
where the cursor is left (at the beginning or end of the value).

If option `icicle-require-match-flag' is non-nil, it overrides the
value of REQUIRE-MATCH.

Cycling into subdirectories is determined by option
`icicle-cycle-into-subdirs-flag'.  Case is ignored if
`read-file-name-completion-ignore-case' is non-nil.  See also
`read-file-name-function'.

If option `icicle-add-proxy-candidates-flag' is non-nil, then the
following proxy file-name candidates are included.  (This inclusion
can be toggled at any time from the minibuffer, using `C-M-_'.)

* `*mouse-2 file name*' - Click `mouse-2' on a file name to choose it.
* `*point file name*'   - Use the file name at point (cursor).
* Single-quoted file-name variables - Use the variable's value.

Candidates `*mouse-2 file name*' and `*point file name*' are available
only if library `ffap.el' can be loaded.  A file-name variable has
custom type `file' or (file :must-match t).

If this command was invoked with the mouse, use a file dialog box if
`use-dialog-box' is non-nil, and the window system or X toolkit in use
provides a file dialog box.

Removes *Completions* window when done."
  (unwind-protect
       (let* ((ffap-available-p (or (require 'ffap- nil t) (require 'ffap nil t)))
              ;; The next four prevent slowing down `ffap-guesser'.
              (ffap-alist nil) (ffap-machine-p-known 'accept)
              (ffap-url-regexp nil) (ffap-shell-prompt-regexp nil) 
              (fap (if (and (eq major-mode 'dired-mode) (fboundp 'dired-get-file-for-visit))
                       (condition-case nil
                           (abbreviate-file-name (dired-get-file-for-visit))
                         (error nil))
                     (and ffap-available-p (ffap-guesser))))
              (mouse-file "*mouse-2 file name*")
              (icicle-proxy-candidates
               (append (and fap (list "*point file name*"))
                       (and ffap-available-p (list mouse-file))
                       (let ((ipc ()))
                         (mapatoms
                          (lambda (cand)
                            (when (and (user-variable-p cand)
                                       (icicle-var-is-of-type-p
                                        cand '(file (file :must-match t))))
                              (push (concat "'" (symbol-name cand) "'") ipc))))
                         ipc)))
              temp result)
         (setq result (icicle-read-file-name-1
                       prompt dir default-filename require-match initial-input predicate))
         (when ffap-available-p
           (cond ((save-match-data (string-match "*point file name\\*$" result))
                  (setq result fap))
                 ((save-match-data (string-match "*mouse-2 file name\\*$" result))
                  (setq result
                        (progn (let ((e (read-event "Click `mouse-2' on file name")))
                                 (read-event) ; Get rid of mouse up event.
                                 (save-excursion
                                   (mouse-set-point e)
                                   (if (and (eq major-mode 'dired-mode)
                                            (fboundp 'dired-get-file-for-visit)) ; In `dired+.el'.
                                       (condition-case nil ; E.g. error: not on file line (ignore)
                                           (abbreviate-file-name (dired-get-file-for-visit))
                                         (error "No such file"))
                                     (or (ffap-guesser) (error "No such file"))))))))))
         (when (setq temp (member (file-name-nondirectory result) icicle-proxy-candidates))
           (setq result (symbol-value (intern (substring (car temp) 1 (1- (length (car temp))))))))
         result)
    (setq icicle-proxy-candidates nil)))

;;;###autoload
(defun icicle-read-file-name-1 (prompt &optional dir default-filename
                                require-match initial-input predicate)
  "Helper function for `icicle-read-file-name'."
  (setq icicle-initial-value (or initial-input
                                 (if (stringp icicle-initial-value) icicle-initial-value "")))
  (setq icicle-nb-of-other-cycle-candidates 0)
  (icicle-fix-default-directory) ; Make sure there are no backslashes in it.

  (unless (string= "" icicle-initial-value) (setq initial-input icicle-initial-value))

  ;; Maybe use DEFAULT-FILENAME for INITIAL-INPUT also, after removing the directory part.
  ;; Note that if DEFAULT-FILENAME is null, then we let INITIAL-INPUT remain null too.
  (when (and icicle-init-value-flag default-filename (string= "" icicle-initial-value))
    (setq initial-input (file-name-nondirectory default-filename)))

  ;; Override REQUIRE-MATCH as needed.
  (setq require-match (case icicle-require-match-flag
                        ((nil) require-match)
                        (no-match-required nil)
                        (partial-match-ok t)
                        (full-match-required 'full-match-required))
        icicle-require-match-p require-match)
  (icicle-highlight-lighter)
  (let ((read-file-name-function nil)
        result)
    (cond ((not icicle-mode)
           (setq icicle-prompt prompt)
           (condition-case nil      ; If Emacs 22+, use predicate arg.
               (setq result (funcall (or icicle-old-read-file-name-fn 'read-file-name)
                                     icicle-prompt dir default-filename
                                     require-match initial-input predicate))
             (wrong-number-of-arguments
              (setq result (funcall (or icicle-old-read-file-name-fn 'read-file-name)
                                    icicle-prompt dir default-filename
                                    require-match initial-input)))))
          (t
           (setq icicle-prompt
                 (if (fboundp 'propertize) ; Emacs 21+ only
                     (concat (propertize
                              (if icicle-candidate-action-fn "+" ".")
                              'face
                              (cond ((and icicle-candidate-action-fn icicle-require-match-p)
                                     '(icicle-multi-command-completion
                                       icicle-mustmatch-completion))
                                    (icicle-candidate-action-fn 'icicle-multi-command-completion)
                                    (icicle-require-match-p
                                     '(icicle-completion
                                       icicle-mustmatch-completion))
                                    (t 'icicle-completion)))
                             " "
                             (propertize prompt 'face 'minibuffer-prompt))
                   (concat (and icicle-candidate-action-fn "+ ") prompt)))
           (let ((minibuffer-prompt-properties
                  (and (boundp 'minibuffer-prompt-properties) ; Emacs 21+ only
                       (icicle-remove-property 'face minibuffer-prompt-properties))))
             (condition-case nil    ; If Emacs 22+, use predicate arg.
                 (setq result
                       (catch 'icicle-read-top
                         (funcall (or icicle-old-read-file-name-fn 'read-file-name)
                                  icicle-prompt dir default-filename
                                  require-match initial-input predicate)))
               (wrong-number-of-arguments
                (setq result
                      (catch 'icicle-read-top
                        (funcall (or icicle-old-read-file-name-fn 'read-file-name)
                                 icicle-prompt dir default-filename
                                 require-match initial-input))))))))
    ;; HACK.  Without this, when REQUIRE-MATCH is non-nil, *Completions* window
    ;; does not disappear.
    (when require-match (icicle-remove-Completions-window))
    result))

(defun icicle-fix-default-directory ()
  "Convert backslashes in `default-directory' to slashes."
  ;; This is a hack.  If you do `C-x 4 f' from a standalone minibuffer
  ;; frame, `default-directory' on MS Windows has this form:
  ;; `C:\some-dir/'.  There is a backslash character in the string.  This
  ;; is not a problem for standard Emacs, but it is a problem for Icicles,
  ;; because we interpret backslashes using regexp syntax - they are not
  ;; file separators for Icicles.  So, we call `substitute-in-file-name' to
  ;; change all backslashes in `default-directory' to slashes.  This
  ;; shouldn't hurt, because `default-directory' is an absolute directory
  ;; name - it doesn't contain environment variables.  For example, we
  ;; convert `C:\some-dir/' to `c:/some-directory/'."
  (setq default-directory (icicle-abbreviate-or-expand-file-name
                           (substitute-in-file-name default-directory))))

(defun icicle-remove-property (prop plist)
  "Remove property PROP from property-list PLIST, non-destructively.
Returns the modified copy of PLIST."
  (let ((cpy plist)
        (result nil))
    (while cpy
      (unless (eq prop (car cpy)) (setq result `(,(cadr cpy) ,(car cpy) ,@result)))
      (setq cpy (cddr cpy)))
    (nreverse result)))


;;; REPLACE ORIGINAL `read-from-minibuffer' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Respect `icicle-init-value-flag'.
;;;
;;; We use M@R%M=X!L$S+P&L^T*Z instead of HIST, to avoid name capture by
;;; `minibuffer-history-variable's value.  If we didn't need to be Emacs 20-compatible, then we
;;; could employ `#1=#:hist'...`#1#'...`#1' read syntax to use an uninterned symbol.
;;;
(or (fboundp 'old-read-from-minibuffer)
(fset 'old-read-from-minibuffer (symbol-function 'read-from-minibuffer)))

;;;###autoload
(defun icicle-read-from-minibuffer (prompt &optional initial-contents keymap read
                                    m@r%m=x!l$s+p&l^t*z default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.
The optional second arg INITIAL-CONTENTS is an alternative to
  DEFAULT-VALUE.  Vanilla Emacs considers it to be obsolete, but
  Icicles does not.  It is discussed in more detail below.
Third arg KEYMAP is a keymap to use while reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a Lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HIST, if non-nil, specifies a history list and optionally
  the initial position in the list.  It can be a symbol, which is the
  history list variable to use, or it can be a cons cell
  (HISTVAR . HISTPOS).  In that case, HISTVAR is the history list variable
  to use, and HISTPOS is the initial position for use by the minibuffer
  history commands.  For consistency, you should also specify that
  element of the history as the value of INITIAL-CONTENTS.  Positions
  are counted starting from 1 at the beginning of the list.
Sixth arg DEFAULT-VALUE is the default value.  If non-nil, it is available
  for history commands; but, unless READ is non-nil, `read-from-minibuffer'
  does NOT return DEFAULT-VALUE if the user enters empty input!  It returns
  the empty string.
Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'.
Eighth arg KEEP-ALL, if non-nil, says to put all inputs in the history list,
 even empty or duplicate inputs.  This is available starting with Emacs 22.
If the variable `minibuffer-allow-text-properties' is non-nil,
 then the string which is returned includes whatever text properties
 were present in the minibuffer.  Otherwise the value has no text properties.

Non-nil `icicle-init-value-flag' means that if DEFAULT-VALUE is
non-nil and INITIAL-CONTENTS is nil or \"\", then DEFAULT-VALUE (or
its string value, if it is a character) is inserted in the minibuffer
as the INITIAL-CONTENTS.  The particular non-nil value determines
whether or not the value is preselected and where the cursor is left
\(at the beginning or end of the value).

The remainder of this documentation string describes the
INITIAL-CONTENTS argument in more detail.  If non-nil,
INITIAL-CONTENTS is a string to be inserted into the minibuffer before
reading input.  Normally, point is put at the end of that string.
However, if INITIAL-CONTENTS is (STRING . POSITION), the initial input
is STRING, but point is placed at one-indexed position POSITION in the
minibuffer.  Any integer value less than or equal to one puts point at
the beginning of the string.  *Note* that this behavior differs from
the way such arguments are used in `completing-read' and some related
functions, which use zero-indexing for POSITION."
  (unless initial-contents (setq initial-contents ""))
  ;; Maybe use DEFAULT-VALUE for INITIAL-CONTENTS also.
  (when (and icicle-init-value-flag default-value (stringp initial-contents)
             (string= "" initial-contents))
    (setq initial-contents (if (integerp default-value) ; Character
                               (char-to-string default-value)
                             default-value)))
  (old-read-from-minibuffer
   prompt initial-contents keymap read m@r%m=x!l$s+p&l^t*z default-value inherit-input-method))


;;; REPLACE ORIGINAL `read-number' defined in `subr.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;; 1. Let user enter a numeric variable name, for its value.  Allow completion.
;;; 2. Allow for `replace-regexp-in-string' not being defined.
;;; 3. Allow for error reading input.
;;; 4. Call `ding' if not a number, and don't redisplay for `sit-for'.
;;;
(when (fboundp 'read-number)
(or (fboundp 'old-read-number)
(fset 'old-read-number (symbol-function 'read-number)))

(defun icicle-read-number (prompt &optional default)
  "Read a number in the minibuffer, prompting with PROMPT (a string).
DEFAULT is returned if the user hits `RET' without typing anything.

If option `icicle-add-proxy-candidates-flag' is non-nil, the user can
also enter the name of a numeric variable - its value is returned.
Completion is available for this.  A numeric variable is a variable
whose value or whose custom type is compatible with type `integer',
`number', or `float'."
  (unwind-protect
       (let ((num nil)
             (icicle-proxy-candidates
              (and icicle-add-proxy-candidates-flag
                   (let ((ipc ()))
                     (mapatoms
                      (lambda (cand)
                        (when (and (user-variable-p cand)
                                   (icicle-var-is-of-type-p cand (if (>= emacs-major-version 22)
                                                                     '(number integer float)
                                                                   '(number integer))))
                          (push (symbol-name cand) ipc))))
                     ipc))))
         (when default
           (save-match-data
             (setq prompt (cond ((string-match "\\(\\):[ \t]*\\'" prompt)
                                 (replace-match (format " (default %s)" default) t t prompt 1))
                                ((fboundp 'replace-regexp-in-string)
                                 (replace-regexp-in-string
                                  "[ \t]*\\'" (format " (default %s) " default) prompt t t))
                                (t prompt)))))
         (while (progn
                  (let ((str (completing-read prompt nil nil nil nil nil
                                              (and default (number-to-string default))))
                        temp)
                    (setq num (cond ((zerop (length str)) default)
                                    ((setq temp (member str icicle-proxy-candidates))
                                     (symbol-value (intern (car temp))))
                                    ((stringp str) (condition-case nil (read str) (error nil))))))
                  (unless (numberp num)
                    (ding) (message "Not a number.  Try again.") (sit-for 0.5 nil t)
                    t)))
         num)
    (setq icicle-proxy-candidates nil)))
)

;; Can't replace standard `read-char-exclusive' with this, because, starting with Emacs 22, it has
;; an optional SECONDS arg that cannot be simulated using `completing-read'.
(defun icicle-read-char-exclusive (prompt &optional inherit-input-method)
  "Read a character in the minibuffer, prompting with PROMPT (a string).
It is returned as a number.
Optional arg INHERIT-INPUT-METHOD is as for `completing-read'.

If option `icicle-add-proxy-candidates-flag' is non-nil, the user can
also enter the name of a character variable - its value is returned.
Completion is available for this.  A character variable is a variable
whose value is compatible with type `character'."
  (unwind-protect
       (let* ((char nil)
              (icicle-proxy-candidates
               (and icicle-add-proxy-candidates-flag
                    (let ((ipc ()))
                      (mapatoms (lambda (cand)
                                  (when (and (user-variable-p cand)
                                             (icicle-var-is-of-type-p cand '(character)))
                                    (push (symbol-name cand) ipc))))
                      ipc)))
              (str (completing-read prompt nil nil nil nil nil nil inherit-input-method))
              temp)
         (setq char (cond ((zerop (length str)) (error "No character read"))
                          ((setq temp (member str icicle-proxy-candidates))
                           (symbol-value (intern (car temp))))
                          ((stringp str)
                           (condition-case nil
                               (progn (when (> (length str) 1)
                                        (message "First char is used: `%c'" (elt str 0)) (sit-for 2))
                                      (elt str 0))
                             (error nil)))))
         char)
    (setq icicle-proxy-candidates nil)))

(defun icicle-read-string-completing (prompt &optional default pred hist)
  "Read a string in the minibuffer, prompting with PROMPT (a string).
If the user hits `RET' without typing anything, return DEFAULT, or \"\"
  if DEFAULT is nil.
PRED is a predicate that filters the variables available for completion.
HIST is the history list to use, as for `completing-read'.

If option `icicle-add-proxy-candidates-flag' is non-nil, the user can
also enter the name of a string variable - its value is returned.
Completion is available for this.  A string variable is a variable
whose value or whose custom type is compatible with type `string'."
  (unwind-protect
       (let ((strg nil)
             (icicle-proxy-candidates
              (and icicle-add-proxy-candidates-flag
                   (let ((ipc ()))
                     (mapatoms (lambda (cand)
                                 (when (and (user-variable-p cand)
                                            (icicle-var-is-of-type-p cand '(string color regexp)))
                                   (push (symbol-name cand) ipc))))
                     ipc))))
         (when default
           (save-match-data 
             (setq prompt (cond ((string-match "\\(\\):[ \t]*\\'" prompt)
                                 (replace-match (format " (default %s)" default) t t prompt 1))
                                ((fboundp 'replace-regexp-in-string)
                                 (replace-regexp-in-string
                                  "[ \t]*\\'" (format " (default %s) " default) prompt t t))
                                (t prompt)))))
         (let ((strg-read (completing-read
                           prompt nil pred nil
                           (and (consp hist) (nth (cdr hist) (symbol-value (car hist))))
                           hist default))
               temp)
           (setq strg (cond ((zerop (length strg-read)) (or default ""))
                            ((setq temp (member strg-read icicle-proxy-candidates))
                             (setq temp (symbol-value (intern (car temp))))
                             (cond ((and (symbolp hist) (consp (symbol-value hist)))
                                    (setcar (symbol-value hist) temp))
                                   ((and (consp hist) (symbolp (car hist))
                                         (consp (symbol-value (car hist))))
                                    (setcar (symbol-value (car hist)) temp)))
                             temp)
                            (t strg-read))))
         strg)
    (setq icicle-proxy-candidates nil)))

;; Same as `help-var-is-of-type-p'.
(defun icicle-var-is-of-type-p (variable types &optional mode)
  "Return non-nil if VARIABLE satisfies one of the custom types in TYPES.
TYPES is a list of `defcustom' type sexps or a list of regexp strings.
TYPES are matched, in order, against VARIABLE's type definition or
VARIABLE's current value, until one is satisfied or all are tried.

If TYPES is a list of regexps, then each is regexp-matched against
VARIABLE's custom type.

Otherwise, TYPES is a list of type sexps, each of which is a
definition acceptable for `defcustom' :type or the first symbol of
such a definition (e.g. `choice').  In this case, two kinds of type
comparison are possible:

1. VARIABLE's custom type, or its first symbol, is matched using
  `equal' against each type in TYPES.

2. VARIABLE's current value is checked against each type in TYPES to
   see if it satisfies one of them.  In this case, VARIABLE's own type
   is not used; VARIABLE might not even be typed - it could be a
   variable not defined using `defcustom'.

For any of the comparisons against VARIABLE's type, either that type
can be checked directly or its supertypes (inherited types) can also
be checked.

These different type-checking possibilities depend on the value of
argument MODE, as follows, and they determine the meaning of the
returned value:

`direct':   VARIABLE's type matches a member of list TYPES
`inherit':  VARIABLE's type matches or is a subtype of a TYPES member
`value':    VARIABLE is bound, and its value satisfies a type in TYPES
`inherit-or-value': `inherit' or `value', tested in that order
`direct-or-value':  `direct' or `value', tested in that order
anything else (default): `inherit'

VARIABLE's current value cannot satisfy a regexp type: it is
impossible to know which concrete types a value must match."
  (case mode
    ((nil inherit)     (icicle-var-inherits-type-p variable types))
    (inherit-or-value  (or (icicle-var-inherits-type-p variable types)
                           (icicle-var-val-satisfies-type-p variable types)))
    (value             (icicle-var-val-satisfies-type-p variable types))
    (direct            (icicle-var-matches-type-p variable types))
    (direct-or-value   (or (member (get variable 'custom-type) types)
                           (icicle-var-val-satisfies-type-p variable types)))
    (otherwise         (icicle-var-inherits-type-p variable types))))

(defun icicle-var-matches-type-p (variable types)
  "VARIABLE's type matches a member of TYPES."
  (catch 'icicle-type-matches
    (let ((var-type (get variable 'custom-type)))
      (dolist (type types)
        (when (if (stringp type)
                  (save-match-data
                    (string-match type (format "%s" (format "%S" var-type))))
                (equal var-type type))
          (throw 'icicle-type-matches t))))
    nil))

(defun icicle-var-inherits-type-p (variable types)
  "VARIABLE's type matches or is a subtype of a member of list TYPES."
  (catch 'icicle-type-inherits
    (let ((var-type (get variable 'custom-type)))
      (dolist (type types)
        (while var-type
          (when (or (and (stringp type)
                         (save-match-data (string-match type (format "%s" (format "%S" var-type)))))
                    (equal type var-type))
            (throw 'icicle-type-inherits t))
          (when (consp var-type) (setq var-type (car var-type)))
          (when (or (and (stringp type)
                         (save-match-data (string-match type (format "%s" (format "%S" var-type)))))
                    (equal type var-type))
            (throw 'icicle-type-inherits t))
          (setq var-type (car (get var-type 'widget-type))))
        (setq var-type (get variable 'custom-type))))
    nil))

(defun icicle-var-val-satisfies-type-p (variable types)
  "VARIABLE is bound, and its value satisfies a type in the list TYPES."
  (and (boundp variable)
       (let ((val (symbol-value variable)))
         (and (widget-convert (get variable 'custom-type))
              (icicle-value-satisfies-type-p val types)))))

(defun icicle-value-satisfies-type-p (value types)
  "Return non-nil if VALUE satisfies a type in the list TYPES."
  (catch 'icicle-type-value-satisfies
    (dolist (type types)
      (unless (stringp type)            ; Skip, for regexp type.
        (setq type (widget-convert type))
        ;; Satisfies if either :match or :validate.
        (when (condition-case nil
                  (progn (when (and (widget-get type :match)
                                    (widget-apply type :match value))
                           (throw 'icicle-type-value-satisfies t))
                         (when (and (widget-get type :validate)
                                    (progn (widget-put type :value value)
                                           (not (widget-apply type :validate))))
                           (throw 'icicle-type-value-satisfies t)))
                (error nil))
          (throw 'icicle-type-value-satisfies t))))
    nil))

(defun icicle-custom-type (variable)
  "Returns the `defcustom' type of VARIABLE.
Returns nil if VARIABLE is not a user option.

Note: If the library that defines VARIABLE has not yet been loaded,
then `icicle-custom-type' loads it.  Be sure you want to do that
before you call this function."
  (and (custom-variable-p variable)
       (or (get variable 'custom-type) (progn (custom-load-symbol variable)
                                              (get variable 'custom-type)))))


;;; REPLACE ORIGINAL `read-string' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Respect `icicle-init-value-flag'.
;;;
;;; We use M@R%M=X!L$S+P&L^T*Z instead of HISTORY, to avoid name capture by
;;; `minibuffer-history-variable's value.  If we didn't need to be Emacs 20-compatible, then we
;;; could employ `#1=#:hist'...`#1#'...`#1' read syntax to use an uninterned symbol.
;;;
(or (fboundp 'old-read-string)
(fset 'old-read-string (symbol-function 'read-string)))

;;;###autoload
(defun icicle-read-string (prompt &optional initial-input m@r%m=x!l$s+p&l^t*z
                           default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
  Vanilla Emacs considers it to be obsolete, but Icicles does not.  It
  behaves like argument INITIAL-CONTENTS in `read-from-minibuffer'.
  See the documentation string of `read-from-minibuffer' for details.
The third arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
  See `read-from-minibuffer' for details of HISTORY argument.
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history commands, and as the value to return if the user enters
 the empty string.
Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of enable-multibyte-characters."
  (let ((value (read-from-minibuffer prompt initial-input nil nil m@r%m=x!l$s+p&l^t*z
                                     default-value inherit-input-method)))
    (if (and default-value (equal value "")) default-value value)))


;;; REPLACE ORIGINAL `read-face-name' in `faces.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Show face names in *Completions* with the faces they name.
;;;
(or (fboundp 'old-read-face-name)
(fset 'old-read-face-name (symbol-function 'read-face-name)))

;;;###autoload
(cond ((< emacs-major-version 21)
       (defun icicle-read-face-name (prompt) ; Emacs 20
         "Read a face name with completion and return its face symbol.
PROMPT is the prompt.

If option `icicle-add-proxy-candidates-flag' is non-nil, the user can
also enter the name of a face-name variable - its value is returned.
If library `palette.el' or `eyedropper.el' is used, then the user can
also choose proxy candidate `*point face name*' to use the face at
point.  A face-name variable is a variable with custom-type `face'."
         (let ((icicle-list-nth-parts-join-string ": ")
               (icicle-list-join-string ": ")
               (icicle-list-end-string "")
               (icicle-list-use-nth-parts '(1))
               (icicle-proxy-candidates
                (and icicle-add-proxy-candidates-flag
                     (append
                      (and (fboundp 'eyedrop-face-at-point) (list "*point face name*"))
                      (let ((ipc ()))
                        (mapatoms
                         (lambda (cand)
                           (when (and (user-variable-p cand) (eq (get cand 'custom-type) 'face))
                             (push `,(concat "'" (symbol-name cand) "'") ipc))))
                        ipc))))
               face)
           (while (= (length face) 0)
             (setq face (icicle-transform-multi-completion
                         (completing-read prompt (mapcar #'icicle-make-face-candidate (face-list))
                                          nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
                                          (if (boundp 'face-name-history)
                                              'face-name-history
                                            'icicle-face-name-history)))))
           (let ((proxy (car (member face icicle-proxy-candidates))))
             (cond ((save-match-data (string-match "*point face name\\*$" face))
                    (eyedrop-face-at-point))
                   (proxy (symbol-value (intern (substring proxy 1 (1- (length proxy))))))
                   (t (intern face)))))))
      ((= emacs-major-version 21)       ; Emacs 21
       (defun icicle-read-face-name (prompt)
         "Read a face name with completion and return its face symbol.
PROMPT is the prompt.

If option `icicle-add-proxy-candidates-flag' is non-nil, the user can
also enter the name of a face-name variable - its value is returned.
If library `palette.el' or `eyedropper.el' is used, then the user can
also choose proxy candidate `*point face name*' to use the face at
point.  A face-name variable is a variable with custom-type `face'."
         (let ((icicle-list-nth-parts-join-string ": ")
               (icicle-list-join-string ": ")
               (icicle-list-end-string "")
               (icicle-list-use-nth-parts '(1))
               (icicle-proxy-candidates
                (and icicle-add-proxy-candidates-flag
                     (append
                      (and (fboundp 'eyedrop-face-at-point) (list "*point face name*"))
                      (let ((ipc ()))
                        (mapatoms
                         (lambda (cand)
                           (when (and (user-variable-p cand) (eq (get cand 'custom-type) 'face))
                             (push `,(concat "'" (symbol-name cand) "'") ipc))))
                        ipc))))
               (face-list (face-list))
               (def (thing-at-point 'symbol))
               face)
           (cond ((assoc def face-list)
                  (setq prompt (concat prompt " (default " def "): ")))
                 (t (setq def nil)
                    (setq prompt (concat prompt ": "))))
           (while (equal "" (setq face (icicle-transform-multi-completion
                                        (completing-read
                                         prompt (mapcar #'icicle-make-face-candidate face-list) nil
                                         (not (stringp icicle-WYSIWYG-Completions-flag)) nil
                                         (if (boundp 'face-name-history)
                                             'face-name-history
                                           'icicle-face-name-history)
                                         def)))))
           (let ((proxy (car (member face icicle-proxy-candidates))))
             (cond ((save-match-data (string-match "*point face name\\*$" face))
                    (eyedrop-face-at-point))
                   (proxy (symbol-value (intern (substring proxy 1 (1- (length proxy))))))
                   (t (intern face)))))))
      (t                                ; Emacs 22
       (defun icicle-read-face-name (prompt &optional string-describing-default multiple)
         "Read a face name with completion and return its face symbol
By default, use the face(s) on the character after point.  If that
character has the property `read-face-name', that overrides the `face'
property.

PROMPT should be a string that describes what the caller will do with the face;
  it should not end in a space.
STRING-DESCRIBING-DEFAULT should describe what default the caller will use if
  the user just types RET; you can omit it.
If MULTIPLE is non-nil, return a list of faces (possibly only one).
Otherwise, return a single face.

If option `icicle-add-proxy-candidates-flag' is non-nil, the user can
also enter the name of a face-name variable - its value is returned.
If library `palette.el' or `eyedropper.el' is used, then the user can
also choose proxy candidate `*point face name*' to use the face at
point.  A face-name variable is a variable with custom-type `face'."
         (let ((faceprop (or (get-char-property (point) 'read-face-name)
                             (get-char-property (point) 'face)))
               (aliasfaces nil)
               (nonaliasfaces nil)
               (icicle-proxy-candidates
                (and icicle-add-proxy-candidates-flag
                     (let ((ipc ()))
                       (mapatoms
                        (lambda (cand)
                          (when (and (user-variable-p cand) (eq (get cand 'custom-type) 'face))
                            (push `,(concat "'" (symbol-name cand) "'") ipc))))
                       ipc)))
               faces)
           ;; Undo Emacs 22 brain-dead treatment of PROMPT arg.
           (when (save-match-data (string-match ": $" prompt)) (setq prompt (substring prompt 0 -2)))
           ;; Try to get a face name from the buffer.
           (when (memq (intern-soft (thing-at-point 'symbol)) (face-list))
             (setq faces (list (intern-soft (thing-at-point 'symbol)))))
           ;; Add the named faces that the `face' property uses.
           (if (and (consp faceprop)
                    ;; Don't treat an attribute spec as a list of faces.
                    (not (keywordp (car faceprop)))
                    (not (memq (car faceprop) '(foreground-color background-color))))
               (dolist (f faceprop) (when (symbolp f) (push f faces)))
             (when (and faceprop (symbolp faceprop)) (push faceprop faces)))
           (delete-dups faces)
           (cond (multiple
                  ;; We leave this branch as is.  Emacs `completing-read-multiple' is not made for
                  ;; use with Icicles.
                  (require 'crm)
                  (mapatoms (lambda (s) (when (custom-facep s) ; Build up the completion tables.
                                          (if (get s 'face-alias)
                                              (push (symbol-name s) aliasfaces)
                                            (push (symbol-name s) nonaliasfaces)))))
                  (let* ((input (completing-read-multiple ; Read the input.
                                 (if (or faces string-describing-default)
                                     (format "%s (default %s): "
                                             prompt (if faces
                                                        (mapconcat 'symbol-name faces ",")
                                                      string-describing-default))
                                   (format "%s: " prompt))
                                 ;; This lambda expression is the expansion of Emacs 22 macro
                                 ;; (complete-in-turn nonaliasfaces aliasfaces).  We expand it here
                                 ;; So this can be compiled also in Emacs < 22 to work for Emacs 22.
                                 (lambda (string predicate mode)
                                   (cond ((eq mode t)
                                          (or (all-completions string nonaliasfaces predicate)
                                              (all-completions string aliasfaces predicate)))
                                         ((eq mode nil)
                                          (or (try-completion string nonaliasfaces predicate)
                                              (try-completion string aliasfaces predicate)))
                                         (t
                                          (or (test-completion string nonaliasfaces predicate)
                                              (test-completion string aliasfaces predicate)))))
                                 nil t nil (if (boundp 'face-name-history)
                                               'face-name-history
                                             'icicle-face-name-history)
                                 (and faces (mapconcat 'symbol-name faces ","))))
                         (output (cond ((or (equal input "") (equal input '(""))) ; Canonicalize.
                                        faces)
                                       ((stringp input)
                                        (mapcar 'intern (split-string input ", *" t)))
                                       ((listp input)
                                        (mapcar 'intern input))
                                       (input))))
                    output))            ; Return the list of faces
                 (t
                  (when (consp faces) (setq faces (list (car faces))))
                  (let ((icicle-list-nth-parts-join-string ": ")
                        (icicle-list-join-string ": ")
                        (icicle-list-end-string "")
                        (icicle-list-use-nth-parts '(1))
                        (face-list (face-list))
                        (def (if faces (mapconcat 'symbol-name faces ",") string-describing-default))
                        face)
                    (while (equal "" (setq face (icicle-transform-multi-completion
                                                 (completing-read
                                                  (if def
                                                      (format "%s (default %s): " prompt def)
                                                    (format "%s: " prompt))
                                                  (mapcar #'icicle-make-face-candidate face-list)
                                                  nil (not (stringp icicle-WYSIWYG-Completions-flag))
                                                  nil (if (boundp 'face-name-history)
                                                          'face-name-history
                                                        'icicle-face-name-history)
                                                  def)))))
                    (let ((proxy (car (member face icicle-proxy-candidates))))
                      (if proxy
                          (symbol-value (intern (substring proxy 1 (1- (length proxy)))))
                        (intern face))))))))))

(defun icicle-make-face-candidate (face)
  "Return a completion candidate for FACE.
The value of option `icicle-WYSIWYG-Completions-flag' determines the
kind of candidate to use.
 If nil, then the face name is used (a string).

 If a string, then a multi-completion candidate is used, with the face
 name followed by a sample swatch using FACE on the string's text.

 If t, then the candidate is the face name itself, propertized with
FACE."
  (if (stringp icicle-WYSIWYG-Completions-flag)
      (let ((swatch (copy-sequence icicle-WYSIWYG-Completions-flag)))
        (put-text-property 0 (length icicle-WYSIWYG-Completions-flag) 'face face swatch)
        (list (list (symbol-name face) swatch)))
    (let ((face-name (copy-sequence (symbol-name face))))
      (when icicle-WYSIWYG-Completions-flag
        (put-text-property 0 (length face-name) 'face face face-name))
      (list face-name))))


;;; REPLACE ORIGINAL `face-valid-attribute-values' in `faces.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Show color names in *Completions* with the (background) colors they name.
;;; This is really so that commands such as `modify-face' take advantage of colored candidates.
;;; We don't bother to try the same thing for Emacs 20, but the fix (directly to `modify-face') is
;;; similar and trivial.
;;;
(when (fboundp 'face-valid-attribute-values) ; Emacs 21 and 22.
  (or (fboundp 'old-face-valid-attribute-values)
      (fset 'old-face-valid-attribute-values (symbol-function 'face-valid-attribute-values)))

;;;###autoload
  (defun icicle-face-valid-attribute-values (attribute &optional frame)
    "Return valid values for face attribute ATTRIBUTE.
The optional argument FRAME is used to determine available fonts
and colors.  If it is nil or not specified, the selected frame is
used.  Value is an alist of (NAME . VALUE) if ATTRIBUTE expects a value
out of a set of discrete values.  Value is `integerp' if ATTRIBUTE expects
an integer value."
    (let ((valid
           (case attribute
             (:family (if window-system
                          (mapcar #'(lambda (x) (cons (car x) (car x)))
                                  (x-font-family-list))
                        ;; Only one font on TTYs.
                        (list (cons "default" "default"))))
             ((:width :weight :slant :inverse-video)
              (mapcar #'(lambda (x) (cons (symbol-name x) x))
                      (internal-lisp-face-attribute-values attribute)))
             ((:underline :overline :strike-through :box)
              (if window-system
                  (nconc (mapcar #'(lambda (x) (cons (symbol-name x) x))
                                 (internal-lisp-face-attribute-values attribute))
                         (mapcar #'(lambda (c) (cons c c))
                                 (mapcar #'icicle-color-name-w-bg (x-defined-colors frame))))
                (mapcar #'(lambda (x) (cons (symbol-name x) x))
                        (internal-lisp-face-attribute-values attribute))))
             ((:foreground :background)
              (mapcar #'(lambda (c) (cons c c))
                      (mapcar #'icicle-color-name-w-bg (x-defined-colors frame))))
             ((:height) 'integerp)
             (:stipple (and (memq window-system '(x w32 mac))
                            (mapcar #'list (apply #'nconc (mapcar (lambda (dir)
                                                                    (and (file-readable-p dir)
                                                                         (file-directory-p dir)
                                                                         (directory-files dir)))
                                                                  x-bitmap-file-path)))))
             (:inherit (cons '("none" . nil) (mapcar #'(lambda (c) (cons (symbol-name c) c))
                                                     (face-list))))
             (t
              (error "Internal error")))))
      (if (and (listp valid) (not (memq attribute '(:inherit))))
          (nconc (list (cons "unspecified" 'unspecified)) valid)
        valid)))

  (defun icicle-color-name-w-bg (color-name)
    "Return copy of string COLOR-NAME with its background of that color."
    (unless (featurep 'hexrgb) (error "`icicle-color-name-w-bg' requires library `hexrgb.el'"))
    (let ((propertized-name (copy-sequence color-name)))
      (put-text-property 0 (length propertized-name)
                         'face (cons 'background-color
                                     (hexrgb-color-name-to-hex color-name))
                         propertized-name)
      propertized-name)))
 
;;(@* "Icicles functions - completion display (not cycling)")

;;; Icicles functions - completion display (not cycling) -------------

(defun icicle-display-candidates-in-Completions (&optional reverse-p no-display-p)
  "Refresh the current set of completion candidates in *Completions*.
REVERSE-P non-nil means display the candidates in reverse order.
NO-DISPLAY-P means do not display the candidates; just recompute them."
  ;;$$   ;; Pred is special if `minibuffer-completion-table' is a function.
  ;;   (when (and (not (functionp minibuffer-completion-table))
  ;;              (functionp minibuffer-completion-predicate))
  ;;     (setq icicle-completion-candidates
  ;;           (icicle-delete-if-not
  ;;            (lambda (cand)
  ;;              (funcall minibuffer-completion-predicate
  ;;                       (if (arrayp minibuffer-completion-table) (intern cand) (list cand))))
  ;;            icicle-completion-candidates)))

  ;; $$$  (case icicle-incremental-completion-flag
  ;;     ((t always) (setq icicle-incremental-completion-p 'always))
  ;;     ((nil) (setq icicle-incremental-completion-p nil)))

  ;; Upgrade `icicle-incremental-completion-p' if we are redisplaying, so that completions will
  ;; be updated by `icicle-call-then-update-Completions' when you edit.
  (setq icicle-incremental-completion-p icicle-incremental-completion-flag)
  (when (and (eq t icicle-incremental-completion-p) (get-buffer-window "*Completions*" 0))
    (setq icicle-incremental-completion-p 'always))

  (cond (no-display-p
         (icicle-msg-maybe-in-minibuffer (format "Candidates updated (%s matching)"
                                                 icicle-current-completion-mode)))
        ((null icicle-completion-candidates)
         (save-selected-window (icicle-remove-Completions-window))
         (icicle-msg-maybe-in-minibuffer
          (if (eq 'apropos icicle-current-completion-mode)
              (let ((typ (car (rassq icicle-apropos-complete-match-fn
                                     icicle-apropos-match-fns-alist))))
                (concat "No " typ (and typ " ") "completions"))
            (if (and icicle-fuzzy-completion-flag (featurep 'fuzzy-match))
                "No fuzzy completions"
              "No prefix completions"))))
        (t
         (when (> (length icicle-completion-candidates) icicle-incremental-completion-threshold)
           (message "Displaying completion candidates..."))
         (with-output-to-temp-buffer "*Completions*"
           ;; `condition-case' shouldn't be needed, but it prevents an "End of buffer"
           ;; message from `display-completion-list' on Emacs 22.
           (condition-case nil
               (display-completion-list
                (if reverse-p (reverse icicle-completion-candidates) icicle-completion-candidates))
             (error nil)))
         (save-excursion
           (save-window-excursion
             (set-buffer (get-buffer "*Completions*"))
             (let ((buffer-read-only nil)
                   (eob (point-max))
                   (case-fold-search (if (and (icicle-file-name-input-p)
                                              (boundp 'read-file-name-completion-ignore-case))
                                         read-file-name-completion-ignore-case
                                       completion-ignore-case))
                   (dir (and (icicle-file-name-input-p) icicle-last-input
                             (file-name-directory icicle-last-input)))
                   (hist (and (symbolp minibuffer-history-variable)
                              (boundp minibuffer-history-variable)
                              (symbol-value minibuffer-history-variable))))
               (goto-char (icicle-start-of-candidates-in-Completions))
               (while (not (eobp))
                 (let* ((beg (point))
                        (end  (next-single-property-change beg 'mouse-face nil eob))
                        (next (next-single-property-change end 'mouse-face nil eob))
                        (faces nil))

                   ;; Highlight candidate specially if it is a proxy candidate.
                   (let ((candidate (icicle-current-completion-in-Completions)))
                     ;;$$$ (when dir (setq candidate (expand-file-name candidate dir)))
                     (when (member candidate icicle-proxy-candidates)
                       (setq faces (cons 'icicle-proxy-candidate faces))
                       (if (not icicle-proxy-candidate-regexp)
                           (add-text-properties beg end (cons 'face (list faces)))
                         (save-match-data
                           (when (string-match icicle-proxy-candidate-regexp candidate)
                             (add-text-properties (+ beg (match-beginning 0)) (+ beg (match-end 0))
                                                  (cons 'face (list faces))))))))

                   ;; Highlight candidate specially if it is a special candidate.
                   (let* ((candidate (icicle-current-completion-in-Completions))
                          (spec-prop (get (intern candidate) 'icicle-special-candidate)))
                     ;;$$$ (when dir (setq candidate (expand-file-name candidate dir)))
                     (save-match-data
                       (when (or (and icicle-special-candidate-regexp
                                      (string-match icicle-special-candidate-regexp candidate))
                                 spec-prop)
                         (if (consp spec-prop)
                             (add-text-properties beg end spec-prop)
                           (setq faces (cons 'icicle-special-candidate faces))
                           (if (not icicle-special-candidate-regexp)
                               (add-text-properties beg end (cons 'face (list faces)))
                             (add-text-properties (+ beg (match-beginning 0)) (+ beg (match-end 0))
                                                  (cons 'face (list faces))))))))

                   ;; Highlight candidate (`*-historical-candidate') if it has been used previously.
                   (when icicle-highlight-historical-candidates-flag
                     (let* ((candidate (icicle-current-completion-in-Completions)))
                       (when dir (setq candidate (expand-file-name candidate dir)))
                       (when (and (consp hist) (member candidate hist))
                         (add-text-properties
                          beg end `(face ,(setq faces (cons 'icicle-historical-candidate faces)))))))

                   ;; Highlight, inside the candidate, the expanded common match.
                   (when (and (or icicle-expand-input-to-common-match-flag
                                  (eq icicle-current-completion-mode 'prefix))
                              icicle-current-input (not (string= "" icicle-current-input)))
                     (save-excursion
                       (save-restriction
                         (narrow-to-region beg end) ; Restrict to the completion candidate.
                         (when (re-search-forward
                                (regexp-quote
                                 (if (icicle-file-name-input-p)
                                     (icicle-file-name-nondirectory icicle-current-input)
                                   icicle-current-input))
                                nil t)
                           (setq faces (cons 'icicle-common-match-highlight-Completions faces))
                           (put-text-property (match-beginning 0) (point) 'face faces)))))

                   ;; Highlight, inside the candidate, what the input expression matches.
                   (unless (and icicle-current-raw-input (string= "" icicle-current-raw-input)
                                icicle-apropos-complete-match-fn) ; Do nothing if no match fn.
                     (save-excursion
                       (save-restriction
                         (narrow-to-region beg end) ; Restrict to the completion candidate.
                         (let ((fn (if (and (eq 'prefix icicle-current-completion-mode)
                                            (not icicle-fuzzy-completion-flag))
                                       'search-forward
                                     (if (eq icicle-apropos-complete-match-fn 'icicle-scatter-match)
                                         (lambda (input bound noerror)
                                           (re-search-forward (icicle-scatter input) bound noerror))
                                       're-search-forward))))
                           (when (funcall fn (if (icicle-file-name-input-p)
                                                 (icicle-file-name-nondirectory
                                                  icicle-current-raw-input)
                                               icicle-current-raw-input)
                                          nil t)
                             (setq faces (cons 'icicle-match-highlight-Completions faces))
                             (put-text-property (match-beginning 0) (point) 'face faces))))))

                   ;; Highlight candidate if it has been saved.
                   (when icicle-saved-completion-candidates
                     (let ((candidate (icicle-current-completion-in-Completions)))
                       (when (member candidate icicle-saved-completion-candidates)
                         (let ((ov (make-overlay beg end (current-buffer))))
                           (push ov icicle-saved-candidate-overlays)
                           (overlay-put ov 'face 'icicle-saved-candidate)
                           (overlay-put ov 'priority '10)))))

                   ;; Treat `icicle-candidate-properties-alist'.
                   ;; A `face' prop will unfortunately wipe out any `face' prop we just applied.
                   (when icicle-candidate-properties-alist
                     (save-excursion
                       (save-restriction
                         (narrow-to-region beg end) ; Restrict to the completion candidate.
                         (let* ((candidate (icicle-current-completion-in-Completions))
                                (orig-pt (point))
                                (start 0)
                                (end 0)
                                (partnum 1)
                                (join (concat "\\(" icicle-list-join-string "\\|$\\)"))
                                (len (length candidate))
                                notfirst)
                           (save-match-data
                             (while (and (string-match join candidate
                                                       (if (and notfirst (= end (match-beginning 0))
                                                                (< end (length candidate)))
                                                           (1+ end)
                                                         end))
                                         (< end len))
                               (setq notfirst t)
                               (setq end (or (match-beginning 0) len))
                               (let* ((entry (assq partnum icicle-candidate-properties-alist))
                                      (properties (cadr entry))
                                      (propertize-join-string (caddr entry)))
                                 (when properties
                                   (add-text-properties
                                    (+ start orig-pt) (+ end orig-pt) properties))
                                 (when propertize-join-string
                                   (add-text-properties
                                    (+ end orig-pt) (+ end orig-pt (length icicle-list-join-string))
                                    properties)))
                               (setq partnum (1+ partnum) start (match-end 0))))))))
                   (goto-char next))))
             (set-buffer-modified-p nil)
             (setq buffer-read-only t)))
         (with-current-buffer (get-buffer "*Completions*")
           (goto-char (icicle-start-of-candidates-in-Completions))
           (set-window-point (get-buffer-window "*Completions*" 0) (point)))
         (message nil))))               ; Clear out any "Looking for..."


;;; REPLACE ORIGINAL `display-completion-list' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; 1. Does not remove text properties from candidates when it displays them in *Completions*.
;;; 2. Adjusts number of columns and their widths to window size.
;;; 3. The optional second arg is ignored.  In the vanilla Emacs version, this is a string
;;;    representing a common prefix, and faces `completions-first-difference' and
;;;    `completions-common-part' are used on candidates.
;;;
(or (fboundp 'old-display-completion-list)
(fset 'old-display-completion-list (symbol-function 'display-completion-list)))

;;;###autoload
(defun icicle-display-completion-list (completions &optional ignored)
  "Display the list of completions, COMPLETIONS, using `standard-output'.
Each element may be just a symbol or string or may be a list of two
strings to be printed as if concatenated.
If it is a list of two strings, the first is the actual completion
alternative, the second serves as annotation.
`standard-output' must be a buffer.
The actual completion alternatives, as inserted, are given the
`mouse-face' property of `highlight'.
At the end, this runs the normal hook `completion-setup-hook'.
It can find the completion buffer in `standard-output'.
The optional second arg is ignored."
  (if (not (bufferp standard-output))
      (let ((standard-output (current-buffer))) (icicle-display-completion-list completions))
    (with-current-buffer standard-output
      (goto-char (point-max))
      (when icicle-show-Completions-help-flag (icicle-insert-Completions-help-string))
      (let ((cand-intro-string (if completions
                                   "Possible completions are:\n"
                                 "There are no possible completions of what you have typed.")))
        (put-text-property 0 (length cand-intro-string) 'face 'icicle-Completions-instruction-1
                           cand-intro-string)
        (insert cand-intro-string))
      (icicle-insert-candidates completions))
    ;; In the vanilla version, the hook is run with `completion-common-substring' bound to
    ;; what is here called IGNORED.
    (run-hooks 'completion-setup-hook)
    nil))

(defun icicle-insert-candidates (candidates)
  "Insert completion candidates from list CANDIDATES into the current buffer."
  (when (consp candidates)
    (let* ((max-cand-len (apply #'max (mapcar (lambda (cand)
                                                (if (consp cand)
                                                    (+ (length (car cand)) (length (cadr cand)))
                                                  (length cand)))
                                              candidates)))
           (window (get-buffer-window (current-buffer) 0))
           (lru-win (get-lru-window))
           (wwidth (let ((spcl-frame-params (special-display-p (buffer-name))))
                     (cond ((and spcl-frame-params ; Special-buffer.  Use its default frame width.
                                 (or (and (consp spcl-frame-params)
                                          (cdr (assq 'width (cadr spcl-frame-params))))
                                     (cdr (assq 'width special-display-frame-alist))
                                     (cdr (assq 'width default-frame-alist)))))
                           (window (1- (window-width window))) ; Already displayed.  Use same width.
                           ((and lru-win (not (eq (selected-window) lru-win)))
                            (1- (window-width lru-win))) ; Use width of window that will be used.
                           (t icicle-Completions-window-default-width)))) ; Default.
           (columns (max 1 (min (/ (* 100 wwidth) (* icicle-candidate-width-factor max-cand-len))
                                (length candidates))))
           (colwidth (/ wwidth columns))
           (column-nb 0)
           startpos endpos string)
      (dolist (cand candidates)
        (setq endpos (point))
        (unless (bolp)
          (indent-to (* (max 1 column-nb) colwidth) icicle-inter-candidates-min-spaces)
          (when (< wwidth (+ (max colwidth (if (consp cand)
                                               (+ (length (car cand)) (length (cadr cand)))
                                             (length cand)))
                             (current-column)))
            (fixup-whitespace)
            (insert "\n")
            (setq column-nb columns)))  ; End of the row. Simulate being in farthest column.
        (when (< endpos (point)) (set-text-properties endpos (point) nil))
        ;; Convert candidate (but not annotation) to unibyte or to multibyte, if needed.
        (setq string (if (consp cand) (car cand) cand))
        (cond ((and (null enable-multibyte-characters) (multibyte-string-p string))
               (setq string (string-make-unibyte string)))
              ((and enable-multibyte-characters (not (multibyte-string-p string)))
               (setq string (string-make-multibyte string))))
        ;; Insert candidate (and annotation).  Mouse-face candidate, except for any newline as final
        ;; char.  This is so that candidates are visually separate in *Completions*.  Instead,
        ;; however, put property `icicle-keep-newline' on final \n, so
        ;; `icicle-mouse-choose-completion' and `icicle-current-completion-in-Completions' can put
        ;; the newline back as part of the candidate.
        (cond ((atom cand)              ; No annotation.
               (put-text-property (point) (progn (insert string)
                                                 (if (eq ?\n (char-before (point)))
                                                     (1- (point))
                                                   (point)))
                                  'mouse-face 'highlight)
               (when (eq ?\n (char-before (point)))
                 (put-text-property (1- (point)) (point) 'icicle-keep-newline t)))
              (t                        ; Candidate plus annotation.
               (put-text-property (point) (progn (insert string)
                                                 (if (eq ?\n (char-before (point)))
                                                     (1- (point))
                                                   (point)))
                                  'mouse-face 'highlight)
               (when (eq ?\n (char-before (point)))
                 (put-text-property (1- (point)) (point) 'icicle-keep-newline t))
               (set-text-properties (point) (progn (insert (cadr cand)) (point)) nil)))
        (setq column-nb (mod (1+ column-nb) columns))))))

(defun icicle-fit-Completions-window ()
  "Fit the window showing *Completions* to the buffer's contents.
Useful in `temp-buffer-show-hook'."
  (when (and (eq major-mode 'completion-list-mode) (fboundp 'fit-window-to-buffer))
    (let ((win (get-buffer-window "*Completions*")))
      (unless (< (window-width win) (frame-width)) ; Don't shrink if split horizontally.
        (fit-window-to-buffer
         win
         ;; Don't let it take over the frame, so we don't completely lose any other window.
         (min (- (frame-parameter (window-frame win) 'height) 8)
              (or (and (symbolp icicle-last-top-level-command)
                       (get icicle-last-top-level-command 'icicle-Completions-window-max-height))
                  icicle-Completions-window-max-height)))))))

(defun icicle-highlight-initial-whitespace (input)
  "Highlight any initial whitespace in your input.
Only if `icicle-highlight-input-initial-whitespace-flag' is non-nil.
INPUT is the current user input, that is, the completion root.
This must be called in the minibuffer."
  (when (and icicle-highlight-input-initial-whitespace-flag (not (string= "" input)))
    (let ((case-fold-search (if (and (icicle-file-name-input-p)
                                     (boundp 'read-file-name-completion-ignore-case))
                                read-file-name-completion-ignore-case
                              completion-ignore-case)))
      (save-excursion
        (goto-char (icicle-minibuffer-prompt-end))
        (when (and (icicle-file-name-input-p) insert-default-directory)
          (search-forward (icicle-file-name-directory-w-default input) nil t)) ; Skip directory.
        (save-excursion
          (save-restriction
            (narrow-to-region (point) (point-max)) ; Search within completion candidate.
            (while (and (not (eobp)) (looking-at "\\(\\s-\\|\n\\)+"))
              (put-text-property (point) (1+ (point)) 'face 'icicle-whitespace-highlight)
              (forward-char 1))
            ;; Remove any previous whitespace highlighting that is no longer part of prefix.
            (while (not (eobp))
              (when (eq (get-text-property (point) 'face) 'icicle-whitespace-highlight)
                (put-text-property (point) (1+ (point)) 'face nil))
              (forward-char 1))))))))

(defun icicle-minibuffer-prompt-end ()
  "Version of `minibuffer-prompt-end' that works for Emacs 20 and later."
  (if (fboundp 'minibuffer-prompt-end) (minibuffer-prompt-end) (point-min)))
 
;;(@* "Icicles functions - prefix completion cycling")

;;; Icicles functions - prefix (and fuzzy) completion cycling --------------------

(defun icicle-prefix-candidates (input)
  "List of prefix or fuzzy completions for the current partial INPUT.
INPUT is a string.  Each candidate is a string.
Non-nil `icicle-fuzzy-completion-flag' means use fuzzy matching for
non-file-name completion."
  (setq icicle-candidate-nb nil)
  (cond ((and icicle-fuzzy-completion-flag (featurep 'fuzzy-match))
         (condition-case nil
             (append icicle-extra-candidates icicle-proxy-candidates
                     (icicle-transform-candidates (icicle-fuzzy-candidates input)))
           (quit (top-level))))         ; Let `C-g' stop it.
        (icicle-sort-function (icicle-reversible-sort (icicle-unsorted-prefix-candidates input)))
        (t (icicle-unsorted-prefix-candidates input))))

(defun icicle-fuzzy-candidates (input)
  "Return `FM-all-fuzzy-matches' (fuzzy matches) for INPUT."
  (let ((candidates ()))
    ;; $$$$ Should treat other `minibuffer-completion-table' types also.
    (cond ((vectorp minibuffer-completion-table)
           (mapatoms (lambda (symb) (when (or (null minibuffer-completion-predicate)
                                              (funcall minibuffer-completion-predicate symb))
                                      (push (symbol-name symb) candidates)))
                     minibuffer-completion-table))
          ((and (consp minibuffer-completion-table) (consp (car minibuffer-completion-table)))
           (dolist (cand minibuffer-completion-table)
             (when (or (null minibuffer-completion-predicate)
                       (funcall minibuffer-completion-predicate cand))
               (push (car cand) candidates)))))
    (setq candidates (FM-all-fuzzy-matches input candidates))
    (when (consp candidates)
      (setq icicle-common-match-string (icicle-expanded-common-match input candidates)))
    candidates))

(defun icicle-unsorted-prefix-candidates (input)
  "Unsorted list of prefix completions for the current partial INPUT.
this also sets `icicle-common-match-string' to the expanded common
prefix over all candidates."
  (condition-case nil
      (let* ((candidates
              (icicle-transform-candidates (all-completions input minibuffer-completion-table
                                                            minibuffer-completion-predicate
                                                            icicle-ignore-space-prefix-flag)))
             (icicle-extra-candidates (icicle-delete-if-not
                                       (lambda (cand)
                                         (save-match-data
                                           (string-match (concat "^" (regexp-quote input)) cand)))
                                       icicle-extra-candidates))
             (icicle-proxy-candidates (icicle-delete-if-not
                                       (lambda (cand)
                                         (save-match-data
                                           (string-match (concat "^" (regexp-quote input)) cand)))
                                       icicle-proxy-candidates))
             (filtered-candidates
              (icicle-delete-if-not
               (lambda (cand)
                 (let ((case-fold-search completion-ignore-case)) (icicle-filter-wo-input cand)))
               (append icicle-extra-candidates icicle-proxy-candidates candidates))))
        (when (consp filtered-candidates)
          (let ((common-prefix (try-completion input minibuffer-completion-table
                                               minibuffer-completion-predicate)))
            (setq icicle-common-match-string (if (eq t common-prefix) input common-prefix))))
        filtered-candidates)
    (quit (top-level))))                ; Let `C-g' stop it.

(defun icicle-file-name-prefix-candidates (input)
  "List of prefix completions for partial file name INPUT.
INPUT is a string.
Candidates can be directories.  Each candidate is a string."
  (setq icicle-candidate-nb nil)
  (let ((default-directory (icicle-file-name-directory-w-default input)))
    (icicle-strip-ignored-files-and-sort
     (icicle-unsorted-file-name-prefix-candidates (or (icicle-file-name-nondirectory input) "")))))

(defun icicle-unsorted-file-name-prefix-candidates (input)
  "Unsorted list of prefix completions for the current file-name INPUT.
This also sets `icicle-common-match-string' to the expanded common
prefix over all candidates."
  (condition-case nil
      (let ((slashed-p (and (> (length input) 0) (eq ?/ (aref input 0)))))
        (when slashed-p (setq input (substring input 1)))
        (let* ((candidates (icicle-transform-candidates
                            (all-completions input minibuffer-completion-table
                                             (if slashed-p "/" default-directory)
                                             icicle-ignore-space-prefix-flag)))
               (filtered-candidates
                (icicle-delete-if-not
                 (lambda (cand)
                   (let ((case-fold-search
                          (if (boundp 'read-file-name-completion-ignore-case)
                              read-file-name-completion-ignore-case
                            completion-ignore-case)))
                     (if (member cand '("../" "./"))
                         (member input '(".." ".")) ; Prevent "" from matching "../"
                       (and (save-match-data (string-match (concat "^" (regexp-quote input)) cand))
                            (icicle-filter-wo-input cand)))))
                 (append icicle-extra-candidates icicle-proxy-candidates candidates))))
          (when (consp filtered-candidates)
            (let ((common-prefix (try-completion input minibuffer-completion-table
                                                 (if slashed-p "/" default-directory))))
              ;; If prefix matches an empty directory, use that directory as the sole completion.
              (when (and (stringp common-prefix)
                         (save-match-data (string-match "/\\.$" common-prefix)))
                (setq common-prefix (substring common-prefix 0 (- (length common-prefix) 2))))
              (setq icicle-common-match-string (if (eq t common-prefix) input common-prefix))))
          filtered-candidates))
    (quit (top-level))))                ; Let `C-g' stop it.
 
;;(@* "Icicles functions - apropos completion cycling")

;;; Icicles functions - apropos completion cycling -------------------

(defun icicle-apropos-candidates (input)
  "List of candidate apropos completions for the current partial INPUT.
INPUT is a string.  Each candidate is a string."
  (setq icicle-candidate-nb nil)
  (if icicle-sort-function
      (icicle-reversible-sort (icicle-unsorted-apropos-candidates input))
    (icicle-unsorted-apropos-candidates input)))

(defun icicle-unsorted-apropos-candidates (input)
  "Unsorted list of apropos completions for the current partial INPUT.
When `icicle-expand-input-to-common-match-flag' is non-nil, this also
sets `icicle-common-match-string' to the expanded common match of
input over all candidates."
  (condition-case nil
      (progn
        (when icicle-regexp-quote-flag (setq input (regexp-quote input)))
        (let* ((candidates (icicle-transform-candidates
                            (if (functionp minibuffer-completion-table)
                                (all-completions input minibuffer-completion-table
                                                 minibuffer-completion-predicate
                                                 icicle-ignore-space-prefix-flag)
                              (all-completions "" minibuffer-completion-table
                                             minibuffer-completion-predicate
                                             icicle-ignore-space-prefix-flag))))
               (filtered-candidates
                (icicle-delete-if-not
                 (lambda (cand)
                   (let ((case-fold-search completion-ignore-case))
                     (and (or (not icicle-apropos-complete-match-fn)
                              (funcall icicle-apropos-complete-match-fn input cand))
                          (icicle-filter-wo-input cand))))
                 (append icicle-extra-candidates icicle-proxy-candidates candidates))))
          (when (and icicle-expand-input-to-common-match-flag (consp filtered-candidates))
            (setq icicle-common-match-string (icicle-expanded-common-match input
                                                                           filtered-candidates)))
          filtered-candidates))         ; Return candidates.
    (quit (top-level))))                ; Let `C-g' stop it.

(defun icicle-file-name-apropos-candidates (input)
  "List of apropos completions for partial file-name INPUT.
INPUT is a string.
Candidates can be directories.  Each candidate is a string."
  (setq icicle-candidate-nb nil)
  (let ((default-directory (icicle-file-name-directory-w-default input)))
    (icicle-strip-ignored-files-and-sort
     (icicle-unsorted-file-name-apropos-candidates
      (or (icicle-file-name-nondirectory input) "")))))

(defun icicle-unsorted-file-name-apropos-candidates (input)
  "Unsorted list of apropos completions for the partial file-name INPUT.
When `icicle-expand-input-to-common-match-flag' is non-nil, this also
sets `icicle-common-match-string' to the expanded common match of
input over all candidates."
  (condition-case nil
      (progn
        (when icicle-regexp-quote-flag (setq input (regexp-quote input)))
        (let ((slashed-p (and (> (length input) 0) (eq ?/ (aref input 0)))))
          (when slashed-p (setq input (substring input 1)))
          (let* ((candidates (icicle-transform-candidates
                              (if (and (not (stringp minibuffer-completion-predicate))
                                       (functionp minibuffer-completion-table))
                                  (all-completions input minibuffer-completion-table
                                                   (if slashed-p "/" default-directory)
                                                   icicle-ignore-space-prefix-flag)
                                (all-completions "" minibuffer-completion-table
                                                 (if slashed-p "/" default-directory)
                                                 icicle-ignore-space-prefix-flag))))
                 (filtered-candidates
                  (append icicle-extra-candidates icicle-proxy-candidates
                          (icicle-delete-if-not
                           (lambda (cand)
                             (let ((case-fold-search
                                    (if (boundp 'read-file-name-completion-ignore-case)
                                        read-file-name-completion-ignore-case
                                      completion-ignore-case)))
                               (if (member cand '("../" "./"))
                                   (member input '(".." ".")) ; Prevent "" from matching "../"
                                 (and (or (not icicle-apropos-complete-match-fn)
                                          (funcall icicle-apropos-complete-match-fn input cand))
                                      (icicle-filter-wo-input cand)))))
                           candidates))))
            (when (and icicle-expand-input-to-common-match-flag (consp filtered-candidates))
              (setq icicle-common-match-string (icicle-expanded-common-match input
                                                                             filtered-candidates)))
            filtered-candidates)))      ; Return candidates.
    (quit (top-level))))                ; Let `C-g' stop it.

(defun icicle-expanded-common-match (input candidates)
  "Return the expanded common match for INPUT among all CANDIDATES.
This assumes that INPUT matches each string in list CANDIDATES.
Return nil if there is no common match.  This actually returns
`regexp-quote' applied to the expanded common match, so that special
characters in the match don't throw off regexp matching.

The expanded common match is typically, but not always, the longest
common match.  See the documentation, section `Expanded-Common-Match
Completion', for details."
  ;; Since `icicle-expanded-common-match-1' checks only the first match for a single candidate,
  ;; we call it twice, once using the first candidate and once using the second.
  ;; Typically, one of these tries will give us the longest common match.
  (let ((first-try (icicle-expanded-common-match-1 input candidates))
        (second-try nil))
    (when (and first-try (cadr candidates))
      (setq second-try (icicle-expanded-common-match-1
                        input (cons (cadr candidates) (cons (car candidates) (cddr candidates))))))
    (if (> (length second-try) (length first-try)) second-try first-try)))

(defun icicle-expanded-common-match-1 (input candidates)
  "Helper function for `icicle-expanded-common-match."
  ;; This does not always give a longest common match, because it looks only at the first match
  ;; of INPUT with the first candidate.  What it returns is the longest match that is common to
  ;; all CANDIDATES and also contains the first match in the first candidate.
  (let ((case-fold-search (if (and (icicle-file-name-input-p)
                                   (boundp 'read-file-name-completion-ignore-case))
                              read-file-name-completion-ignore-case
                            completion-ignore-case))
        (first (car candidates)))
    (and icicle-apropos-complete-match-fn ; Return nil if no match function.
         (save-match-data
           (funcall icicle-apropos-complete-match-fn input first)
           (let* ((len-first (length first))
                  (beg 0)
                  (end len-first)
                  (orig-match-beg (match-beginning 0))
                  (orig-match-end (match-end 0))
                  (ecm first)           ; "ecm" for "expanded common match".
                  (rest (cdr candidates))
                  beg-ecm beg-next)
             (if (= orig-match-beg end)
                 (setq ecm "")          ; INPUT was, for instance, "$" or "\\>$; return "".
               ;; Compare with the rest of the candidates, reducing as needed.
               (while (and rest ecm)
                 (funcall icicle-apropos-complete-match-fn input (car rest))
                 (setq beg-next (match-beginning 0))
                 ;; Remove any prefix that doesn't match some other candidate.
                 (while (and (< beg orig-match-beg)
                             (not (funcall icicle-apropos-complete-match-fn
                                           (regexp-quote (substring ecm 0 (- orig-match-end beg)))
                                           (car rest)))
                             (progn (setq beg-ecm (match-beginning 0))
                                    (>= beg-ecm beg-next)))
                   ;; Take a character off of the left.
                   (setq ecm (substring ecm 1)
                         beg (1+ beg)))
                 ;; Remove any suffix that doesn't match some other candidate.
                 (while (and (> end 0)
                             (not (funcall icicle-apropos-complete-match-fn
                                           (regexp-quote ecm) (car rest))))
                   ;; Take a character off of the right.
                   (setq ecm (substring ecm 0 (1- (length ecm)))
                         end (1- end)))
                 (unless (and (funcall icicle-apropos-complete-match-fn
                                       (regexp-quote ecm) (car rest))
                              (funcall icicle-apropos-complete-match-fn input ecm))
                   (setq ecm nil))      ; No possible expansion
                 (pop rest))
               ecm))))))

(defun icicle-scatter-match (string completion)
  "Returns non-nil if STRING scatter-matches COMPLETION.
This means that all of the characters in STRING are also in string
COMPLETION, in the same order, but perhaps scattered among other
characters.  For example, STRING = \"ure\" matches COMPLETION
\"curried\"."
  (string-match (icicle-scatter string) completion))


(defun icicle-scatter (string)
  "Returns a regexp that matches a scattered version of STRING.
The regexp will match any string that contains the characters in
STRING, in the same order, but possibly with other characters as well.
Returns, for example, \"a.*b.*c.*d\" for input string \"abcd\"."
  (if (> emacs-major-version 21)
      (mapconcat #'regexp-quote (split-string string "" t) ".*")
    (mapconcat #'regexp-quote (split-string string "") ".*")))
 
;;(@* "Icicles functions - common helper functions")

;;; Icicles functions - common helper functions ----------------------

;; Main cycling function - used by `icicle-next-prefix-candidate', `icicle-next-apropos-candidate'.
(defun icicle-next-candidate (nth candidates-fn &optional regexp-p)
  "Replace input by NTH next or previous completion for an input.
Default value of NTH is 1, meaning use the next completion.
Negative NTH means use a previous, not subsequent, completion.

CANDIDATES-FN is a function that returns the list of candidate
completions for its argument, the current partial input (a string).

Optional arg REGEXP-P non-nil means that CANDIDATES-FN uses regexp
matching. This is used to highlight the appropriate matching root."
  (let ((saved-last-input icicle-last-input)) ; For call to `icicle-recompute-candidates'.
    (unless (stringp icicle-last-completion-candidate)
      (setq icicle-last-completion-candidate icicle-initial-value))
    (setq nth (or nth 1))
    (setq icicle-current-input
          (if (icicle-file-name-input-p)
              (abbreviate-file-name (icicle-minibuffer-contents-from-minibuffer))
            (icicle-minibuffer-contents-from-minibuffer)))
    (unless (and (symbolp this-command) (get this-command 'icicle-apropos-cycling-command)
                 (or (and (symbolp last-command)
                          (get last-command 'icicle-apropos-cycling-command))
                     (memq last-command
                           '(icicle-candidate-action
                             icicle-remove-candidate icicle-mouse-remove-candidate
                             icicle-apropos-complete icicle-apropos-complete-no-display))))
      (setq icicle-common-match-string nil)) ; Don't use old one, in `icicle-save-or-restore-input'.
    (icicle-save-or-restore-input)
    (when (and (icicle-file-name-input-p) (icicle-file-directory-p icicle-current-input))
      (setq icicle-default-directory icicle-current-input))
    (icicle-recompute-candidates nth candidates-fn saved-last-input)
    (icicle-save-or-restore-input)      ; Again, based on updated `icicle-common-match-string'.
    (cond ((null icicle-completion-candidates)
           (save-selected-window (icicle-remove-Completions-window))
           (minibuffer-message "  [No completion]"))
          (t
           (icicle-clear-minibuffer)
           (let ((nb-cands (length icicle-completion-candidates))
                 (unit (if (wholenump nth) 1 -1))
                 next)
             ;; So `icomplete+' can append the number of other candidates to the minibuffer.
             (when icicle-completion-candidates
               (setq icicle-nb-of-other-cycle-candidates (1- nb-cands)))
             (icicle-increment-cand-nb+signal-end nth nb-cands)
             (setq next (elt icicle-completion-candidates icicle-candidate-nb))
             (while (null next)         ; Skip null candidates.
               (icicle-increment-cand-nb+signal-end unit nb-cands)
               (setq next (elt icicle-completion-candidates icicle-candidate-nb)))

             ;; Reset last candidate.  Need a copy, because we change its text properties.
             (setq icicle-last-completion-candidate (copy-sequence next))

             ;; Highlight any initial whitespace (probably a user typo).
             (let ((input (if regexp-p icicle-current-raw-input icicle-current-input)))
               (icicle-highlight-initial-whitespace input))

             ;; Underline the root that was completed, in the minibuffer.
             (let ((case-fold-search (if (and (icicle-file-name-input-p)
                                              (boundp 'read-file-name-completion-ignore-case))
                                         read-file-name-completion-ignore-case
                                       completion-ignore-case))
                   (inp (if (icicle-file-name-input-p)
                            (icicle-file-name-nondirectory icicle-current-input)
                          icicle-current-input))
                   indx)
               (unless regexp-p (setq inp (regexp-quote inp)))
               (save-match-data
                 (setq indx (string-match inp icicle-last-completion-candidate))
                 (when indx
                   (put-text-property indx (match-end 0) 'face 'icicle-match-highlight-minibuffer
                                      icicle-last-completion-candidate))))

             ;; Insert candidate in minibuffer.
             (insert (if (and (icicle-file-name-input-p) insert-default-directory)
                         (icicle-file-name-directory-w-default icicle-current-input)
                       "")
                     icicle-last-completion-candidate)
             (icicle-place-cursor icicle-current-input)

             ;; Highlight current completion candidate, if *Completions* is displayed.
             (when (get-buffer-window "*Completions*" 0)

               ;; Refresh *Completions*, updating it to reflect the current candidates.
               (unless (or (and (symbolp this-command)
                                (get this-command 'icicle-apropos-cycling-command)
                                (or (and (symbolp last-command)
                                         (get last-command 'icicle-apropos-cycling-command))
                                    (memq last-command '(icicle-candidate-action
                                                         icicle-remove-candidate
                                                         icicle-mouse-remove-candidate))))
                           (and (symbolp this-command)
                                (get this-command 'icicle-prefix-cycling-command)
                                (or (and (symbolp last-command)
                                         (get last-command 'icicle-prefix-cycling-command))
                                    (memq last-command '(icicle-candidate-action
                                                         icicle-remove-candidate
                                                         icicle-mouse-remove-candidate)))))
                 (icicle-display-candidates-in-Completions))
               (save-selected-window
                 (select-window (get-buffer-window "*Completions*" 'visible))
                 (if (fboundp 'only-raise-frame) (only-raise-frame) (raise-frame)))
               (icicle-highlight-candidate-in-Completions)))))))

(defun icicle-recompute-candidates (nth candidates-fn saved-last-input)
  "Recompute `icicle-completion-candidates', if needed.
If buffer *Completions* is already displayed, it is updated.
This does nothing, unless the user changed the minibuffer input or the
completion type has changed (from apropos to prefix or vice versa).
NTH < 0 means candidate order is reversed in *Completions*.
Argument CANDIDATES-FN is a function that recomputes the candidates.
SAVED-LAST-INPUT is the last input, as in `icicle-last-input'."
  (unless (and icicle-last-completion-command
               (string= icicle-current-input saved-last-input) ; No change in user input.
               ;; No change in completion type: apropos vs prefix.
               (or (and (memq icicle-last-completion-command
                              '(icicle-apropos-complete icicle-candidate-set-complement
                                icicle-mouse-remove-candidate icicle-keep-only-past-inputs))
                        (or (eq this-command 'icicle-apropos-complete)
                            (and (symbolp this-command)
                                 (get this-command 'icicle-apropos-cycling-command))))
                   (and (memq icicle-last-completion-command
                              '(icicle-prefix-complete icicle-candidate-set-complement
                                icicle-mouse-remove-candidate icicle-keep-only-past-inputs))
                        (or (eq this-command 'icicle-prefix-complete)
                            (and (symbolp this-command)
                                 (get this-command 'icicle-prefix-cycling-command))))))
    ;; Set `icicle-last-completion-command', to record new completion type.
    (cond ((and (symbolp this-command) (get this-command 'icicle-prefix-cycling-command))
           (setq icicle-last-completion-command 'icicle-prefix-complete))
          ((and (symbolp this-command) (get this-command 'icicle-apropos-cycling-command))
           (setq icicle-last-completion-command 'icicle-apropos-complete)))

    ;; Recompute and redisplay completion candidates.  Reset candidate number.
    (setq icicle-completion-candidates
          (condition-case nil
              (funcall candidates-fn icicle-current-input)
            (error icicle-completion-candidates))) ; No change if completion error.
    (when (get-buffer-window "*Completions*" 0) ; Update *Completions* display or remove it.
      (if icicle-completion-candidates
          (icicle-display-candidates-in-Completions (not (wholenump nth)))
        (save-selected-window (icicle-remove-Completions-window))))))

(defun icicle-save-or-restore-input ()
  "Save the current minibuffer input, or restore the last input.
If this is a cycling command, and there is a previous input, and the
  current input differs from the last cycling candidate (so the user
  has edited it), then restore the last input.  Cycled completions
  don't count as input.
Otherwise, save the current input for use by `C-l', and then compute
  the expanded common match.

There are several particular cases that modulate the behavior - see
the code."
  (let* ((prev-inputs-var (if (icicle-file-name-input-p)
                              'icicle-previous-raw-file-name-inputs
                            'icicle-previous-raw-non-file-name-inputs))
         (prev-inputs (symbol-value prev-inputs-var)))
    (cond
      ;; Restore the last input, provided there is some to restore and this a cycling command.
      ((and icicle-last-input
            (symbolp this-command) (get this-command 'icicle-cycling-command)
            icicle-last-completion-candidate
            ;; Current input = last completion candidate?
            (string= (if (icicle-file-name-input-p)
                         (directory-file-name (icicle-remove-dots icicle-last-completion-candidate))
                       icicle-last-completion-candidate)
                     (if (icicle-file-name-input-p)
                         (if icicle-cycle-into-subdirs-flag
                             (icicle-file-name-nondirectory icicle-current-input)
                           (file-name-nondirectory
                            (directory-file-name (icicle-remove-dots icicle-current-input))))
                       icicle-current-input)))
       (setq icicle-current-input icicle-last-input)) ; Return `icicle-current-input'.
      (t
       (cond
         ;; Save the current input for `C-l', then update it to the expanded common match.
         ;; DON'T do this if:
         ;;      the user doesn't want to use the expanded common match
         ;;   or there is no common match string
         ;;   or the last command was a cycling command
         ;;   or the input and the completion mode have not changed
         ;;      (so saved regexp will not be overwritten).
         ((not (or (and (not icicle-expand-input-to-common-match-flag)
                        (eq icicle-current-completion-mode 'apropos))
                   (not icicle-common-match-string)
                   (and (symbolp last-command) (get last-command 'icicle-cycling-command))
                   (and (equal icicle-last-input icicle-current-input)
                        (eq icicle-current-completion-mode
                            (case icicle-last-completion-command
                              ((icicle-prefix-complete icicle-prefix-complete-no-display
                                                       icicle-prefix-word-complete)
                               'prefix)
                              (t 'apropos))))))

          ;; Expand current input to expanded common match, after saving it for `C-l'.
          (let ((common (if (and (icicle-file-name-input-p) insert-default-directory)
                            (if (string= "" icicle-common-match-string)
                                (or (file-name-directory icicle-current-input) "")
                              (directory-file-name (icicle-abbreviate-or-expand-file-name
                                                    icicle-common-match-string
                                                    (file-name-directory icicle-current-input))))
                          icicle-common-match-string)))

            ;; Save current input for `C-l', then save common match as current input.
            ;; Don't do anything if we're ignoring letter case and that is the only difference
            ;; between the common match and the input (e.g. MS Windows file names).
            (unless (and case-fold-search (string= (upcase icicle-current-input) (upcase common))
                         (not (string= icicle-current-input common)))

              ;; Save input for `C-l' if this is not a cycling command, `C-l', or `C-L'.
              ;; Save it also if this is the first cycling command, or the first after completion.
              (unless (or (and (symbolp this-command) (get this-command 'icicle-cycling-command)
                               (or icicle-candidate-nb ; Not the first cycling command.
                                   (and (symbolp last-command)
                                        (get last-command 'icicle-completing-command))))
                          (memq this-command
                                '(icicle-retrieve-previous-input icicle-retrieve-next-input)))
                (setq icicle-current-raw-input icicle-current-input)
                ;; Save it for `C-l'.  Drop old entries when too big.
                (set prev-inputs-var (icicle-put-at-head prev-inputs-var icicle-current-raw-input))
                (when (> (length prev-inputs) icicle-completion-history-max-length)
                  (setcdr (nthcdr (1- icicle-completion-history-max-length) prev-inputs) nil)))

              ;; Save expanded common match as current input, unless input is a directory.
              (unless (and (icicle-file-name-input-p) (file-directory-p icicle-current-input))
                (setq icicle-current-input common)))))

         ;; Save input for `C-l'.
         ;; Don't do this if:
         ;;      this command or last command was a cycling command
         ;;   or this command is the same as last command
         ;;   or this command is `C-l' or `C-L'.
         ((not (or (and (symbolp last-command) (get last-command 'icicle-cycling-command))
                   (and (symbolp this-command) (get this-command 'icicle-cycling-command))
                   ;;$$$ (and (symbolp last-command) (get last-command 'icicle-completing-command))
                   (eq last-command this-command)
                   (memq this-command '(icicle-retrieve-previous-input icicle-retrieve-next-input))))
          (setq icicle-current-raw-input icicle-current-input)
          ;; Save it for `C-l'.  Drop old entries when too big.
          (set prev-inputs-var (icicle-put-at-head prev-inputs-var icicle-current-raw-input))
          (when (> (length prev-inputs) icicle-completion-history-max-length)
            (setcdr (nthcdr (1- icicle-completion-history-max-length) prev-inputs) nil)))
         ;; Forget last raw input, so it is not highlighted in *Completions*.
         ((not (and (symbolp this-command) (get this-command 'icicle-cycling-command)))
          (setq icicle-current-raw-input "")))))
    (setq icicle-last-input icicle-current-input))) ; Return `icicle-current-input'.

(defun icicle-put-at-head (list-var element)
  "Put ELEMENT at the front of the value of LIST-VAR.
If ELEMENT is already a member of the list, then it is moved to the
front.  Otherwise, it is added to the front.  Membership is tested
with `equal'.  The return value is the new value of LIST-VAR.
This is a destructive operation: the list structure is changed."
  (let* ((lis (symbol-value list-var))
         (tl (member element lis)))
    (cond ((null lis) (set list-var (list element)))
          ;;;((eq tl lis) (set list-var (cdr lis)))
          ((not (eq tl lis))
           (when tl (setcdr (nthcdr (1- (- (length lis) (length tl))) lis) (cdr tl)))
           (set list-var (cons element lis)))))
  (symbol-value list-var))

(defun icicle-remove-dots (filename)
  "Strip leading string through last ../ or ./ from FILENAME."
  (let ((newname filename))
    (save-match-data
      (while (or (string-match "\\.\\./" newname)
                 (string-match "\\./" newname)
                 ;; Emacs 21+ `file-relative-name' returns ".." and "." (no slash) for "" first arg
                 (string-match "^\\.\\.$" newname)
                 (string-match "^\\.$" newname))
        (setq newname (substring newname (match-end 0)))))
    newname))

(defun icicle-increment-cand-nb+signal-end (incr max)
  "Increment candidate number by INCR modulo MAX, and signal end of cycle."
  (if icicle-candidate-nb
      (setq icicle-candidate-nb (+ incr icicle-candidate-nb))
    (setq icicle-candidate-nb 0))       ; Reset.
  (setq icicle-candidate-nb (mod icicle-candidate-nb max))
  (when (and (= 0 icicle-candidate-nb)  ; Signal end of cycle.
             (eq last-command this-command))
    (let ((visible-bell t)) (ding))))

(defun icicle-place-cursor (input)
  "Position point and mark with respect to the minibuffer candidate.
Positions are `icicle-point-position-in-candidate' and
`icicle-mark-position-in-candidate', respectively.
INPUT is the current user input, that is, the completion root."
  (let ((case-fold-search (if (and (icicle-file-name-input-p)
                                   (boundp 'read-file-name-completion-ignore-case))
                              read-file-name-completion-ignore-case
                            completion-ignore-case))
        input-start-position)
    (goto-char (icicle-minibuffer-prompt-end))
    (setq input-start-position (point))
    (when (and (icicle-file-name-input-p) insert-default-directory)
      (search-forward (icicle-file-name-directory-w-default input) nil t)
      (setq input-start-position (point))) ; Skip directory.
    ;; Locate completion root within current completion candidate.
    (when (or (memq icicle-point-position-in-candidate '(root-start root-end))
              (memq icicle-mark-position-in-candidate  '(root-start root-end)))
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (point-max)) ; Search within the completion candidate.
          (re-search-forward (if (icicle-file-name-input-p)
                                 (icicle-file-name-nondirectory input)
                               input)
                             nil t))))
    ;; Position point.
    (case icicle-point-position-in-candidate
      (input-start (goto-char input-start-position))
      (input-end (goto-char (point-max)))
      (root-start (goto-char (max input-start-position (match-beginning 0))))
      (root-end (goto-char (max input-start-position (match-end 0)))))
    ;; Position mark.
    (unless (eq icicle-point-position-in-candidate icicle-mark-position-in-candidate)
      (push-mark (case icicle-mark-position-in-candidate
                   (input-start input-start-position)
                   (input-end (point-max))
                   (root-start (max input-start-position (match-beginning 0)))
                   (root-end (max input-start-position (match-end 0))))
                 'nomsg
                 'activate-mark))))

(defun icicle-highlight-candidate-in-Completions ()
  "Highlight the current candidate in *Completions*."
  (let ((compl-win (get-buffer-window "*Completions*" 0))
        curr-cand-pos)
    (when compl-win
      (set-window-dedicated-p compl-win t)
      (save-window-excursion
        (select-window compl-win)
        (goto-char (icicle-start-of-candidates-in-Completions))
        (icicle-move-to-next-completion icicle-candidate-nb t)
        (set-buffer-modified-p nil)
        (setq curr-cand-pos (point)))
      (set-window-point compl-win curr-cand-pos))))

(defun icicle-place-overlay (start end overlay face priority buffer &rest properties)
  "Put OVERLAY with FACE and PRIORITY between START and END in BUFFER.
OVERLAY is a symbol whose value is the overlay.  If nil, the overlay
  is created.  If non-nil, it is simply moved.
PROPERTIES are additional overlay properties to add: pairs of a
property and a value."
  (if (symbol-value overlay)            ; Overlay exists, just move it.
      (move-overlay (symbol-value overlay) start end buffer)
    (set overlay (make-overlay start end buffer))
    (overlay-put (symbol-value overlay) 'face face)
    (overlay-put (symbol-value overlay) 'priority priority)))

(defun icicle-strip-ignored-files-and-sort (candidates)
  "Remove file names with ignored extensions, and \".\".  Sort CANDIDATES.
If `icicle-sort-function' is nil, then do not sort."
  (let* ((pred1 (lambda (cand) (or (save-match-data
                                     (string-match icicle-ignored-extensions-regexp cand))
                                   (string= "./" cand))))
         (pred2 (lambda (cand) (string= "./" cand)))
         (new-candidates (icicle-delete-if (if icicle-ignored-extensions-regexp pred1 pred2)
                                           candidates)))
    ;; If the only candidates have ignored extensions, then use them.
    (unless new-candidates (setq new-candidates (icicle-delete-if pred2 candidates)))
    (if icicle-sort-function (icicle-reversible-sort new-candidates) new-candidates)))

(defun icicle-transform-candidates (candidates)
  "Apply `icicle-transform-function' to CANDIDATES.
If `icicle-transform-function' is nil, return CANDIDATES.

Note that this transformation is normally applied before completion
candidates are made available to the user, in particular, before they
are displayed in *Completions*.  Its use is thus quite different from
that of `icicle-transform-sole-candidate'."
  (if icicle-transform-function (funcall icicle-transform-function candidates) candidates))

(defun icicle-file-name-directory-w-default (file)
  "Like `file-name-directory', but return `default-directory', not nil.
Does not treat backslash as a directory separator, even on MS Windows."
  (let ((escaped-file (subst-char-in-string ?\\ ?\a file)))
    (or (file-name-directory escaped-file) default-directory)))

(defun icicle-file-name-nondirectory (file)
  "Like `file-name-nondirectory', but does not treat backslash specially.
That is, backslash is never treated as a directory separator."
  (let ((escaped-file (subst-char-in-string ?\\ ?\a file)))
    (subst-char-in-string ?\a ?\\ (file-name-nondirectory escaped-file))))

(defun icicle-file-name-input-p ()
  "Return non-nil if expected input is a file name.
This is used, instead of variable `minibuffer-completing-file-name',
because we sometimes complete against an explicit alist of file names,
even in the overall context of file-name input.  In that case, we do
not want to use file-name completion.  An example of this is
completing against a history list of file names, using
`icicle-history'."
  ;; Note that some Emacs 20 code uses this as the equivalent of `minibuffer-completing-file-name':
  ;; (memq minibuffer-completion-table '(read-file-name-internal read-directory-name-internal))
  (and (symbolp minibuffer-completion-table) (stringp minibuffer-completion-predicate)))

(defun icicle-file-directory-p (file)
  "Local, faster replacement for `file-directory-p'.
This does not do all of the file-handler processing that
`file-directory-p' does, so it is not a general replacement."
  (and (stringp file) (string= file (icicle-file-name-directory-w-default file))))

(defun icicle-minibuffer-contents ()
  "Return the user minibuffer input as a string, without text-properties."
  (save-selected-window (select-window (minibuffer-window))
                        (icicle-minibuffer-contents-from-minibuffer)))

;;$$$$$ Need to double all $'s in output from `icicle-subst-envvar-in-file-name', before calling
;;     `substitute-in-file-name'?
(defun icicle-minibuffer-contents-from-minibuffer ()
  "Return the user minibuffer input as a string, without text-properties.
The current buffer must be a minibuffer."
  (let ((input (if (fboundp 'minibuffer-contents-no-properties)
                   (minibuffer-contents-no-properties) ; e.g. Emacs 22
                 (buffer-substring-no-properties (point-min) (point-max))))) ; e.g. Emacs 20
    (when (and (icicle-file-name-input-p)
               (not (string= "" input))) ; Do nothing if user deleted everything in minibuffer.
      (let ((last-char ""))
        (when (string= "$" (substring input (1- (length input)) (length input)))
          (setq last-char "$"
                input (substring input 0 (1- (length input)))))
        (setq input
              (save-match-data
                (concat (subst-char-in-string ?\a ?\\
                                              (condition-case nil
                                                  (substitute-in-file-name
                                                   (icicle-subst-envvar-in-file-name
                                                    (subst-char-in-string ?\\ ?\a input 'in-place)))
                                                (error input))
                                              'in-place)
                        last-char)))))
    input))

(defun icicle-subst-envvar-in-file-name (input)
  "Substitute any environment vars in INPUT by their values.
Unlike `substitute-in-file-name', this does not make any other
changes, such as switching `\\' to `/' on MS Windows."
  (let ((pat1 "[^$]\\([$]{\\([^$}]+\\)}\\)") ; e.g. aaa${HOME}
        (pat2 "^[$]{\\([^$}]+\\)}")          ; e.g. ${HOME}
        (pat3 "[^$]\\([$]\\([^$]+\\)\\)")    ; e.g. aaa$HOME
        (pat4 "^[$]\\([^$]+\\)"))            ; e.g. $HOME
    (cond ((string-match pat1 input)
           (replace-regexp-in-string pat1 (or (getenv (match-string 2 input))
                                              (concat "$" (match-string 2 input)))
                                     input t t 1))
          ((string-match pat2 input)
           (replace-regexp-in-string pat2 (or (getenv (match-string 1 input))
                                              (concat "$" (match-string 1 input)))
                                     input t t))
          ((string-match pat3 input)
           (replace-regexp-in-string pat3 (or (getenv (match-string 2 input))
                                              (concat "$" (match-string 2 input)))
                                     input t t 1))
          ((string-match pat4 input)
           (replace-regexp-in-string pat4 (or (getenv (match-string 1 input))
                                              (concat "$" (match-string 1 input)))
                                     input t t))
          (t input))))

(defun icicle-filter-wo-input (candidate)
  "Filter completion CANDIDATE using regexps and predicate.
This filtering is in addition to matching user input."
  (and (or (not icicle-must-match-regexp)
           (save-match-data (string-match icicle-must-match-regexp candidate)))
       (or (not icicle-must-not-match-regexp)
           (not (save-match-data (string-match icicle-must-not-match-regexp candidate))))
       (or (not icicle-must-pass-predicate)
           (funcall icicle-must-pass-predicate candidate))))

(defun icicle-update-completions (&optional no-display)
  "Update completions list.
Update display too, if already shown and NO-DISPLAY is nil."
  (setq icicle-completion-candidates
        (condition-case nil
            (funcall (case icicle-last-completion-command
                       ((icicle-prefix-complete icicle-prefix-word-complete)
                        (if (icicle-file-name-input-p)
                            #'icicle-file-name-prefix-candidates
                          #'icicle-prefix-candidates))
                       (t
                        (if (icicle-file-name-input-p)
                            #'icicle-file-name-apropos-candidates
                          #'icicle-apropos-candidates)))
                     icicle-current-input)
          (error icicle-completion-candidates))) ; No change if completion error.
  (when (and (get-buffer-window "*Completions*" 0) (not no-display))
    (icicle-display-candidates-in-Completions)))

(defun icicle-msg-maybe-in-minibuffer (format-string &rest args)
  "Display FORMAT-STRING as a message.
If called with the minibuffer inactive, this is done using `message'.
Otherwise, it is done using `minibuffer-message'."
  (if (active-minibuffer-window)
      (save-selected-window
        (select-window (minibuffer-window))
        (minibuffer-message (apply #'format (concat "  [" format-string "]") args)))
    (apply #'message format-string args)))

(defun icicle-delete-if (pred xs)
  "A copy of list XS with no elements that satisfy predicate PRED."
  (let ((result nil))
    (dolist (x xs) (unless (funcall pred x) (push x result)))
    (nreverse result)))

(defun icicle-delete-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result nil))
    (dolist (x xs) (when (funcall pred x) (push x result)))
    (nreverse result)))

(defun icicle-frames-on (buffer &optional frame) ; From `frames-on' in `frame-fns.el'.
  "List of all live frames showing BUFFER (a buffer or its name).
The optional FRAME argument is as for function `get-buffer-window'."
  (filtered-frame-list (function (lambda (fr) (get-buffer-window buffer fr)))))

(defun icicle-candidate-set-1 (set-fn msg)
  "Helper function for defining Icicle set commands.
SET-FN is the function to apply to the current and saved candidates.
MESSAGE is the confirmation message to display in the minibuffer."
  (setq icicle-completion-candidates
        (funcall set-fn icicle-completion-candidates icicle-saved-completion-candidates))
  (if (null icicle-completion-candidates)
      (save-selected-window (select-window (minibuffer-window)) (minibuffer-message "  [EMPTY SET]"))
    (icicle-maybe-sort-and-strip-candidates)
    (icicle-scroll-or-update-Completions msg)))

(defun icicle-maybe-sort-and-strip-candidates ()
  "Sort `icicle-completion-candidates'.  Strip ignored file names too."
  (if (icicle-file-name-input-p)
      (setq icicle-completion-candidates
            (icicle-strip-ignored-files-and-sort icicle-completion-candidates))
    (when icicle-sort-function
      (setq icicle-completion-candidates (icicle-reversible-sort icicle-completion-candidates)))))

(defun icicle-scroll-or-update-Completions (msg)
  "Scroll *Completions* if this command was repeated; else update it."
  (if (get-buffer-window "*Completions*" 0)
      (if (eq last-command this-command)
          ;; User repeated the command.  Scroll window around.
          (icicle-scroll-Completions)
        ;; User did something else (e.g. changed input).  Update the display.
        (icicle-display-candidates-in-Completions)
        (save-selected-window (select-window (minibuffer-window)) (minibuffer-message msg)))
    ;; No window yet.  Show window.
    (icicle-display-candidates-in-Completions)
    (save-selected-window (select-window (minibuffer-window)) (minibuffer-message msg))))

;; $$ No longer used.
(defun icicle-display-Completions ()
  "Display *Completions* buffer."
  (let ((completions (all-completions "" minibuffer-completion-table minibuffer-completion-predicate
                                      icicle-ignore-space-prefix-flag)))
    (when (> (length icicle-completion-candidates) icicle-incremental-completion-threshold)
      (message "Displaying completion candidates..."))
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list
       (if icicle-sort-function (icicle-reversible-sort completions) completions)))))

;; From `cl-seq.el', function `union', without keyword treatment.
;; Same as `simple-set-union' in `misc-fns.el'.
(defun icicle-set-union (list1 list2)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or
LIST2.  This is a non-destructive function; it copies the data if
necessary."
  (cond ((null list1) list2)
        ((null list2) list1)
        ((equal list1 list2) list1)
        (t
         (or (>= (length list1) (length list2))
             (setq list1 (prog1 list2 (setq list2 list1)))) ; Swap them.
         (while list2
           (unless (member (car list2) list1)
               (setq list1 (cons (car list2) list1)))
           (setq list2 (cdr list2)))
         list1)))

;; From `cl-seq.el', function `intersection', without keyword treatment.
;; Same as `simple-set-intersection' in `misc-fns.el'.
(defun icicle-set-intersection (list1 list2)
  "Set intersection of lists LIST1 and LIST2.
This is a non-destructive operation: it copies the data if necessary."
  (and list1 list2
       (if (equal list1 list2)
           list1
         (let ((result nil))
           (unless (>= (length list1) (length list2))
             (setq list1 (prog1 list2 (setq list2 list1)))) ; Swap them.
           (while list2
             (when (member (car list2) list1)
               (setq result (cons (car list2) result)))
             (setq list2 (cdr list2)))
           result))))

;; From `cl-seq.el', function `set-difference', without keyword treatment.
;; Same as `simple-set-difference' in `misc-fns.el'.
(defun icicle-set-difference (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all items that appear in LIST1 but not LIST2.
This is non-destructive; it makes a copy of the data if necessary, to
avoid corrupting the original LIST1 and LIST2."
  (if (or (null list1) (null list2)) list1
    (let ((result nil))
      (while list1
        (unless (member (car list1) list2) (setq result (cons (car list1) result)))
        (setq list1 (cdr list1)))
      result)))

;; Note that initial and trailing spaces will not be noticeable.  That's OK.
(defun icicle-highlight-complete-input ()
  "Highlight minibuffer input, showing that it is a sole completion.
Overlay `icicle-complete-input-overlay' is created with `match' face,
unless it exists."
  (let ((case-fold-search (if (and (icicle-file-name-input-p)
                                   (boundp 'read-file-name-completion-ignore-case))
                              read-file-name-completion-ignore-case
                            completion-ignore-case))
        input-start-position)
    (save-excursion
      (goto-char (icicle-minibuffer-prompt-end))
      (setq input-start-position (point))
      (when (and (icicle-file-name-input-p) insert-default-directory)
        (search-forward (icicle-file-name-directory-w-default
                         (icicle-minibuffer-contents-from-minibuffer))
                        nil
                        t)
        (setq input-start-position (point))) ; Skip directory.
      (if icicle-complete-input-overlay ; Don't recreate if exists.
          (move-overlay icicle-complete-input-overlay
                        input-start-position (point-max) (current-buffer))
        (setq icicle-complete-input-overlay (make-overlay input-start-position (point-max)))
        (overlay-put icicle-complete-input-overlay 'face 'icicle-complete-input)))))

(defun icicle-call-then-update-Completions (fn &rest args)
  "Call FN with ARGS, then update *Completions* with input matches."
  (save-match-data
    (apply fn args)
    ;;$$$ (let ((tramp-completion-mode t))    ; Fool Tramp into thinking it is in completion mode.
    (setq icicle-current-input   (icicle-minibuffer-contents-from-minibuffer)
          icicle-input-fail-pos  nil)
    (when (overlayp icicle-complete-input-overlay) (delete-overlay icicle-complete-input-overlay))
    (icicle-highlight-initial-whitespace icicle-current-input)
    (if (< (length icicle-current-input) icicle-Completions-display-min-input-chars)
        (save-selected-window (icicle-remove-Completions-window))
      ;; `icicle-highlight-input-noncompletion' return val just saves call to `icicle-file-remote-p'.
      (let ((tested-remote-p (eq 'file-remote-p (icicle-highlight-input-noncompletion))))
        (when (and icicle-incremental-completion-p
                   (or tested-remote-p  ; Don't autocomplete remote file.
                       (not (icicle-file-name-input-p))
                       ;; Might be remote if `icicle-highlight-input-completion-failure' is `always'.
                       (not (icicle-file-remote-p icicle-current-input)))
                   (or (get-buffer-window "*Completions*" 0) ; Already displayed.
                       (not (eq t icicle-incremental-completion-p))) ; Display anyway.
                   (let ((len (length icicle-completion-candidates)))
                     (or (and (> len 1) (> icicle-incremental-completion-threshold len)) ; Many cands
                         (sit-for icicle-incremental-completion-delay)))) ; Wait, unless input.
          (let ((icicle-edit-update-p t))
            (funcall (if (eq icicle-current-completion-mode 'prefix)
                         #'icicle-prefix-complete
                       #'icicle-apropos-complete))
            (run-hooks 'icicle-update-input-hook)))))
    (setq mark-active nil)))

(defun icicle-highlight-input-noncompletion ()
  "Highlight the portion of the current input that does not complete.
See the doc string of `icicle-highlight-input-completion-failure' for
information about when this highlighting occurs.

Returns symbol `file-remote-p' if we know the input is a remote file
name.  Otherwise, returns the part of the input that matches
candidates, or nil if no highlighting was attempted."
  (let ((input-start (icicle-minibuffer-prompt-end))
        (input (icicle-minibuffer-contents-from-minibuffer)))
    (cond ((string= "" input) "")       ; Return string: highlighting attempted.
          ((or (input-pending-p)
               (not icicle-highlight-input-completion-failure)
               (and (not icicle-incremental-completion-flag)
                    (not (eq 'always icicle-highlight-input-completion-failure)))
               (and (not icicle-require-match-p)
                    (memq icicle-highlight-input-completion-failure
                          '(implicit-strict explicit-strict)))
               (let ((len (length icicle-completion-candidates)))
                 (and (> len 1) (> icicle-highlight-input-completion-failure-threshold len))))
           nil)                         ; Return nil: no highlighting attempted.
          ((and icicle-input-fail-pos (< (point) icicle-input-fail-pos))
           (setq icicle-input-fail-pos nil) ; Reset failure position.
           ;; Remove vestigial highlighting on matched part (e.g. from another completion mode).
           (when (> (or icicle-input-fail-pos (point-max)) input-start)
             (remove-text-properties input-start (1- (or icicle-input-fail-pos (point-max)))
                                     '(face)))
           nil)                         ; Return nil: no highlighting attempted.
          ;; Do the same as for the previous, except return indication that it is a remote file.
          ((and (icicle-file-name-input-p)
                (not (memq icicle-highlight-input-completion-failure '(always explicit-remote)))
                (icicle-file-remote-p input))
           (setq icicle-input-fail-pos nil)
           (when (> (or icicle-input-fail-pos (point-max)) input-start)
             (remove-text-properties input-start (+ input-start (length input)) '(face)))
           'file-remote-p)              ; Indicate that it is a remote file.
          ((and icicle-highlight-input-completion-failure-delay
                (sit-for icicle-highlight-input-completion-failure-delay))
           ;; First, a quick check through last two chars.
           ;; If last succeeds, then done.
           ;; If last fails and next-to-last succeeds, then done.
           ;; Otherwise, highlight the others using a binary search.
           (let ((matchp (icicle-any-candidates-p input))) ; Entire input, through last char.
             (unless matchp
               ;; Record failure position and highlight last char.
               (setq icicle-input-fail-pos (if icicle-input-fail-pos
                                               (min icicle-input-fail-pos (point-max))
                                             (point-max)))
               (put-text-property (1- icicle-input-fail-pos) (point-max)
                                  'face (if icicle-require-match-p
                                            'icicle-input-completion-fail
                                          'icicle-input-completion-fail-lax))
               ;; See if next-to-last char gives a match.  Typical use case: mistyping a char at end.
               (setq input (substring input 0 (1- (length input))))
               (unless (string= "" input)
                 (setq matchp (icicle-any-candidates-p input))
                 ;; If more than just the last char fails, highlight the others using binary search.
                 (unless matchp (icicle-highlight-input-noncompletion-rest)))))
           ;; Remove vestigial highlighting on matched part (e.g. from another completion mode).
           (when (> (or icicle-input-fail-pos (point-max)) input-start)
             (remove-text-properties
              input-start (1- (or icicle-input-fail-pos (point-max))) '(face)))
           input)                       ; Return part of INPUT that matches: highlighting attempted.
          (t nil))))                    ; Return nil: no highlighting attempted.

(defun icicle-highlight-input-noncompletion-rest ()
  "Helper function for `icicle-highlight-input-noncompletion'."
  (let* ((input-start (icicle-minibuffer-prompt-end))
         (pos (1- icicle-input-fail-pos))
         (delta pos)
         (last-pos input-start)
         (matchp nil)
         input)
    (while (and (> pos input-start) (or (not matchp) (< pos icicle-input-fail-pos))) ; Binary search.
      (setq input  (buffer-substring input-start pos)
            delta  (max 1 (/ (abs (- pos last-pos)) 2))
            matchp (icicle-any-candidates-p input))
      ;; $$$$$$ Emacs BUG (prefix completion): c:/foo/$$ does not highlight the `$$', because
      ;; (try-completion "c:/foo/$" 'read-file-name-internal "c:/foo/") returns "c:/foo/$".
      ;; (However, c:/foo/$ highlights the `$' correctly.)
      (unless matchp (setq icicle-input-fail-pos (min pos icicle-input-fail-pos)))
      (setq last-pos  pos
            pos       (if matchp (+ pos delta) (- pos delta))))
    (unless (or (< pos input-start) (> pos icicle-input-fail-pos))
      (put-text-property (1- icicle-input-fail-pos) (point-max)
                         'face (if icicle-require-match-p
                                   'icicle-input-completion-fail
                                 'icicle-input-completion-fail-lax)))
    input))                       ; Return part of INPUT that matches.

;; $$$$$ TRYING WITHOUT `save-match-data', but probably need it.
(defun icicle-file-remote-p (file)
  "Non-nil means FILE is likely to name a file on a remote system.
For MS Windows, this includes a file on a mapped network drive.
Otherwise, this uses `ffap-file-remote-p' and `file-remote-p' (if
defined)."
  ;; $$$$  (save-match-data        ; $$$$$ IS THIS NEEDED?
  (if (and (eq system-type 'windows-nt)
           (let ((case-fold-search t))
             (string-match "\\`\\([a-z]:\\)" file)))
      (eq 0 (condition-case nil
                (call-process shell-file-name nil nil nil shell-command-switch
                              (concat "NET USE " (match-string 1 file)))
              (error nil)))
    (or (and (fboundp 'ffap-file-remote-p) (ffap-file-remote-p file))
        (and (fboundp 'file-remote-p) (file-remote-p file)))))

(defun icicle-any-candidates-p (input)
  "Return non-nil if there is any completion for INPUT, nil otherwise."
  (condition-case nil
      (funcall (case icicle-current-completion-mode
                 (apropos (if (icicle-file-name-input-p)
                              #'icicle-apropos-any-file-name-candidates-p
                            #'icicle-apropos-any-candidates-p))
                 (otherwise (if (icicle-file-name-input-p)
                                #'icicle-prefix-any-file-name-candidates-p
                              #'icicle-prefix-any-candidates-p)))
               input)
    (error nil)))

(defun icicle-prefix-any-candidates-p (input)
  "Return non-nil if current partial INPUT has prefix completions."
  (let ((minibuffer-completion-table minibuffer-completion-table)
        (minibuffer-completion-predicate minibuffer-completion-predicate))
    (try-completion input minibuffer-completion-table minibuffer-completion-predicate)))

(defun icicle-prefix-any-file-name-candidates-p (input)
  "Return non-nil if partial file-name INPUT has prefix completions."
  (let* ((minibuffer-completion-table minibuffer-completion-table)
         (minibuffer-completion-predicate minibuffer-completion-predicate)
         (slashed-p (and (> (length input) 0) (eq ?/ (aref input 0))))
         (pred (if slashed-p "/" default-directory)))
    (when slashed-p (setq input (substring input 1)))
    (try-completion input minibuffer-completion-table pred)))

(defun icicle-apropos-any-candidates-p (input)
  "Return non-nil if current partial INPUT has apropos completions."
  (when icicle-regexp-quote-flag (setq input (regexp-quote input)))
  (let* ((minibuffer-completion-table minibuffer-completion-table)
         (minibuffer-completion-predicate minibuffer-completion-predicate)
         (all (all-completions "" minibuffer-completion-table minibuffer-completion-predicate
                              icicle-ignore-space-prefix-flag)))
    (catch 'icicle-apropos-any-candidates-p
      (dolist (cand all)
        (when (funcall icicle-apropos-complete-match-fn input cand)
          (throw 'icicle-apropos-any-candidates-p cand)))
      nil)))

(defun icicle-apropos-any-file-name-candidates-p (input)
  "Return non-nil if partial file-name INPUT has apropos completions."
  (let* ((default-directory (icicle-file-name-directory-w-default input))
         (minibuffer-completion-table minibuffer-completion-table)
         (minibuffer-completion-predicate minibuffer-completion-predicate))
    (setq input (or (icicle-file-name-nondirectory input) ""))
    (condition-case nil
        (progn
          (when icicle-regexp-quote-flag (setq input (regexp-quote input)))
          (let ((slashed-p (and (> (length input) 0) (eq ?/ (aref input 0)))))
            (when slashed-p (setq input (substring input 1)))
            (let ((candidates (icicle-transform-candidates
                               (all-completions "" minibuffer-completion-table
                                                (if slashed-p "/" default-directory)
                                                icicle-ignore-space-prefix-flag))))
              (catch 'icicle-apropos-any-candidates-p
                (dolist (cand candidates)
                  (when (if (member cand '("../" "./"))
                            (member input '(".." ".")) ; Prevent "" from matching "../"
                          (and (or (not icicle-apropos-complete-match-fn)
                                   (funcall icicle-apropos-complete-match-fn input cand))))
                    (throw 'icicle-apropos-any-candidates-p cand)))
                nil))))
      (quit (top-level)))))             ; Let `C-g' stop it.

(defun icicle-clear-minibuffer ()
  "Delete all user input in the minibuffer.
This must be called from the minibuffer."
  (if (fboundp 'delete-minibuffer-contents) (delete-minibuffer-contents) (erase-buffer)))

;; Borrowed from `ps-print.el'
(defun icicle-remove-duplicates (list)
  "Copy of LIST with duplicate elements removed.  Tested with `equal'."
  (let ((tail list)
        new)
    (while tail
      (unless (member (car tail) new) (push (car tail) new))
      (pop tail))
    (nreverse new)))

(defun icicle-file-readable-p (file)
  "Return non-nil if FILE (a string) names a readable file."
  (and (not (string= "" file)) (file-readable-p file) (not (file-directory-p file))))

(defun icicle-file-writable-p (file)
  "Return non-nil if FILE (a string) names a writable file."
  (and (not (string= "" file)) (file-writable-p file) (not (file-directory-p file))))

(defun icicle-files-within (file-list accum)
  "List of all readable files in FILE-LIST.
Accessible directories in FILE-LIST are processed recursively to
include their files and the files in their subdirectories.  The list
of files is accumulated in ACCUM, which is used for recursive calls."
  (let ((res accum))
    (while file-list
      (if (file-directory-p (car file-list))
          (when (file-accessible-directory-p (car file-list)) ; Skip inaccessible directories.
            (setq res (icicle-files-within (directory-files (car file-list) 'full icicle-re-no-dot)
                                           res)))
        (when (file-readable-p (car file-list)) (setq res (cons (car file-list) res))))
      (pop file-list))
    res))

(defun icicle-delete-whitespace-from-string (string &optional from to)
  "Remove whitespace from substring of STRING from FROM to TO.
If FROM is nil, then start at the beginning of STRING (FROM = 0).
If TO is nil, then end at the end of STRING (TO = length of STRING).
FROM and TO are zero-based indexes into STRING.
Character FROM is affected (possibly deleted).  Character TO is not."
  (setq from (or from 0) to (or to (length string)))
  (with-temp-buffer
    (insert string)
    (goto-char (+ from (point-min)))
    (let ((count from)
          char)
      (while (and (not (eobp)) (< count to))
        (setq char (char-after))
        (if (memq char '(?\  ?\t ?\n)) (delete-char 1) (forward-char 1))
        (setq count (1+ count)))
      (buffer-string))))

(defun icicle-barf-if-outside-minibuffer ()
  "Raise an error if `this-command' is called outside the minibuffer."
  (unless (eq (current-buffer) (window-buffer (minibuffer-window)))
    (error "Command `%s' must be called from the minibuffer" this-command)))

(defun icicle-barf-if-outside-Completions ()
  "Raise an error if `this-command' is called outside buffer *Completions*."
  (unless (eq (current-buffer) (get-buffer "*Completions*"))
    (error "Command `%s' must be called from *Completions* buffer" this-command)))

(defun icicle-barf-if-outside-Completions-and-minibuffer ()
  "Error if `this-command' called outside *Completions* and minibuffer."
  (unless (or (eq (current-buffer) (window-buffer (minibuffer-window)))
              (eq (current-buffer) (get-buffer "*Completions*")))
    (error "Command `%s' must be called from *Completions* buffer or the minibuffer" this-command)))

(defun icicle-command-abbrev-save ()
  "Save `icicle-command-abbrev-alist'.  Used on `kill-emacs-hook'."
  (condition-case err                   ; Don't raise an error, since it's on `kill-emacs-hook.
      (let ((sav (get 'icicle-command-abbrev-alist 'saved-value)))
        (unless (and (or (null sav)
                         (and (consp sav) (consp (car sav)) (consp (cdar sav)) (consp (cadar sav))))
                     (equal icicle-command-abbrev-alist (cadar sav)))
          (customize-save-variable 'icicle-command-abbrev-alist icicle-command-abbrev-alist)))
    (error (message "Cannot save new value of `icicle-command-abbrev-alist'") (sleep-for 3))))

(defun icicle-expand-file-name (input dir)
  "Expand file-name INPUT in directory DIR.
Similar to `expand-file-name', except:

 - If INPUT does not end in a slash, and DIR/INPUT is a directory,
   a trailing slash is added.

 - If INPUT ends in a slash, but DIR/INPUT is not a directory, then
   the trailing slash is removed."
  (let ((expanded-input (directory-file-name (expand-file-name input dir))))
    ;; Add trailing slash if input is a directory.
    (when (file-directory-p expanded-input)
      (setq expanded-input (file-name-as-directory expanded-input)))
    expanded-input))

(defun icicle-start-of-candidates-in-Completions ()
  "Return buffer position of the first candidate in *Completions*."
  (save-excursion
    (goto-char (point-min))
    (forward-line (if icicle-show-Completions-help-flag 2 1))
    (point)))

(defun icicle-key-description (keys &optional no-angles)
  "`key-description', but non-nil NO-ANGLES means use no angle brackets."
  (let ((result (key-description keys)))
    (when no-angles              ; Assume space separates angled keys.
      (setq result (replace-regexp-in-string "<\\([^>]+\\)>" "\\1" result 'fixed-case)))
    result))

;; $$ Not used.
;; (defun icicle-alist-delete-all (key alist &optional test)
;;     "Delete from ALIST all elements whose car is the same as KEY.
;; Optional arg TEST is the equality test to use.  If nil, `eq' is used.
;; Return the modified alist.
;; Elements of ALIST that are not conses are ignored."
;;     (setq test (or test #'eq))
;;     (while (and (consp (car alist)) (funcall test (car (car alist)) key))
;;       (setq alist (cdr alist)))
;;     (let ((tail alist) tail-cdr)
;;       (while (setq tail-cdr (cdr tail))
;;         (if (and (consp (car tail-cdr)) (funcall test (car (car tail-cdr)) key))
;;             (setcdr tail (cdr tail-cdr))
;;           (setq tail tail-cdr))))
;;     alist)

;;; Standard Emacs 21+ function, defined here for Emacs 20.
(unless (fboundp 'assq-delete-all)
  (defun assq-delete-all (key alist)
    "Delete from ALIST all elements whose car is `eq' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
    (while (and (consp (car alist)) (eq (car (car alist)) key)) (setq alist (cdr alist)))
    (let ((tail alist) tail-cdr)
      (while (setq tail-cdr (cdr tail))
        (if (and (consp (car tail-cdr)) (eq (car (car tail-cdr)) key))
            (setcdr tail (cdr tail-cdr))
          (setq tail tail-cdr))))
    alist))

(defun icicle-assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist)) (equal (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr)) (equal (car (car tail-cdr)) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail tail-cdr))))
  alist)

(defun icicle-abbreviate-or-expand-file-name (filename &optional default-dir)
  "`abbreviate-file-name' if `icicle-use-~-for-home-dir-flag' is non-nil.
`expand-file-name' if `icicle-use-~-for-home-dir-flag' is nil."
  (if icicle-use-~-for-home-dir-flag
      (abbreviate-file-name (expand-file-name filename default-dir))
    (expand-file-name filename default-dir)))

(defun icicle-reversible-sort (list)
  "`sort' using `icicle-sort-function', or the reverse.
Sort LIST using `icicle-sort-function'.  Reverse the result if
`icicle-reverse-sort-p' is non-nil."
;;$$ (when (and icicle-edit-update-p icicle-completion-candidates
;;              (> (length icicle-completion-candidates) icicle-incremental-completion-threshold))
;;     (message "Sorting candidates..."))
   (sort list (if icicle-reverse-sort-p
                 (lambda (a b) (not (funcall icicle-sort-function a b)))
               icicle-sort-function)))

(defun icicle-put-alist-candidate (cand)
  "Put cdr of CAND on its car, as text property `icicle-whole-candidate'.
Returns the propertized string, (car CAND)."
  (let ((text-cand (copy-sequence (car cand))))
    (put-text-property 0 (length text-cand) 'icicle-whole-candidate (cdr cand) text-cand)
    (setcar cand text-cand)
    text-cand))

(defun icicle-get-alist-candidate (string)
  "Return full completion candidate that corresponds to displayed STRING.
STRING is the name of the candidate, as shown in *Completions*.

If `icicle-whole-candidate-as-text-prop-p' is non-nil, then the full
candidate might be available as text property `icicle-whole-candidate'
of STRING.  If so, then that is used.

Otherwise, the full candidate is obtained from
`icicle-candidates-alist'.  In this case:
 If the user cycled among candidates or used `mouse-2', then use the
   current candidate number, and ignore STRING.
 Otherwise:
   If only one candidate matches STRING, use that.
   Else raise an error telling user to use cycling or `mouse-2'."
  (or (and icicle-whole-candidate-as-text-prop-p
           (get-text-property 0 'icicle-whole-candidate string))
      (and icicle-candidates-alist
           (let ((cand-entries (icicle-filter-alist icicle-candidates-alist
                                                    icicle-completion-candidates)))
             (if (wholenump icicle-candidate-nb) ; Cycled or used `mouse-2' to choose the candidate.
                 (elt cand-entries (mod icicle-candidate-nb (length icicle-candidates-alist)))
               ;; If `icicle-completion-candidates' is nil, because user didn't use `TAB' or `S-TAB',
               ;; then `icicle-candidates-alist' can contain non-matches.  So, we check for more than
               ;; one match.  However, we can't just use `assoc', because candidates might be
               ;; multi-completions (lists).
               (let ((first-match (icicle-first-matching-candidate string icicle-candidates-alist)))
                 (if (and first-match
                          (not (icicle-first-matching-candidate
                                string
                                (setq cand-entries (delete first-match cand-entries)))))
                     first-match        ; Only one match, so use it.
                   (error
                    "Ambiguous choice. Cycle or use `mouse-2' to choose unique matching \
candidate."))))))))

(defun icicle-filter-alist (alist filter-keys)
  "Filter ALIST, keeping items whose cars match FILTER-KEYS, in order.
The original ALIST is not altered; a copy is filtered and returned.
If FILTER-KEYS is empty, then ALIST is returned, not a copy."
  (if filter-keys
      (icicle-delete-if-not
       (lambda (item)
         (member (if (consp (car item))
                     (concat (mapconcat #'identity (car item) icicle-list-join-string)
                             icicle-list-end-string)
                   (car item))
                 filter-keys))
       alist)
    alist))

(defun icicle-first-matching-candidate (cand candidates)
  "Return the first element of alist CANDIDATES that matches CAND.
If CANDIDATES is a normal list of completion candidates, then this is
just `assoc'.
If CANDIDATES contains multi-completions, then matching means matching
the concatenated multi-completion parts, joined by
`icicle-list-join-string'."
  (cond ((null candidates) nil)
        ((if (consp (caar candidates))  ; Multi-completion candidate
             (save-match-data
               (string-match cand (mapconcat #'identity (caar candidates) icicle-list-join-string)))
           (equal cand (caar candidates))) ; This case is just `assoc'.
         (car candidates))
        (t (icicle-first-matching-candidate cand (cdr candidates)))))

(defun icicle-completing-p ()
  "Non-nil if reading minibuffer input with completion.
This caches the value returned in variable `icicle-completing-p'.
Use the function, not the variable, to test, if not sure to be in the
minibuffer."
  (and (active-minibuffer-window)
       ;; $$$ (where-is-internal 'icicle-candidate-action nil 'first-only)
       (member (current-local-map)
               (if (boundp 'minibuffer-local-filename-completion-map)
                   (list minibuffer-local-completion-map minibuffer-local-must-match-map
                         minibuffer-local-filename-completion-map
                         minibuffer-local-must-match-filename-map)
                 (list minibuffer-local-completion-map minibuffer-local-must-match-map)))
       (setq icicle-completing-p t)))   ; Cache the value, to avoid calling `where-is-internal'.

(defun icicle-substring-no-properties (string &optional from to)
  "Return a substring of STRING, without text properties.
It starts at index FROM and ending before TO.
TO may be nil or omitted; then the substring runs to the end of STRING.
If FROM is nil or omitted, the substring starts at the beginning of STRING.
If FROM or TO is negative, it counts from the end.

With one argument, just copy STRING without its properties."
  (if (fboundp 'substring-no-properties)
      (substring-no-properties string from to) ; Emacs 22.
    (let ((substrg (copy-sequence (substring string (or from 0) to))))
      (set-text-properties 0 (length substrg) nil substrg)
      substrg)))

(defun icicle-highlight-lighter ()
  "Highlight `Icy' mode-line indicator of Icicle mode.
Highlighting indicates the current completion status."
  (when icicle-highlight-lighter-flag
    (let ((strg " Icy")
          (face (cond ((and icicle-candidate-action-fn icicle-require-match-p)
                       '(icicle-multi-command-completion icicle-mustmatch-completion))
                      (icicle-candidate-action-fn 'icicle-multi-command-completion)
                      (icicle-require-match-p
                       '(icicle-completion icicle-mustmatch-completion))
                      (t 'icicle-completion))))
      (when icicle-candidate-action-fn (setq strg (concat strg "+")))
      (put-text-property 0 (length strg) 'face face strg)
      (setq minor-mode-alist (delete '(icicle-mode " Icy") minor-mode-alist))
      (setq minor-mode-alist (delete '(icicle-mode " Icy+") minor-mode-alist))
      (add-to-list 'minor-mode-alist `(icicle-mode ,strg)))
    (if (fboundp 'redisplay) (redisplay t) (force-mode-line-update t))))

(defun icicle-unhighlight-lighter ()
  "Unhighlight `Icy' mode-line indicator of Icicle mode."
  (when icicle-highlight-lighter-flag
    (let ((strg " Icy"))
      (setq minor-mode-alist (delete `(icicle-mode ,strg) minor-mode-alist))
      (when icicle-candidate-action-fn (setq strg (concat strg "+")))
      (setq minor-mode-alist (delete `(icicle-mode ,strg) minor-mode-alist))
      (add-to-list 'minor-mode-alist '(icicle-mode " Icy")))
    (if (fboundp 'redisplay)
        (redisplay t)
      (force-mode-line-update t))))
 
;;(@* "Icicles functions - sort functions")

;;; Icicles functions - sort functions -------------------------------

(defun icicle-historical-alphabetic-p (s1 s2)
  "Non-nil means S1 is a past input and S2 is not or S1 < S2 (alphabet).
Return non-nil if S1 is a previous input and either S2 is not or
S1 `icicle-case-string-less-p' S2.  S1 and S2 must be strings.

When used as a comparison function for completion candidates, this
makes candidates matching previous inputs available first (at the top
of buffer *Completions*).  Candidates are effectively in two groups,
each of which is sorted alphabetically separately: matching previous
inputs, followed by matching candidates that have not yet been used."
  ;; We could use `icicle-delete-duplicates' to shorten the history, but that takes time too.
  ;; And, starting in Emacs 22, histories will not contain duplicates anyway.
  (let ((hist (and (symbolp minibuffer-history-variable)
                   (boundp minibuffer-history-variable)
                   (symbol-value minibuffer-history-variable)))
        (dir (and (icicle-file-name-input-p)
                  (or (file-name-directory (or icicle-last-input icicle-current-input))
                       default-directory))))
    (if (not (consp hist))
        (icicle-case-string-less-p s1 s2)
      (when dir (setq s1 (expand-file-name s1 dir) s2 (expand-file-name s2 dir)))
      (let ((s1-previous-p (member s1 hist))
            (s2-previous-p (member s2 hist)))
        (or (and (not s1-previous-p) (not s2-previous-p) (icicle-case-string-less-p s1 s2))
            (and s1-previous-p (not s2-previous-p))
            (and s1-previous-p s2-previous-p (icicle-case-string-less-p s1 s2)))))))

;; $$ Alternative definition, but it doesn't seem any faster, and is slightly less clear.
;; (defun icicle-most-recent-first-p (s1 s2)
;;   "Non-nil means S1 was used more recently than S2.
;; Also:
;;  S1 < S2 if S1 was used previously but S2 was not.
;;  S1 < S2 if neither was used previously
;;   and S1 `icicle-case-string-less-p' S2."
;;   ;; We could use `icicle-delete-duplicates' to shorten the history, but that takes time too.
;;   ;; And, starting in Emacs 22, histories will not contain duplicates anyway.
;;   (let ((hist (and (symbolp minibuffer-history-variable)
;;                    (symbol-value minibuffer-history-variable)))
;;         (dir (and (icicle-file-name-input-p)
;;                   (or (file-name-directory (or icicle-last-input icicle-current-input))
;;                       default-directory)))
;;         (s1-in-hist nil)
;;         (s2-in-hist nil))
;;     (if (not (consp hist))
;;         (icicle-case-string-less-p s1 s2)
;;       (when dir (setq s1 (expand-file-name s1 dir) s2 (expand-file-name s2 dir)))
;;       (while (and hist (not (setq s1-in-hist (equal s1 (car hist)))))
;;         (when (setq s2-in-hist (equal s2 (car hist))) (setq hist nil))
;;         (setq hist (cdr hist)))
;;       (or (and hist s1-in-hist) (and (not s2-in-hist) (icicle-case-string-less-p s1 s2))))))

(defun icicle-most-recent-first-p (s1 s2)
  "Non-nil means S1 was used more recently than S2.
Also:
 S1 < S2 if S1 was used previously but S2 was not.
 S1 < S2 if neither was used previously
  and S1 `icicle-case-string-less-p' S2."
  ;; We could use `icicle-delete-duplicates' to shorten the history, but that takes time too.
  ;; And, starting in Emacs 22, histories do not contain duplicates anyway.
  (let ((hist (and (symbolp minibuffer-history-variable)
                   (boundp minibuffer-history-variable)
                   (symbol-value minibuffer-history-variable)))
        (dir (and (icicle-file-name-input-p)
                  (or (file-name-directory (or icicle-last-input icicle-current-input))
                      default-directory)))
        (s1-tail nil)
        (s2-tail nil))
    (if (not (consp hist))
        (icicle-case-string-less-p s1 s2)
      (when dir (setq s1 (expand-file-name s1 dir) s2 (expand-file-name s2 dir)))
      (setq s1-tail (member s1 hist) s2-tail (member s2 hist))
      (cond ((and s1-tail s2-tail) (>= (length s1-tail) (length s2-tail)))
            (s1-tail t)
            (s2-tail nil)
            (t (icicle-case-string-less-p s1 s2))))))

(defun icicle-dirs-last-p (s1 s2)
  "Non-nil means S1 is a file and S2 a dir, or S1 < S2 (alphabet).
This can be used as the value for `icicle-sort-function'.
It is especially useful when `icicle-cycle-into-subdirs-flag' is
non-nil.  Otherwise, cycling into subdirectories is depth-first, not
breadth-first.
If not doing file-name completion, then this is the same as
`icicle-case-string-less-p'."
  (if (icicle-file-name-input-p)
      (let ((s1-dir-p (icicle-file-directory-p s1))
            (s2-dir-p (icicle-file-directory-p s2)))
        (if (or (and s1-dir-p s2-dir-p) ; Both or neither are directories.
                (not (or s1-dir-p s2-dir-p)))
            (icicle-case-string-less-p s1 s2)  ; Compare equals.
          s2-dir-p))                 ; Files come before directories.
    (icicle-case-string-less-p s1 s2)))

(defun icicle-last-modified-first-p (s1 s2)
  "Non-nil means file S1 was last modified after S2.
If not doing file-name completion, then this is the same as
`icicle-case-string-less-p'.
This can be used as the value for `icicle-sort-function'."
  (if (icicle-file-name-input-p)
      (let ((mod-date1 (nth 5 (file-attributes s1)))
            (mod-date2 (nth 5 (file-attributes s2))))
        (or (< (car mod-date2) (car mod-date1)) ; High-order bits.
            (and (= (car mod-date2) (car mod-date1)) ; Low-order bits.
                 (< (cadr mod-date2) (cadr mod-date1)))))
    (icicle-case-string-less-p s1 s2)))

(defun icicle-proxy-candidate-first-p (s1 s2)
  "Return non-nil if S1 is a proxy candidate and S2 is not.
Return nil if S2 is a proxy candidate and S1 is not.
Otherwise, return non-nil if S1 is `string-lessp' S2."
  (let ((s1-proxy-p (or (member s1 icicle-proxy-candidates)
                        (and icicle-proxy-candidate-regexp
                             (save-match-data (string-match icicle-proxy-candidate-regexp s1)))))
        (s2-proxy-p (or (member s2 icicle-proxy-candidates)
                        (and icicle-proxy-candidate-regexp
                             (save-match-data (string-match icicle-proxy-candidate-regexp s2))))))
    (or (and (not s1-proxy-p) (not s2-proxy-p) (icicle-case-string-less-p s1 s2))
        (and s1-proxy-p (not s2-proxy-p))
        (and s1-proxy-p s2-proxy-p (icicle-case-string-less-p s1 s2)))))

(defun icicle-command-abbrev-used-more-p (s1 s2)
  "Return non-nil if S1 was invoked more often than S2 via an abbrev.
S1 and S2 are strings naming commands.
If neither was invoked or both were invoked the same number of times,
then return non-nil if S1 is `string-lessp' S2."
  (let* ((alist-tails (mapcar #'cdr icicle-command-abbrev-alist))
         (s1-entry (assq (intern s1) alist-tails))
         (s2-entry (assq (intern s2) alist-tails)))
    (if (and (not s1-entry) (not s2-entry))
        (string-lessp s1 s2)
      (let ((s1-rank (elt s1-entry 1))
            (s2-rank (elt s2-entry 1)))
        (cond ((and (not s1-rank) (not s2-rank)) (string-lessp s1 s2))
              ((and s1-rank s2-rank (eq s1-rank s2-rank)) (string-lessp s1 s2))
          (t (>= (or s1-rank 0) (or s2-rank 0))))))))

(defun icicle-part-N-lessp (n s1 s2)
  "`icicle-case-string-less-p' applied to the Nth parts of S1 and S2.
The strings each have at least N parts, separated by
`icicle-list-join-string'.  Parts other than the Nth are ignored.
Return non-nil if and only if the Nth part of S1 is less than the Nth
part of S2.  The Nth parts are compared lexicographically without
regard to letter case.  N is one-based, so a value of 1 means compare
the first parts."
  (unless (and (wholenump n) (> n 0)) (error "`icicle-part-N-lessp': N must be > 0"))
  (let ((case-fold-search t)
        (part1 (elt (split-string s1 icicle-list-join-string) (1- n)))
        (part2 (elt (split-string s2 icicle-list-join-string) (1- n))))
    (and part1 part2 (icicle-case-string-less-p part1 part2)))) ; In case strings were not multipart.

(defun icicle-part-1-lessp (s1 s2)
  "`icicle-part-N-lessp', with N = 1."
  (icicle-part-N-lessp 1 s1 s2))

(defun icicle-part-2-lessp (s1 s2)
  "`icicle-part-N-lessp', with N = 2."
  (icicle-part-N-lessp 2 s1 s2))

(defun icicle-part-3-lessp (s1 s2)
  "`icicle-part-N-lessp', with N = 3."
  (icicle-part-N-lessp 3 s1 s2))

(defun icicle-part-4-lessp (s1 s2)
  "`icicle-part-N-lessp', with N = 4."
  (icicle-part-N-lessp 4 s1 s2))

(defun icicle-color-red-lessp (s1 s2)
  "Non-nil means the RGB in S1 has less red than in S2.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The RGB values are assumed to
be the second parts of the strings, and they are assumed to start with
`#'."
  (unless (featurep 'hexrgb) (error "`icicle-color-red-lessp' requires library `hexrgb.el'"))
  (let ((rgb1 (elt (split-string s1 icicle-list-join-string) 1))
        (rgb2 (elt (split-string s2 icicle-list-join-string) 1)))
    (and rgb1 rgb2 ; Just in case strings were not multipart.
         (< (hexrgb-red rgb1) (hexrgb-red rgb2)))))

(defun icicle-color-green-lessp (s1 s2)
  "Non-nil means the RGB in S1 has less green than in S2.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The RGB values are assumed to
be the second parts of the strings, and they are assumed to start with
`#'."
  (unless (featurep 'hexrgb) (error "`icicle-color-green-lessp' requires library `hexrgb.el'"))
  (let ((rgb1 (elt (split-string s1 icicle-list-join-string) 1))
        (rgb2 (elt (split-string s2 icicle-list-join-string) 1)))
    (and rgb1 rgb2 ; Just in case strings were not multipart.
         (< (hexrgb-green rgb1) (hexrgb-green rgb2)))))

(defun icicle-color-blue-lessp (s1 s2)
  "Non-nil means the RGB in S1 has less blue than in S2.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The RGB values are assumed to
be the second parts of the strings, and they are assumed to start with
`#'."
  (unless (featurep 'hexrgb) (error "`icicle-color-blue-lessp' requires library `hexrgb.el'"))
  (let ((rgb1 (elt (split-string s1 icicle-list-join-string) 1))
        (rgb2 (elt (split-string s2 icicle-list-join-string) 1)))
    (and rgb1 rgb2 ; Just in case strings were not multipart.
         (< (hexrgb-blue rgb1) (hexrgb-blue rgb2)))))

(defun icicle-color-hue-lessp (s1 s2)
  "Non-nil means the RGB hue in S1 is less than that in S2.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The RGB values are assumed to
be the second parts of the strings, and they are assumed to start with
`#'."
  (unless (featurep 'hexrgb) (error "`icicle-color-hue-lessp' requires library `hexrgb.el'"))
  (let ((rgb1 (elt (split-string s1 icicle-list-join-string) 1))
        (rgb2 (elt (split-string s2 icicle-list-join-string) 1)))
    (and rgb1 rgb2 ; Just in case strings were not multipart.
         (< (hexrgb-hue rgb1) (hexrgb-hue rgb2)))))

(defun icicle-color-saturation-lessp (s1 s2)
  "Non-nil means the RGB in S1 is less saturated than in S2.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The RGB values are assumed to
be the second parts of the strings, and they are assumed to start with
`#'."
  (unless (featurep 'hexrgb) (error "`icicle-color-saturation-lessp' requires library `hexrgb.el'"))
  (let ((rgb1 (elt (split-string s1 icicle-list-join-string) 1))
        (rgb2 (elt (split-string s2 icicle-list-join-string) 1)))
    (and rgb1 rgb2 ; Just in case strings were not multipart.
         (< (hexrgb-saturation rgb1) (hexrgb-saturation rgb2)))))

(defun icicle-color-value-lessp (s1 s2)
  "Non-nil means the RGB value in S1 is darker than that in S2.
The strings are assumed to have at least two parts, with the parts
separated by `icicle-list-join-string' The RGB values are assumed to
be the second parts of the strings, and they are assumed to start with
`#'."
  (unless (featurep 'hexrgb) (error "`icicle-color-value-lessp' requires library `hexrgb.el'"))
  (let ((rgb1 (elt (split-string s1 icicle-list-join-string) 1))
        (rgb2 (elt (split-string s2 icicle-list-join-string) 1)))
    (and rgb1 rgb2 ; Just in case strings were not multipart.
         (< (hexrgb-value rgb1) (hexrgb-value rgb2)))))

(defun icicle-prefix-keys-first-p (s1 s2)
  "Non-nil if S1 is a prefix key and S2 is not or S1 < S2 (alphabet).
For this function, a prefix key is represented by a string that ends
in \"...\".

When used as a comparison function for completion candidates, this
makes prefix keys that match your input available first (at the top of
buffer *Completions*).  Candidates are effectively in two groups, each
of which is sorted alphabetically separately: prefix keys, followed by
non-prefix keys.  Letter case is ignored.

The special key representation \"..\" is less than all other keys,
including prefix keys."
  (let* ((prefix-string "  =  \\.\\.\\.$")
         (parent-string "..")
         (s1-prefix-p (save-match-data (string-match prefix-string s1)))
         (s2-prefix-p (save-match-data (string-match prefix-string s2)))
         (completion-ignore-case t))
    (and (not (string= parent-string s2))
         (or (string= parent-string s1)
             (and (not s1-prefix-p) (not s2-prefix-p) (icicle-case-string-less-p s1 s2))
             (and s1-prefix-p (not s2-prefix-p))
             (and s1-prefix-p s2-prefix-p (icicle-case-string-less-p s1 s2))))))

(defun icicle-command-names-alphabetic-p (s1 s2)
  "Non-nil if command name of S1 `icicle-case-string-less-p' that of S2.
When used as a comparison function for completion candidates, this
assumes that each candidate, S1 and S2, is composed of a key name
followed by \"  =  \", followed by the corresponding command name."
  (let ((icicle-list-join-string "  =  ")) ; Fake a multi-completion.  Candidate is key  =  cmd.
    (icicle-part-2-lessp s1 s2)))

(defun icicle-special-candidates-first-p (s1 s2)
  "Non-nil if S1 is special candidate and S2 is not or S1<S2 (alphabet).
That is, S1 < S2 if S1 is a special candidate and S2 is not or S1
`icicle-case-string-less-p' S2 and either both or neither are special
candidates."
  (let ((s1-special (get (intern s1) 'icicle-special-candidate))
        (s2-special (get (intern s2) 'icicle-special-candidate)))
    (when (or case-fold-search completion-ignore-case
              (and (icicle-file-name-input-p) (boundp 'read-file-name-completion-ignore-case)
                   read-file-name-completion-ignore-case))
      (setq s1 (upcase s1) s2 (upcase s2)))
    (or (and s1-special (not s2-special))
        (and s1-special s2-special (icicle-case-string-less-p s1 s2))
        (and (not s1-special) (not s2-special) (icicle-case-string-less-p s1 s2)))))

(defun icicle-case-insensitive-string-less-p (string1 string2)
  "Like `string-lessp', but case is ignored, so `A' = `a' , and so on."
  (string-lessp (upcase string1) (upcase string2)))

(defun icicle-case-string-less-p (s1 s2)
  "Like `string-lessp', but respects `completion-ignore-case'."
  (when (if icicle-completing-p         ; Use var, not fn, `icicle-completing-p', or else too slow.
            (if (and (icicle-file-name-input-p)
                     (boundp 'read-file-name-completion-ignore-case))
                read-file-name-completion-ignore-case
              completion-ignore-case)
          case-fold-search)
    (setq s1 (upcase s1) s2 (upcase s2)))
  ;; $$(when completion-ignore-case (setq s1 (upcase s1) s2 (upcase s2))) ; Alternative.
  (string-lessp s1 s2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-fn.el ends here

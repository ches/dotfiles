;;; else-mode.el --- Emacs Language Sensitive Editor (ELSE)
;;
;; Copyright (C) 1997 - 2006 Peter Milliken
;;
;; Author: Peter Milliken <peterm@resmed.com.au> 
;;                        <peter.milliken@exemail.com.au>
;; Version: 1.21
;; Keywords: language sensitive abbreviation template placeholder token
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This facility is documented in the accompanying distribution file(s).

;;; Change Log:

;;; Code:

;; This package provides line-numbering to the menu buffer (if enabled - see
;; else-enable-lineno custom variable). Encase the conditional load within an
;; error handling struct so that ELSE doesn't blow up on loading.
(condition-case nil
    (require 'setnu)
  (error nil))

;; Only attempt expansion of token abbreviations if the expand-a-word
;; package is available.

;; To allow partial expansion/recognition of tokens, the expand-a-word
;; function is required. 
(defvar else-expand-a-word-available nil)
(condition-case nil
    (progn
      (require 'expand-a-word)
      (setq else-expand-a-word-available t))
  (error 
   (setq else-expand-a-word-available nil)))

;; The following constants are associated keys into the data structure which
;; records the definition of a placeholder/token/language definition. Use the
;; property list of a symbol to store the individual attributes of a
;; definition. The following constants are the "keys" into the property list
;; (see else-read-a-definition) for the setting of the property list
;; information).
;;
(defconst else-placeholder-ref                    0)
(defconst else-type-ref                           1)
(defconst else-separator-ref                      2)
(defconst else-substitute-ref                     3)
(defconst else-duplication-ref                    4)
(defconst else-topic-ref                          5)   ;; Not used
(defconst else-description-ref                    6)
(defconst else-body-ref                           7)
(defconst else-substitute-count-ref               8)
(defconst else-Language-Name                      9)
(defconst else-Punctuation-ref                   10)
(defconst else-Punctuation-Length-ref            11)
(defconst else-Initial-String-ref                12)
(defconst else-Self-Insert-Characters-ref        13)
(defconst else-Self-Insert-Characters-Length-ref 14)
(defconst else-Valid-Idents-ref                  15)
(defconst else-Valid-Idents-Length-ref           16)
(defconst else-tab-size-ref                      17)
(defconst else-original-name                     18)
(defconst else-language-version-ref              19)
(defconst else-elisp-action                      20)


(defconst else-before-key "/BEFORE"
  "association list key into the run-time action sequences.")

(defconst else-after-key "/AFTER"
  "association list key into the run-time action sequences.")

(defconst else-oninsert-key "/ONINSERT"
  "association list key into the run-time action sequences.")

(defconst else-Extract-Column 4)

(defconst else-Placeholder-Vector-Size 1023)
(defconst else-Token-Vector-Size       512)

;; XEmacs behaves differently in the before/after change functions than FSF
;; Emacs. The following three variables are used in the XEmacs code. All XEmacs
;; code is differentiated using a 'cond' statement and the
;; 'else-in-xemacs variable setting.
(defvar else-move-change nil)
(defvar else-move-to-position nil)
(defvar else-in-xemacs (integerp (string-match "XEmacs\\|Lucid" (emacs-version))))

;; Do some compatability between XEmacs and FSF Emacs - hopefully can
;; remove this one day when XEmacs matches FSF? If not, then it
;; doesn't matter as the issue will work either way.
(unless (boundp 'undo-in-progress)
  (defvar undo-in-progress nil
   "Indicate that an indo is in progress to before/after change fns.")
  (defadvice undo-more (around else-undo-more activate compile)
     (let ((undo-in-progress t)) ad-do-it)))

;; The following constants define the offsets for the nth and nthcdr
;; functions into the data element which records each line of the 'body' of a
;; placeholder/token definition. The meanings are:
;;
;; else-body-type-ref            - if menu line then /placeholder or /token (?p
;;                                 ?t or nil) 
;; else-body-indent-ref          - indentation 'level'
;; else-body-text-ref            - actual text of line
;; else-body-menu-follow-on-ref - if an item is a menu then the user can
;;                                selectively disable/enable follow-on in the
;;                                menu display. See also else-follow-menus.
(defconst else-body-type-ref           0) 
(defconst else-body-indent-ref         1)
(defconst else-body-text-ref           2)
(defconst else-body-menu-follow-on-ref 3)

;; The following constants are the regular expressions and the corresponding
;; `match-data' offsets that are used in else. These are general expressions
;; that are constructed as follows:
;; `defining' construct:
;; Each `defining' construct is one of (where []'s represent alternatives and
;; (digit) represent the match-data offset):
;;
;;              [LANGUAGE   ]
;;   DELETE]____[TOKEN      ]_____["some text enclosed by quotes"
;;   DEFINE]    [PLACEHOLDER]     [text not enclosed by quotes (no spaces).
;;     (1)         (2)                   (3)
;;
;; Examples are:
;;
;; (a)  DEFINE PLACEHOLDER "THIS IS A TEMPLATE"
;;                  or
;; (b)  DELETE LANGUAGE ADA
;;
;; For example (a), the match-data would be (1) "DEFINE"
;;                                          (2) "PLACEHOLDER"
;;                                          (3) "\"THIS IS A TEMPLATE\""
;;
;;             (b), the match-data would be (1) "DELETE"
;;                                          (2) "LANGUAGE"
;;                                          (3) "ADA"
;;
;; `body' construct:
;; Each `body' construct is one of (where []'s represent alternatives and
;; (digit) represent the match-data offset):
;;
;;   ]/text=]_____________________________["some text in quotes"
;;   ]      ]                             [text not enclosed by quotes
;;   ]      ]_____________________________["some text in quotes"      ]____________/text
;;   ]      ]                             [text not enclosed by quotes]
;;   ]
;;   ]"some text enclosed by quotes"______[/text
;;   ]                                    [or nothing.
;;   ]/text-
;;   ]
;;   END DEFINE
;;     (1), (3), (5) or (6)                 (2) or (4)
;;
;;
;;  Examples are:
;;
;;  (a) /LANGUAGE=ADA -
;;
;;  (b) /NOAUTOSUBSTITUTE -
;;
;;  (c) /SEPARATOR=", " -
;;
;;  (d) "null"/PLACEHOLDER
;;
;;  (e) "enter a valid number"
;;
;;  (f) END DEFINE
;;
;;  For example (a), the match-data would be (1) "/LANGUAGE="
;;                                           (2) "ADA"
;;                                           (3) nil
;;                                           (4) nil
;;                                           (5) nil
;;                                           (6) nil
;;
;;  For example (b), the match-data would be (1) nil
;;                                           (2) nil
;;                                           (3) nil
;;                                           (4) nil
;;                                           (5) "/NOAUTOSUBSTITUTE"
;;                                           (6) nil
;;
;;  For example (c), the match-data would be (1) "/SEPARATOR="
;;                                           (2) ", "
;;                                           (3) nil
;;                                           (4) nil
;;                                           (5) nil
;;                                           (6) nil
;;
;;  For example (d), the match-data would be (1) nil
;;                                           (2) nil
;;                                           (3) "null"
;;                                           (4) "/PLACEHOLDER"
;;                                           (5) nil
;;                                           (6) nil
;;
;;  For example (e), the match-data would be (1) nil
;;                                           (2) nil
;;                                           (3) "enter a valid number"
;;                                           (4) nil
;;                                           (5) nil
;;                                           (6) nil
;;
;;  For example (f), the match-data would be (1) nil
;;                                           (2) nil
;;                                           (3) nil
;;                                           (4) nil
;;                                           (5) nil
;;                                           (6) END DEFINE
;;

(defconst else-defining-string
  "\\(DELETE\\|DEFINE\\) +\\(TOKEN\\|PLACEHOLDER\\|LANGUAGE\\) +\\(\".*\"\\|[ -~]+\\) +-")
(defconst else-defining-command 1)
(defconst else-defining-type 2)
(defconst else-defining-name 3)

(defconst else-body-string
  "\\(/[A-Z_]+\\) *= *\\(\".*\"\\|[-a-zA-Z_0-9\\.]+\\)\\([/A-Z_]+\\)*\\|\\(/[A-Z_]+\\) *-\\|\\(\".*\"\\) *\\(/[A-Z_]+\\)?\\(/NOFOLLOW\\|/FOLLOW\\)?\\|\\(END *DEFINE\\)")

(defconst else-body-command-1 1)
(defconst else-body-command-2 2)
(defconst else-body-command-3 3)
(defconst else-body-command-4 4)
(defconst else-body-text-1 5)
(defconst else-body-text-2 6)
(defconst else-body-text-3 7)
(defconst else-body-end 8)

(defconst else-template-comment "^;;.*"
  "Comment lines must start at the line beginning with ;;")

(defvar else-deleted-string   nil)      ; The last placeholder text that is
                                        ; currently being processed.
(defvar else-definition-name  nil)      ; string of the definition being
                                        ; processed i.e. identifier.
(defvar else-fallback-text    nil)      ; The full original deleted text.
(defvar else-deleted-column   0)        ; Column from which the deleted string
(defvar else-please-duplicate nil)      ; t when the placeholder was followed
                                        ; by ellipses.

;; This is a flag which is used by the before and after change functions. It
;; transfers information about whether the "auto-substitute" mode has just gone
;; active to the after change function. This is because the after change
;; function receives a confusing begin/end pair (because the before change
;; function has deleted the placeholder) so it can't work out what has
;; happened. This flag being set t means that it interprets the change as a
;; single character insert.
(defvar else-sub-active-toggle nil)

(defvar else-Mandatory-Placeholder nil) ; Flag used to indicate if current
                                        ; placeholder is mandatory {} or
                                        ; optional [].

(defvar else-placeholder-start 0
  "Start position of the placeholder/token being processed")

(defvar else-placeholder-end   0
  "End position of the placeholder/token being processed")

(defvar else-definition-type nil
  "Definition type of the element bounded by else-placeholder-start/end")

(defvar else-Auto-Sub-Active nil
  "Used to determine if an auto-substitute string is Active")

(defvar else-Auto-Sub-Marker-List nil
  "A list of paired Auto Sub Markers, this list grows or shrinks as required.")

(defvar else-Language-Definitions '(("Empty" . nil))
  "alist containing all the language definitions.")

(defvar else-Current-Language nil
  "Holds the Language Identification string for the current buffer ie \"Ada\"")

(defvar else-menu-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "q" 'else-menu-quit)
    (define-key map "s" 'else-menu-select)
    (define-key map "Q" 'else-menu-quit)
    (define-key map "S" 'else-menu-select)
    (define-key map " " 'else-menu-next-line)
    (define-key map "n" 'else-menu-next-line)
    (define-key map "p" 'else-menu-previous-line)
    (define-key map "N" 'else-menu-next-line)
    (define-key map "P" 'else-menu-previous-line)
    (define-key map "?" 'else-summary)
    (define-key map "\r" 'else-menu-select)
    map )
  "Local keymap for else-mode.")

(defvar else-mode-key-map
  (let ((mode-map (make-sparse-keymap))
        (prefix-map (make-sparse-keymap))
        (command-map (make-sparse-keymap)))
    (define-key mode-map "\C-c" prefix-map)
    (define-key prefix-map "/" command-map)
    (define-key command-map "e" 'else-expand-placeholder)
    (define-key command-map "k" 'else-kill-placeholder)
    (define-key command-map "n" 'else-next-placeholder)
    (define-key command-map "p" 'else-previous-placeholder)
    (define-key command-map "i" 'else-insert-placeholder)
    (define-key command-map "c" 'else-comment-placeholders)
    (define-key command-map "u" 'else-uncomment-placeholders)
    mode-map))

(defvar else-selected-text nil
  "string selected by the user when multiple choices in the LSE word function are
available" )

(defvar else-type-of-item-being-expanded ?p
  "'type' of item being expanded ie either a placeholder or a token")

(defvar else-mode nil
  "The minor mode flag")

(defvar else-current-definition nil
  "Current definition as found by else-in-placeholder")

;; The following variables need to be local to each buffer in which they are
;; used.
(make-variable-buffer-local 'else-Auto-Sub-Active)
(make-variable-buffer-local 'else-Auto-Sub-Marker-List)
(make-variable-buffer-local 'else-Current-Language)
(make-variable-buffer-local 'else-mode)
(make-variable-buffer-local 'else-move-change)
(make-variable-buffer-local 'else-move-to-position)
(make-variable-buffer-local 'else-current-definition)
(make-variable-buffer-local 'else-please-duplicate)
(make-variable-buffer-local 'else-deleted-column)
(make-variable-buffer-local 'else-deleted-string)
(make-variable-buffer-local 'else-definition-type)
(make-variable-buffer-local 'else-definition-name)
(make-variable-buffer-local 'else-Mandatory-Placeholder)

(setq-default else-mode nil)

(defvar Placeholder ()
  "Array holding the `placeholder' tokens for the current language")
(defvar Token ()
  "Array holding the `token' tokens for the current language" )
(defvar Language-Specifics ()
  "Structure that holds the language definition information for the current
language")

(defvar Language-Self-Insert-Characters-Vector
  "Vector to look-up whether a character is 'self-insert'.
Contains True or False (t or nil) and is indexed by character code")

(defvar Language-Valid-Identifier-Characters-Vector
  "Vector to look-up whether a character is a 'valid identifier'.
Contains True or False (t or nil) and is indexed by character code")

(defvar Language-Punctuation-Characters-Vector
  "Vector to look-up whether a character is a 'punctuation' character.
Contains True or False (t or nil) and is indexed by character code")


(defconst else-character-vector-length 256
  "Language character vectors length")


;;; Now make these variables buffer local. At the moment, these are just
;;; buffer local copies that refer to the full language templates, ideally, we
;;; would want to offer the facility of being able to customise the templates
;;; of a language for each buffer ie in one program you might be doing a lot
;;; of case statements and in another buffer there may be more 'if' statements
;;; so the user may decide it would be nice to have two versions of the
;;; STATEMENT placeholder. This is currently not available but could easily be
;;; added. At the moment, change one and all buffers are affected.
(make-variable-buffer-local 'Placeholder)
(make-variable-buffer-local 'Token)
(make-variable-buffer-local 'Language-Specifics)
(make-variable-buffer-local 'Language-Self-Insert-Characters-Vector)
(make-variable-buffer-local 'Language-Valid-Identifier-Characters-Vector)
(make-variable-buffer-local 'Language-Punctuation-Characters-Vector)

;; The following variables are used purely for the fast save/load process
;; (else-compile-fast-load and else-restore).

;; This variable is used to communicate the "type" of symbols being processed
;; during an ELSE compile for fast loading operation ie else-compile-fast-load
;; and else-store-element use this variable to communicate what the element
;; type that is being "stored". This is only necessary because the mapatoms
;; process doesn't allow more that the one argument to the called routine.
(defvar else-type-of-symbols ?p)

;; This is the marker that tracks the "read" process used by
;; else-compile-fast-load and else-restore. We use a marker into the restore
;; file because the 'read' function automatically increments the marker as it
;; reads each line. Again, the variable is only required to be global because
;; of the use of 'mapatoms'.
(defvar else-read-marker nil)

(defconst else-lse-ext "\.lse")
(defconst else-esl-ext "\.esl")

(defvar else-placeholder-overlay nil
  "Placeholder overlay is used by the Voice Coder modifications to ELSE.")

;; This is the after change function. Its primary purpose is to act in
;; situations where the auto-substitute function is active and the change has
;; occurred in the specified region. It must repeat the change into each of the
;; auto-substitute "pair"s.
(defun else-after-change (begin end length)
  (let ((marker-index)
        (move-text)
        (local-begin begin)
        (local-end end))

    ;; Only duplicate any changes if the change has occurred within the auto-sub
    ;; area.
    (setq else-after-var (append (list begin end length
                                       (symbol-name this-command))
                                 else-after-var))
    ;; Xemacs changes here
    (cond (else-in-xemacs
           (if else-move-change
               (progn
                 (setq local-begin (marker-position else-move-to-position))
                 (setq local-end (+ local-begin (- end begin)))
                 (goto-char local-begin)
                 (setq move-text (buffer-substring begin end))
                 (delete-region begin end)
                 ;; At this point we need to 'modify' the undo list for the
                 ;; buffer i.e. to allow the user to perform a seamless undo, we
                 ;; have to delete the events that have just occurred e.g. the
                 ;; insert of text into the buffer at the 'wrong' place and the
                 ;; subsequent deleting of that text. The only event we wish to
                 ;; remain in the undo list is the insertion of the text into
                 ;; the 'correct' place in the buffer (which will be
                 ;; accomplished directly after we remove the two mentioned
                 ;; events).
                 (setq buffer-undo-list (cdr (cdr buffer-undo-list)))
                 ;; Now insert the text where it should have originally
                 ;; gone. Note that this is the event that will be recorded in
                 ;; the buffer unto list.
                 (insert move-text)
                 (setq else-move-change nil)))))

    (if (and (not undo-in-progress)
             else-Auto-Sub-Active
             (>= local-begin (marker-position (car (nth 1 else-Auto-Sub-Marker-List))))
             (< local-end (marker-position (cdr (nth 1 else-Auto-Sub-Marker-List)))))
        (progn
          (save-excursion
            (if else-sub-active-toggle
                (progn
                  (setq else-sub-active-toggle nil)))
            ;; want to iterate over all of the active markers in the marker
            ;; list. This starts at the second entry in the marker list.
            (setq marker-index 2)
            (while (and (marker-position
                         (car (nth marker-index
                                   else-Auto-Sub-Marker-List)))
                        (<= marker-index (car else-Auto-Sub-Marker-List)))
              (delete-region (marker-position
                              (car (nth marker-index
                                        else-Auto-Sub-Marker-List)))
                             (1- (marker-position
                                  (cdr (nth marker-index
                                            else-Auto-Sub-Marker-List)))))
              (goto-char (marker-position
                          (car (nth marker-index
                                    else-Auto-Sub-Marker-List))))
              (insert (buffer-substring (marker-position
                                         (car (nth 1
                                                   else-Auto-Sub-Marker-List)))
                                        (1- (marker-position
                                             (cdr (nth 1
                                                       else-Auto-Sub-Marker-List))))))
              (setq marker-index (1+ marker-index))))))))

(defun else-after-token ()
  "Test if string preceeding point is a valid token."
  (let ((here (point))
        (result nil))
    (save-excursion
      (setq match-scan (format "\\([^%s]+\\)\\|\\(^\\)" 
                               (cdr (assoc else-Valid-Idents-ref 
                                           Language-Specifics))))
      (if (else-scan-for-match match-scan nil t)
          (if (not (= (point) here))
              (progn
                ;; We have found a (potential) token string, have to make a
                ;; small adjustment here for detected white-space. Note that no
                ;; adjustment required if we matched the beginning of line (^)
                ;; (that's why I used the \\( groupings)
                (if (match-string 1)
                    (forward-char))
                (if (else-look-up (buffer-substring
                                   (point)
                                   here) ?t)
                    (progn
                      ;; We have a valid token, so save the start and end of the
                      ;; text that was matched.
                      (setq else-placeholder-start (point))
                      (setq else-placeholder-end here)
                      (setq else-definition-type ?t)
                      ;; No duplication possible....
                      (setq else-please-duplicate nil)

                      (setq else-definition-name (buffer-substring
                                                  (point) here))

                      ;; Get the definition while we are here....
                      (setq else-current-definition (else-look-up
                                                     else-definition-name
                                                     else-definition-type))
                      (setq result t)))))))
    result))

;; These variables are used for diagnostic purposes only. They will eventually
;; (?) disappear once I am convinced the before and after change code has all
;; the wrinkles removed.
(defvar else-before-var nil)
(defvar else-after-var nil)
(defvar zero-diff 0)
(defvar zero-begin 0)
(defvar zero-end 0)

;; The Placeholder and Token variables are obarrays of considerable
;; length. Rather than take up space in Emacs memory when ELSE is not active,
;; I don't bother initialising them until they are required. This routine is
;; called by the language loading procedures.
(defun else-check-and-init-globals (language)
  "If language doesn't exist, create a set of variables for it."
  (let ((result t))
    (if (not (assoc language else-Language-Definitions))
        (progn
          (setq result nil)
          (setq Placeholder
                (make-vector else-Placeholder-Vector-Size 0))
          (setq Token (make-vector else-Token-Vector-Size 0))
          (setq Language-Specifics
                (list (cons else-Language-Name           "")
                      (cons else-Punctuation-ref         "")
                      (cons else-Punctuation-Length-ref  0)
                      (cons else-Initial-String-ref  "")
                      (cons else-Self-Insert-Characters-ref "")
                      (cons else-Self-Insert-Characters-Length-ref 0)
                      (cons else-Valid-Idents-ref    "")
                      (cons else-Valid-Idents-Length-ref 0)
                      (cons else-tab-size-ref 4)
                      (cons else-language-version-ref "")))
          ;; Put the new variable set into the global structure.
          (setq else-Language-Definitions
                (cons
                 (cons language
                       (list Placeholder Token Language-Specifics))
                 else-Language-Definitions))))
    result))

;;
;; Function that runs as part of the before change functions. Its main job is
;; to determine whether the command is a "self-insert" and if so, whether the
;; cursor is within a placeholder, if these conditions are true then it must
;; delete the placeholder before allowing the command to proceed. It also
;; checks whether the placeholder is part of an auto-substitute pairing and if
;; so, sets up the appropriate global variables for use by the after change
;; function.
(defun else-before-change (begin end)
  (let ((data (match-data))
        (is-auto-sub)
        (this-pos)
        (this-column)
        (vert-dup)
        (valid-search)
        (action-struct)
	(marker-index)
        (sub-marker-counter)
        (sub-marker-counter-limit))
    (progn
      (cond (else-in-xemacs
             ;; Make sure that the flag is reset i.e. it is possible
             ;; (but unlikely) that the variable was not cleared for
             ;; some reason by the after change function. This is just
             ;; a precaution...
             (setq else-move-change nil)))
      (save-excursion
        ;; Ideally we want to detect any advertised-undo commands at
        ;; this point so that we don't try and duplicate changes in
        ;; the after-change function. Unfortunately, command
        ;; processing has apparently set the "this-command" variable
        ;; to "t" ie it is useless to detect the change here - we will
        ;; use the undo-in-progress flag in the after-change-function
        ;; to handle things - thanks to Stefan Monnier for this tip.
        (setq else-before-var (append (list begin end
                                            (symbol-name this-command))
                                      else-before-var))
        (setq zero-begin (append (list begin end) zero-begin))
        (if (equal 0 (- end begin))
            (progn
              (setq zero-diff (+ 1 zero-diff))
              (setq zero-begin 0)))

        ;; check if the change is a keystroke i.e. a "self-insert"
        ;; character/command. We detect this by checking if the length of the
        ;; keystrokes is 1 e.g. a single keypress will result in a single length
        ;; key vector from the "this-command-keys-vector" function.  This is a
        ;; "safe" test because any "and" will "short-circuit" on the first
        ;; "evaluation" that equates to "nil". i.e. the compare-strings won't
        ;; "happen" unless the length of the key vector is 1.
        (setq zero-diff (1+ zero-diff))

        ;; Only process changes in a placeholder if the change region is not
        ;; exactly the boundaries of an existing placeholder i.e. commands such
        ;; as insert register etc will delineate begin/end as being outside the
        ;; start/end of a placeholder whereas a simple insert will have a
        ;; begin/end pairing within these boundaries. Innocuous changes such as
        ;; text properties on a placeholder will have the begin/end pair
        ;; matching the else-placeholder-start/else-placeholder-end and so we
        ;; should(?) ignore them.
        (if (and (else-in-placeholder)
                 (not (= begin else-placeholder-start))
                 (not (= end else-placeholder-end)))
            (progn
              ;; Check if the template definition contains an "oninsert" elisp
              ;; action - call it if it does. 
              (condition-case err
                  (progn
                    (setq action-struct (get else-current-definition 
                                             'else-elisp-action))
                    (if (assoc else-oninsert-key action-struct)
                        (funcall (intern-soft 
                                  (cdr (assoc else-oninsert-key
                                              action-struct))))))
                (void-function
                 (message "Symbol's function definition is void: %s" 
                          (cdr (assoc else-oninsert-key action-struct))))
                (error 
                 (message "%s" (error-message-string err))))
              
              (setq zero-diff (1+ zero-diff))
              (setq is-auto-sub (char-equal (get else-current-definition 
                                                 'else-substitute-ref)
                                            ?a))
              ;; In a placeholder it is either an auto-substitute or
              ;; not, in either case  clear out the substitution
              ;; markers.
              (setq sub-marker-counter-limit
                    (car else-Auto-Sub-Marker-List))
              (setq sub-marker-counter 1)
              (while (< sub-marker-counter sub-marker-counter-limit)
                (set-marker (car (nth sub-marker-counter
                                      else-Auto-Sub-Marker-List))
                            nil)
                (set-marker (cdr (nth sub-marker-counter
                                      else-Auto-Sub-Marker-List))
                            nil)
                (setq sub-marker-counter (1+ sub-marker-counter)))
              ;; The markers aren't "active" yet, must make sure that
              ;; the secondary markers/placeholders are present!
              (setq else-Auto-Sub-Active nil)
              (if is-auto-sub
                  (progn
                    (set-marker (car (nth 1 else-Auto-Sub-Marker-List))
                                else-placeholder-start)
                    (set-marker (cdr (nth 1 else-Auto-Sub-Marker-List))
                                (1+ else-placeholder-end))))

              ;; XEmacs change - once again not guarded because harmless to FSF
              ;; operation. XEmacs has different behaviour wrt a before-change
              ;; function altering a buffer. The FSF Emacs seems to perform the
              ;; change at the position that 'point' is after exiting the
              ;; before-change-function (despite whatever changes occurred in the
              ;; function) but XEmacs does not i.e. if you delete X characters
              ;; then the point at which a self-insert character is placed will
              ;; be "offset" by that length i.e. it will go into the buffer at
              ;; the wrong place which is the offset from 'point from the start
              ;; of the placeholder. So we provide the after change function
              ;; with an indication of where the change should have occurred...
              (set-marker else-move-to-position else-placeholder-start)
              (setq else-move-change t)

              ;; Check if this is an "auto-substitute" placeholder, if so then
              ;; set a flag and create some markers to where the placeholder
              ;; substitution is required. Note that the front and back markers
              ;; are used in later invocations of this function to determine
              ;; whether the cursor and the action taken should be duplicated in
              ;; the auto-substitute string.
              ;;
              (if is-auto-sub
                  (save-excursion
                    (goto-char else-placeholder-end)
                    ;; Set up all of the after change sub markers. The
                    ;; number is controlled by the number of counts in
                    ;; the placeholder definition. We increment the
                    ;; following limit number because of list contains
                    ;; a counter as the first element.
                    (setq sub-marker-counter-limit
                          (1+
                           (get else-current-definition
                                'else-substitute-count-ref)))
                    ;; Check whether the total number of markers will
                    ;; be adequate to meet the needs of the
                    ;; saubstitution count - if not then 'grow' the
                    ;; list here as the easiest point to do it.
                    (if (> (1- sub-marker-counter-limit)
                           (- (car else-Auto-Sub-Marker-List) 2))
                        ;; Grow the list by the required number....
                        (progn
                          (let ((count-increase 
                                 (- sub-marker-counter-limit
                                    (- (car else-Auto-Sub-Marker-List)
                                       2)
                                    1)))
                            (while (> count-increase 0)
                              (setq else-Auto-Sub-Marker-List
                                    (append else-Auto-Sub-Marker-List
                                            (list (cons (make-marker) 
                                                        (make-marker)))))
                              (setq count-increase (1- count-increase)))

                            (setcar else-Auto-Sub-Marker-List
                                    (length (cdr else-Auto-Sub-Marker-List))))))

                    (setq valid-search t)
                    ;; Note that the first pair of markers have
                    ;; already been used to capture the master text
                    ;; area, so start at the second set of pairs.
                    (setq sub-marker-counter 2)

                    ;; Save the global placeholder variables because
                    ;; kill-placeholder is used within the following
                    ;; FORMS
                    (let ((current-definition else-current-definition)
                          (please-duplicate else-please-duplicate)
                          (deleted-string else-deleted-string)
                          (definition-type else-definition-type)
                          (definition-name else-definition-name)
                          (placeholder-start else-placeholder-start)
                          (placeholder-end else-placeholder-end)
                          (mandatory-placeholder else-Mandatory-Placeholder))
                      (while (and valid-search
                                  (<= sub-marker-counter
                                      sub-marker-counter-limit))
                        ;; Find the matching string, arguments mean
                        ;; search to end of buffer and don't error if
                        ;; search fails.
                        (if (search-forward else-definition-name nil t)
                            (progn
                              ;; position back into the placeholder
                              ;; and then kill it (even if it is
                              ;; mandatory).
                              (backward-char)
                              ;; Next call just sets up all of the
                              ;; global variables so that the text can
                              ;; be deleted - don't use a
                              ;; kill-placeholder here because it
                              ;; would destroy the information that is
                              ;; required to be kept.....
                              (else-in-placeholder)
                              (if else-please-duplicate
                                  (delete-region else-placeholder-start 
                                                 (+ else-placeholder-end 3))
                                (delete-region else-placeholder-start 
                                               else-placeholder-end))

                              (if (> sub-marker-counter
                                     (car else-Auto-Sub-Marker-List))
                                  (progn
                                    ;; we need to grow the list
                                    (error 
                                     "growth of sub marker list not implemented")
                                    )
                                (set-marker (car 
                                             (nth sub-marker-counter 
                                                  else-Auto-Sub-Marker-List))
                                            (point))
                                (set-marker (cdr 
                                             (nth sub-marker-counter 
                                                  else-Auto-Sub-Marker-List))
                                            (1+ (point)))))
                          (setq valid-search nil))

                        (setq sub-marker-counter (1+ sub-marker-counter)))

                      ;; Restore the global variables.
                      (setq else-current-definition current-definition)
                      (setq else-please-duplicate please-duplicate)
                      (setq else-deleted-string deleted-string)
                      (setq else-definition-type definition-type)
                      (setq else-definition-name definition-name)
                      (setq else-placeholder-start placeholder-start)
                      (setq else-placeholder-end placeholder-end)
                      (setq else-Mandatory-Placeholder mandatory-placeholder))
                    
                    ;; This is not legacy code
                    (setq else-Auto-Sub-Active t)
                    
                    ;; Set the substitute "toggle" flag. This variable
                    ;; is only used the first time that the
                    ;; placeholder is deleted. It is used in the
                    ;; "after" change function because the begin/end
                    ;; pair is "screwed" up by what is happening here
                    ;; ie it receives a "begin" which is after the
                    ;; "end" and the difference is the difference
                    ;; between the beginning of the placeholder string
                    ;; and the current cursor position within the
                    ;; placeholder.
                    (setq else-sub-active-toggle t)))

              (if else-please-duplicate
                  (delete-region else-placeholder-start 
                                 (+ else-placeholder-end 3))
                (delete-region else-placeholder-start else-placeholder-end))

              (setq else-deleted-column (current-column))

              ;; if the placeholder is/was trailed by a duplication request
              ;; indicator (...) then replicate it not only in the primary
              ;; location but also in any repeat/auto-substitute locations.
              (if else-please-duplicate
                  (save-excursion
                    (setq this-pos (point))
                    ;; Work out the duplication requirements only if the
                    ;; placeholder definition is context_dependent i.e. it
                    ;; is not being overridden explicitly in the
                    ;; definition. 
                    (setq vert-dup (get else-current-definition 
                                        'else-duplication-ref))
                    (if (char-equal vert-dup ?c)
                        (if (not (else-scan-for-match "[^ \t]" nil t))
                            (setq vert-dup ?v)
                          (setq vert-dup ?h)))

                    (goto-char this-pos)
                    (else-replicate-placeholder-string vert-dup
                                                       else-deleted-column
                                                       else-current-definition)
                    ;; Now lets replicate the auto-sub placeholders - this would
                    ;; be considered to be an unusual event but may be required
                    ;; in the case of duplication of parameters etc of
                    ;; function/procedure calls as might be desired by languages
                    ;; that allow/require a declaration and a definition.
                    (setq marker-index 2)
                    (while (and (marker-position
                                 (car (nth marker-index
                                           else-Auto-Sub-Marker-List)))
                                (<= marker-index (car
                                                  else-Auto-Sub-Marker-List)))
                      ;; Position at the start of where the original placeholder
                      ;; was.
                      (setq this-pos (marker-position (car (nth marker-index
                                                           else-Auto-Sub-Marker-List))))

                      ;; Have to calculate the column number for the call to
                      ;; replicate....
                      (goto-char this-pos)
                      (beginning-of-line)
                      (setq this-column (- this-pos (point)))
                      (goto-char this-pos)
                      ;; Now do the replication
                      (else-replicate-placeholder-string vert-dup
                                                         this-column
                                                         else-current-definition)
                      (setq marker-index (1+ marker-index))))))

          (progn
            ;; This is the point where we have decided that we are not in
            ;; a placeholder, so therefore it is not a situation of
            ;; deleting any placeholders etc. for an insertion.
            ;;
            ;; Check whether auto-substitute should be cancelled (if
            ;; active).
            (if else-Auto-Sub-Active
                (progn
                  ;; OK, need to check whether auto-sub mode should
                  ;; remain active.
                  (if (not
                       (and (>= begin
                                (car (nth 1 else-Auto-Sub-Marker-List)))
                            (<= end
                                (cdr (nth 1 else-Auto-Sub-Marker-List)))))
                      (setq else-Auto-Sub-Active nil))))))

        ;; The 'else' case of this not being a self-insert (basically). This is
        ;; a real problem because just deleting the placeholder can cause
        ;; serious problems for some commands i.e. 'erase-buffer after
        ;; performing a kill-placeholder will crash Emacs! There is insufficient
        ;; information in the Elisp manual about what you can and cannot do in a
        ;; before-change-function, so the following code was used extensively
        ;; (well, it was active for some considerable time :-)) in my edit
        ;; sessions with no bad side-effects - so it must be OK?
        (if (else-in-placeholder)
            ;; There is at least one command, erase-buffer, that doesn't like
            ;; the contents of the buffer being changed. So, limit the
            ;; execution of the kill-placeholder command to the situation
            ;; where it only occurs if the (begin . end) change area is within
            ;; the limits of the placeholder.
            (if (and (> begin else-placeholder-start)
                     (< begin else-placeholder-end))
                ;; There doesn't seem to be any reason to check the 'end
                ;; variable i.e. commands like insert-register, kill-line etc
                ;; seem to have only a single variable in the 'begin.
                (progn
                  (else-kill-placeholder t t t)
                  (set-marker else-move-to-position else-placeholder-start)
                  (setq else-move-change t))))))

    ;; restore the match data
    (set-match-data data)))

(defun else-check-language-file (path-name)
  "Check if the file contains a fast load version and verify validity.
Validity checking entails the date stamps on the .esl file versus the time
stamp on the .lse file. A warning is issued if the .esl version is younger than
the .lse version ie the fast load version should be recompiled."
  (let ((file-name)
        (split-name)
        (file-name-with-esl)
        (file-name-with-lse)
        (fast-mod-time)
        (slow-mod-time))
    ;; Split the path up into path+name and extension.
    (setq split-name (split-string path-name "\\."))
    (if (= (length split-name) 2)
        (progn
          (setq file-name (car split-name)))
      ;; Otherwise, no extension available.
      (setq file-name path-name))
    ;; Next check the timestamps of the two versions of the files.
    (setq file-name-with-esl (concat file-name else-esl-ext))
    (setq file-name-with-lse (concat file-name else-lse-ext))
    (if (and (file-exists-p file-name-with-esl)
             (file-exists-p file-name-with-lse))
        (progn
          (setq fast-mod-time
                (nth 5 (file-attributes file-name-with-esl)))
          (setq slow-mod-time
                (nth 5 (file-attributes file-name-with-lse)))
          (if (or (> (car slow-mod-time) (car fast-mod-time))
                  (and (= (car slow-mod-time) (car fast-mod-time))
                       (> (cadr slow-mod-time) (cadr fast-mod-time))))
              (message "%s.esl is older than %s.lse"
                       file-name file-name))))))

;; Clean up (delete) all lines that contain placeholders. This would be the
;; final step after all coding is complete.
(defun else-cleanup-placeholders ()
  "Delete every _line_ in the buffer containing a valid placeholder.
Note the emphasis on _line_, so be careful :-)."
  (interactive)
  (let ()
    (progn
      (save-excursion
        (goto-char (point-min))
        (while (not (= (point) (else-next-placeholder)))
          (else-kill-placeholder nil t))))))


;; Rip thru' the buffer (or narrowed region) and place comment characters
;; on each line that has a valid placeholder. Use the 'comment-region
;; function, so end comments will be placed as well.
(defun else-comment-placeholders ()
  "Comments out every line in the buffer containing a valid placeholder.
This function uses the 'comment-region' function to achieve this miracle of
modern science."
  (interactive)
  (let ((region-start)
        (region-end))
    (progn
      (save-excursion
        (goto-char (point-min))
        (while (not (= (point) (else-next-placeholder)))
          (progn
            (beginning-of-line)
            (setq region-start (point))
            (end-of-line)
            (setq region-end (point))
            (comment-region  region-start region-end)
            (end-of-line)))))))

;;
;; "Compile" the language template definitions in the current buffer.
;; Processes from `point' to the end of the buffer.
(defun else-compile-buffer (&optional start-at-point-min)
  "Compile the language template definitions from 'point' to the end."
  (interactive "P")
  (let ((command)
        (err-msg nil)
        (here (point))
        ;; User may not like UPPER-CASE in the template file - sigh......
        (case-fold-search t))
    (setq err-msg
          (catch 'compile
            (if start-at-point-min
              (goto-char (point-min)))
            (while (not (= (point) (point-max)))
              (progn
                ;;Only look if the line isn't a comment line
                (if (not (else-scan-for-match else-template-comment nil))
                    (if (else-scan-for-match else-defining-string nil)
                        (progn
                          ;; Get the command, either a DELETE or a DEFINE
                          (setq command (match-string else-defining-command))
                          (cond ((string= command "DELETE")
                                 ;; Actions to delete a definition
                                 (else-delete-a-definition))
                                ((string= command "DEFINE")
                                 ;; Actions to define a definition
                                 (else-read-a-definition))
                                ((string= command ";;")
                                 (setq command 1))
                                (t
                                 ()))))
                  ;; if-else to template comment string search - do nothing
                  ))
              (forward-line))))

    ;; If there has been an error then leave point at the error,
    ;; otherwise return the user to the starting point.
    (if err-msg
        (progn
          (message "Aborted - %s" err-msg)
          nil)
      (goto-char here)
      t)))

(defun else-compile-fast-load (language-file-name)
  "Make an Emacs Lisp loadable file from a standard language template file.
The resulting file has a .esl extension as opposed to the .lse extension
of a normal language template file."
  (interactive "fName of language file:")
  (let ((file-ext-start)
        (compiled-file-name)
        (language-input-buffer)
        (result nil)
        (auto-mode-alist nil)
        (else-lse-ext-search-string (concat "\\" else-lse-ext)))

    ;; Check if the file contains the ELSE file extension.
    (setq file-ext-start 
          (string-match else-lse-ext-search-string language-file-name))
    (if (not file-ext-start)
        ;; Take the easy out here and just tack on the .lse extension.
        (setq language-file-name (append language-file-name else-lse-ext)))
    ;; Check if the file exists
    (if (file-exists-p language-file-name)
        (progn
          ;; locate the start of the normal file extension because we have to
          ;; construct a file name using the new file extension.
          (setq file-ext-start 
                (string-match else-lse-ext-search-string language-file-name))

          ;; Copy the entire language file name to the place where the
          ;; compiled version of the name will be created.
          (setq compiled-file-name (substring language-file-name 0))
          ;; create the compiled version
          (store-substring compiled-file-name file-ext-start else-esl-ext)

          (save-excursion
            ;; Read the language template into a buffer and "compile" it.
            (setq language-input-buffer
                  (find-file-noselect language-file-name))
            (set-buffer language-input-buffer)
            (setq result (else-compile-buffer))
            (if result
                (progn
                  ;; Dump the language to the file
                  (else-dump-language compiled-file-name))
              (message "Failed to compile %s" language-file-name))
            ;; Clean up the input buffer.
            (kill-buffer language-input-buffer)))
      (message "File %s doesn't exist." language-file-name))))

(defun else-convert-pseudo-code-to-comment ()
  "Convert text enclosed by '<< >>' into a comment."
  (interactive)
  (let ((limit)
        (data))
    (progn
      (save-match-data
        (save-excursion
          (end-of-line)
          (setq limit (point))
          (beginning-of-line)
          (if (re-search-forward "<<\\(.+\\)>>" limit t)
              (progn
                (setq data (match-string 1))
                (delete-region (match-beginning 0) (match-end 0))
                (beginning-of-line)
                (call-interactively 'comment-dwim)
                (insert data))))))))

;;
;; Delete either a "LANGUAGE", "PLACEHOLDER" or "TOKEN" definition.
;; Called upon detection of the "DELETE" token in the template source file.
;;
(defun else-delete-a-definition ()
  (let ((obarray-name nil)
        (array-type nil)
        (language-name "")
        (current-language else-Current-Language)
        (object-name)
        (defining-type))
    ;; Get the 'type' of thing being deleted ie LANGUAGE, PLACEHOLDER or TOKEN
    (setq defining-type (match-string else-defining-type))
    (cond ((string= defining-type "PLACEHOLDER")
           (setq array-type ?p))
          ((string= defining-type "TOKEN")
           (setq array-type ?t))
          ((string= defining-type "LANGUAGE")
           (else-delete-language-definition)))

    (if array-type
        (progn
          ;;
          ;; Get the name of the item being deleted first then move on and get
          ;; the target language database to perform the operation upon.
          ;;
          (setq object-name
                (else-strip-quotes (match-string else-defining-name)))
          (forward-line)
          (if (else-scan-for-match else-body-string nil)
              (progn
                (setq language-name
                      (else-strip-quotes (match-string else-body-command-2)))
                (if language-name
                    (progn
                      (if (else-establish-language language-name)
                          (progn
                            (cond ((char-equal array-type ?p)
                                   (setq obarray-name Placeholder))
                                  ((char-equal array-type ?t)
                                   (setq obarray-name Token)))
                            (if obarray-name
                                (unintern (upcase object-name) obarray-name)))
                        (message "This language (%s) has not been defined!"
                                 language-name))
                      ;; restore the 'original' language i.e. might
                      ;; have template language enabled for a .lse
                      ;; file that contains definitions for some other
                      ;; language - so compiling a definition
                      ;; shouldn't destroy the current language
                      ;; settings for the buffer.
                      (else-establish-language current-language)))))))))

;; Unintern or "Delete" an entry in `obarray'. This function is used as an
;; argument to the mapatoms command.
(defun else-delete-entry (s)
  (let ()
    (if (not (unintern s))
        (message "Can't delete from obarray!"))))

;;
;; Delete a language definition from the Global Template definitions.
;; Called when the sequence "DELETE LANGUAGE" has been parsed in the
;; language template definition file.
;;
(defun else-delete-language-definition ()
  (let ((language-name "")
        (language-assoc)
        (current-language else-Current-Language))
    (progn
      (setq language-name
            (else-strip-quotes (match-string else-defining-name)))
      (if (else-establish-language language-name)
          (progn
            (mapatoms (lambda (s)
                        (if (not (unintern s Placeholder))
                            (message "Can't delete from Placeholder")))
                      Placeholder)
            (mapatoms (lambda (s)
                        (if (not (unintern s Token))
                            (message "Can't delete from Token")))
                      Token)
            (setq language-assoc
                  (assoc language-name else-Language-Definitions))
            (if language-assoc
                (progn
                  ;;
                  ;; The 'delete' command doesn't seem to work, so I will
                  ;; adopt a strategy of "emptying" the language entry
                  ;; Unless I apply logic to "re-use" the slot, then
                  ;; we will waste the memory allocated to the vectors
                  ;; but at the moment I don't care.
                  ;;
                  (setq else-Language-Definitions
                        (delete (assoc language-name else-Language-Definitions)
                                else-Language-Definitions))))))
      ;; restore the 'original' language i.e. might have template
      ;; language enabled for a .lse file that contains definitions
      ;; for some other language - so compiling a definition shouldn't
      ;; destroy the current language settings for the buffer.
      (else-establish-language current-language))))

;;
;; Delete or "Kill" the placeholder in which `point' resides. The 'force' option
;; is added because it is possible for else-mode to want to kill a placeholder
;; and it really means it wants to - this situation arises when creating new
;; language templates and the second /LANGUAGE="{language_name}" has been
;; defined as a AUTO-SUBSTITUTE - in this case the else-before-change function
;; really does want to kill the second occurrence but we really do want to keep
;; the definition as a mandatory just in case the user doesn't want to define
;; this placeholder type (language_name).
;;
(defun else-delete-placeholder (&optional leave-spacing force dont-kill-empty-lines)
  "Delete the placeholder at `point'. Clean up syntactically."
  (interactive "i\nP")
  (let ((separator)
        (separator-region-end)
        (had-left-space nil)
        (had-right-space nil)
        (here)
        (search-limit)
        (string-index)
        (new-separator-search))

    (catch 'problem
      (if (not else-mode)
          (progn
            (error "ELSE mode not enabled.")
            (throw 'problem nil)))

      ;; Check if we are in a valid placeholder and also that the detected
      ;; placeholder is not mandatory (the else-Mandatory-Placeholder flag is
      ;; set as a side effect of the call to else-in-placeholder).
      (if (else-in-placeholder)
          (if (or (not else-Mandatory-Placeholder) force)
              (progn
                ;; Make sure we keep a copy of the text of the
                ;; placeholder being deleted - this is mainly because
                ;; of the symbol-name of the definition doesn't
                ;; reflect the same character casing of the text being
                ;; deleted.
                (setq else-deleted-string (buffer-substring
                                           else-placeholder-start
                                           else-placeholder-end))

                (goto-char else-placeholder-end)

                (if else-please-duplicate
                    (delete-region else-placeholder-start (+ (point) 3))
                  (delete-region else-placeholder-start (point)))
                  
                ;; Pause here a moment and check what "spacing" surrounded the
                ;; deleted placeholder.
                (if (char-or-string-p (preceding-char))
                    (setq had-left-space
                          (char-equal (preceding-char) ?\ ))
                  (setq had-left-space nil))

                (if (char-or-string-p (following-char))
                    (setq had-right-space
                          (char-equal (following-char) ?\ ))
                  (setq had-right-space nil))

                ;; Process a placeholder that maintains a "separator" string
                (setq separator (get else-current-definition 'else-separator-ref))
                (if (> (length separator) 0)
                    (progn
                      (setq separator-region-end (point))
                      ;; Set a limit as the beginning of the previous line, this
                      ;; is a worst case situation.
                      (forward-line -1)
                      (setq search-limit (point))
                      (goto-char separator-region-end)

                      ;; Note that some major modes provide trimming of trailing
                      ;; spaces on file-save! So, any template separators may
                      ;; have one or more trailing spaces stripped off in the
                      ;; buffer (if they appear at a line and the file was saved
                      ;; and then re-edited i.e. the separator won't get
                      ;; matched! So the following code looks for the separator
                      ;; in it's entirety, then if not found it trims off
                      ;; trailing spaces and attempts to do a regexp match for
                      ;; the separator.

                      (if (search-backward separator search-limit t)
                          (delete-region (point) separator-region-end)
                        ;; not found
                        (progn
                          ;; check for special case mentioned above i.e. if
                          ;; there is a trailing space in the separator, has it
                          ;; been deleted accidently?
                          (if (char-equal
                               (aref separator (1- (length separator))) ?\ )
                              ;; there is at least one trailing space, so make
                              ;; up a new search string. Might as well make it a
                              ;; regexp.
                              (progn
                                (setq string-index (1- (length separator)))
                                (while (and (< 0 string-index)
                                            (char-equal
                                             (aref separator string-index) ?\ ))
                                  (setq string-index (1- string-index)))
                                ;; Now make a regexp which is the left over
                                ;; separator character(s) plus " +" at the end.
                                (setq new-separator-search
                                      (make-string (+ 3 string-index) ?\ ))
                                ;; Copy the separator character(s) over.
                                (while (not (= -1 string-index))
                                  (aset new-separator-search string-index
                                        (aref separator string-index))
                                  (setq string-index (1- string-index)))
                                ;; Now append the regexp for a series of spaces.
                                (aset new-separator-search
                                      (1- (length new-separator-search))
                                      ?*)
                                (aset new-separator-search
                                      (- (length new-separator-search) 2)
                                      ?\ )
                                ;; Now try the search.
                                (if (re-search-backward
                                     new-separator-search search-limit t)
                                    ;; found
                                    (delete-region (point) 
                                                   separator-region-end))))))))
                (setq here (point))

                ;; If this leaves the line blank, then delete the entire line
                (end-of-line)
                (if (and (not (else-scan-for-match "[^ \t]" nil t))
                         (not dont-kill-empty-lines))
                    (progn
                      (beginning-of-line)
                      (kill-line))
                  ;; Line is not blank
                  (progn
                    (goto-char here)
                    ;; If there was a space before and after the placeholder
                    ;; then "clean-up" by deleting one more space under
                    ;; point. Groan.... only if there is more than one space!
                    (if (and (not leave-spacing) had-right-space had-left-space
                             (char-equal (preceding-char) ?\ )
                             (char-equal (following-char) ?\ ))
                        (delete-char 1))
                    ;; Finally, if it is a "punctuation" character of the
                    ;; language then make sure there is no preceding space. But
                    ;; if it's not at the start of a line (and the leave-spacing
                    ;; hasn't been requested) (exceptions... exceptions...
                    ;; *sigh*)
                    (if (not (= (current-column) 0))
                        (if (and (not leave-spacing)
                                 (aref Language-Punctuation-Characters-Vector
                                       (following-char)))
                            (while (char-equal (preceding-char) ?\ )
                              (delete-char -1)))))))
            (error "Can't delete - mandatory entry required"))))))

;;
;; Display the list of possible matches for the expand-a-word function, it is
;; not called unless there is more than one possible match
;;
(defun else-display-menu (possible-matches &optional momentary-only)
  "Display menu of possible choices. Doubles as prompt display as well - yuk."
  (let ((my-buffer)
        (start-window (selected-window))
        (menu-string ""))
    (save-window-excursion
      (save-excursion
        ;; Process all of the possible matches into a list of strings
        ;; appropriate for display to the user.
        (dolist (match-element possible-matches)
          (setq menu-string (concat menu-string 
                                    (else-display-menu-element match-element))))

        ;; Sometimes we end up with an extraneous carriage return at
        ;; the end of the menu list - this causes a blank line in the
        ;; menu display. Get rid of it.
        (if (= (elt menu-string (1- (length menu-string))) ?\n)
          (aset menu-string (1- (length menu-string)) ?\ ))
        ;; Set the selection item to nothing
        (setq else-selected-text nil)

        ;; Create and point to the appropriate buffer i.e. if a momentary
        ;; display then use a separate buffer to that used by the menu selection
        ;; process. 
        (if momentary-only
            (progn
              (setq my-buffer (get-buffer-create " *ELSE Placeholder Prompt*"))
              (set-buffer my-buffer))
          ;; Otherwise it is a menu display of possible choices, so create a
          ;; temporary buffer and display the data.
          (setq my-buffer (get-buffer-create " *ELSE Menu List*"))
          (set-buffer my-buffer)
          (if (and else-set-lineno (featurep 'setnu))
              ;; Don't have to worry about "standard" minor mode behaviour of
              ;; each call "toggling" the mode as this mode allows a positive
              ;; argument to indicate that the mode should be turned on.
              (setnu-mode 1)))

        (erase-buffer)

        ;; Am now pointing into the appropriate buffer so insert the data for
        ;; display. 
        (setq truncate-lines t)
        (insert menu-string)

        ;; Now position the cursor appropriately, size the window to fit the
        ;; buffer to be displayed and then display it.
        (goto-char (point-min))
        (split-window (selected-window))
        (set-window-buffer start-window my-buffer)
        ;; Finally, size the window to match the size of the buffer i.e. no
        ;; point in displaying a window that is unnecessarily large.
        (shrink-window-if-larger-than-buffer start-window)
        (select-window start-window)
        (setq major-mode 'else-Display)
        (use-local-map else-menu-mode-map)
        (setq mode-name "ELSE mode")
        (if momentary-only
            (sit-for else-prompt-time)
          (recursive-edit))))))

(defun else-display-menu-element (body-element)
  "Given a definition body element, extract the appropriate
information for inclusion in a menu display and format it
appropriately. Return the resultant string."
  (let ((description nil)
        (element-text)
        (current-type)
        (result ""))

    (setq element-text (elt body-element else-body-text-ref))
    (setq current-type (elt body-element else-body-type-ref))
    (if current-type
        (progn
          (setq description
                (else-get-description element-text current-type))
          (cond ((char-equal current-type ?p)
                 (setq result (concat result 
                                      (format "{%s}" element-text))))
                ((char-equal current-type ?t)
                 (setq result (concat result 
                                      (format "\"%s\"" element-text)))))
          (if description
              (setq result (concat result (format " - %s\n" description)))
            (setq result (concat result "\n"))))
      (setq result (concat result 
                           (format "\"%s\"\n" element-text))))
    result))

(defun else-dump-language (compiled-file-name)
  "Dump the current buffer language template to the named file.
Note that the file name parameter must have been already vetted to make sure
it complies with the else naming conventions ie .esl"
  (let ((language-output-buffer))
    ;; The language definition should be in the local copies of Placeholder,
    ;; Token and Language-Specifics. So we can take that and write it out to
    ;; the language compilation file.
    (save-excursion
      (setq language-output-buffer
            (find-file-noselect compiled-file-name t))
      (set-buffer language-output-buffer)
      (setq else-read-marker (point-marker)))

    ;; Ok, all set to write the data to the buffer. Write the language specific
    ;; information and then each element of the Placeholder and Token obarrays.
    (print Language-Specifics else-read-marker)
    (setq else-type-of-symbols ?p)
    (mapatoms 'else-store-element Placeholder)
    (setq else-type-of-symbols ?t)
    (mapatoms 'else-store-element Token)

    (save-excursion
      (set-buffer language-output-buffer)
      (save-buffer))
    ;; Clean up the output buffer.
    (kill-buffer language-output-buffer)))

;;
;; Make sure that all key-bindings that bind to the expansion command are
;; echoed in the menu selection keymap ie the user doesn't have to move his
;; fingers from the command that caused a menu pick list to be displayed.
;;
(defun else-enable-dups (map command-to-search-out replacement-command)
  (let (abc)
    (setq abc (where-is-internal command-to-search-out))
    (while abc
      (progn
        (define-key map (car abc) replacement-command)
        (setq abc (cdr abc))))))

;;
;; Set the local buffer variables to the appropriate language definition
;; templates.
(defun else-establish-language (language-name)
  "Set language template set 'language-name as the current template
set for this buffer."
  (let ((language-assoc)
        (result nil))
    (setq language-assoc
          (cdr (assoc language-name else-Language-Definitions)))
    (if language-assoc
        (progn
          (setq Placeholder (car language-assoc))
          (setq Token (car (cdr language-assoc)))
          (setq Language-Specifics (car (cdr (cdr language-assoc))))
          (setq else-Current-Language language-name)
          (setq result t)))
    result))

(defun else-expand-item-at-overlay (item)
  "Expand the object (placeholder or token) 'item at the location of the
else-placeholder-overlay. This defun is designed to be called by an external
entity with a selection from a menu that was constructed using
else-get-menu-entries. It must work in conjunction with the use of the Overlay
denoted by else-placeholder-overlay."
  (let ((this-definition nil)
        (origin nil))
    ;; Protect against inadvertant use
    (catch 'problem
      (if (not else-mode)
          (progn
            (error "ELSE mode not enabled.")
            (throw 'problem nil)))

      (if (overlayp else-placeholder-overlay)
          (progn
            (goto-char (1+ (overlay-start else-placeholder-overlay)))
            (setq origin (point))
            ;; 'process' the selected menu entry - note the expectation that any
            ;; item entered to this defun must be encoded using the format that
            ;; else-get-menu-entries imposes.
            (setq else-selected-text (else-extract-item item))

            ;; see if the item is a placeholder or token. This is made easier
            ;; because the menu encoding adds {}'s to placeholders and leaves
            ;; tokens and substitution strings alone!
            (if (char-equal (elt item 0) ?\{)
                ;; placeholder
                (progn
                  (setq this-definition (else-look-up else-selected-text ?p))
                  (if this-definition
                      ;; It's a valid placeholder
                      (else-process-definition this-definition ?p)))
              ;; must be either a token or a straight string substitution
              (setq this-definition (else-look-up else-selected-text ?t))
              (if this-definition
                  (else-process-definition this-definition ?t)
                ;; Not a placeholder or a token so it must be text to substitute
                (else-substitute else-selected-text nil)))
            ;; Now position the cursor intelligently
            (goto-char origin)
            (else-next-placeholder))
        ;; There is no overlay defined
        ))))

;;
;; Function name is a misnomer, this routine is the general start point for
;; expanding either a placeholder or a token.
;;
(defun else-expand-placeholder ()
  "Expand the placeholder or token located at `point'."
  (interactive)
  (let ((here (point)))
    (catch 'problem
      (if (not else-mode)
          (progn
            (error "ELSE mode not enabled.")
            (throw 'problem nil)))

      ;; Reset the definition type and then work out if there is a
      ;; valid definition to expand - setting this variables as a side
      ;; effect.
      (setq else-definition-type nil)

      (if (else-in-placeholder)
          (progn
            ;; Yup, detected a valid placeholder, so just skip
            ;; straight down and process it.
            )
        ;; Else - check if it is as a token.
        (if (else-after-token)
            (progn
              ;; yes, it is a token
              )
          ;; No, not a token either - but it might be a partial token
          ;; or point might not be positioned at the end of the token
          ;; - it is 'nice' to handle either of these two cases. 
          (if else-expand-a-word-available
              (progn
                (if (or (looking-at "\\s-\\|$")
                        (eq (point) (point-max)))
                    ;; at the end of a string of text - so try
                    ;; completion on the name. Note that this
                    ;; functionality should only be expected to work
                    ;; for TOKENs! This code does not guaratee
                    ;; this!!!!
                    (progn
                      (if (expand-a-word (else-return-sorted-list Token))
                          (else-after-token)))

                  ;; else point may be in the middle of a string? If
                  ;; so then position at the end of the string and
                  ;; check for token validity again.
                  (setq match-scan (format "\\([^%s]+?\\|$\\)" 
                                           (cdr (assoc else-Valid-Idents-ref 
                                                       Language-Specifics))))
                  (if (else-scan-for-match match-scan nil nil)
                      (if (match-string 0)
                          (progn
                            (goto-char (match-beginning 0))
                            (if (expand-a-word (else-return-sorted-list Token))
                                (else-after-token))))))))))

      ;; O.K. If it's a placeholder or a token then process it.
      (if else-definition-type
          (progn 
            (if (else-process-definition else-current-definition
                                         else-definition-type)
                (progn 
                  ;; A valid definition was processed, so position back to
                  ;; the start of it and proceed to the next placeholder.
                  (goto-char else-placeholder-start)
                  (else-next-placeholder))))
        (progn
          ;; Not a valid placeholder or a token string. Check to see what
          ;; behaviour is required. The following code dealing with
          ;; expand-or-move flag and else-direction flag was added to make ELSE
          ;; more usable with VoiceCoder. the test for interactiveness is needed
          ;; because the case no valid placeholder can be found in the
          ;; search-direction results in a crash (endless loop).  This situation
          ;; is quite likely if you use speech recognition and try to expand a
          ;; token.
          (if (and else-move-and-execute (interactive-p))
              ;; User must desire a movement and then an expansion i.e. the next
              ;; placeholder can be "seen" and an expansion is desired there.
              (progn
                (if else-direction
                    (else-next-placeholder)
                  (else-previous-placeholder))
                (else-expand-placeholder))
            ;; otherwise default behaviour is to print an error message to the
            ;; user.  This is voice coding slanted: sometimes when called as-if
            ;; interactive from abbreviations, an error destroys the contents of
            ;; before-change-functions. I don't know exactly why and where, but
            ;; this is the solution:
            (if else-move-and-execute
                (message "Not a valid placeholder or token.")
              (goto-char here)
              (error "Not a valid placeholder or token."))))))))

;;
;; These routines provide the 'extract' feature of ELSE
;;
(defun else-extract-all ()
  "Extract all placeholders and tokens definitions for the enabled language
template."
  (interactive)
  (let ((current-language else-Current-Language)
	(sorted-names))
    ;; Allow a bit of flexibility here, if the user is extracting in a buffer
    ;; that has language defined then operate on that language, otherwise
    ;; prompt the user for a language name and then operate on that language
    ;; definition. Do also if the user is editing an LSE file i.e. template
    ;; would be the language enabled - which you don't want to extract from
    ;; :-).
    (if (else-extract-chk-language)
        (progn
          ;; Extract the language definition
          (insert (concat "DELETE LANGUAGE \"" else-Current-Language
                          "\" -"))
          (newline)
          (insert (concat "DEFINE LANGUAGE \"" else-Current-Language
                          "\" -"))
          (newline)
          (indent-to else-Extract-Column)
          (insert (concat "/INITIAL_STRING=\""
                          (cdr (assoc else-Initial-String-ref Language-Specifics))
                          "\" -"))
          (newline)
          (indent-to else-Extract-Column)
          (insert (concat "/PUNCTUATION_CHARACTERS=\""
                          (cdr (assoc else-Punctuation-ref Language-Specifics))
                          "\" -"))
          (newline)
          (indent-to else-Extract-Column)
          (insert (concat "/SELF_INSERT_CHARACTERS=\""
                          (cdr (assoc else-Self-Insert-Characters-ref
                                      Language-Specifics))
                          "\" -"))
          (newline)
          (indent-to else-Extract-Column)
          (insert (concat "/VALID_IDENTIFIER_CHARACTERS=\""
                          (cdr (assoc else-Valid-Idents-ref
                                      Language-Specifics))
                          "\" -"))
          (newline)
          (indent-to else-Extract-Column)
          (insert (concat "/INDENT_SIZE=\""
                          (number-to-string
                           (cdr (assoc else-tab-size-ref
                                       Language-Specifics)))
                          "\" -"))
          (newline)
          (indent-to else-Extract-Column)
          (insert (concat "/VERSION=\""
                          (cdr (assoc else-language-version-ref
                                      Language-Specifics))
                          "\" -"))
          (newline)
          (newline)
          (insert "END DEFINE")
          (newline)
          (newline)
          ;; Extract all the placeholder and token definitions. Do
          ;; this by getting a alphabetically sorted list of the
          ;; placeholder/token names and then processing each of the
          ;; definitions individually
          (setq sorted-names (else-return-sorted-list Placeholder))
          (mapc '(lambda (element-name)
                   (else-extract-a-placeholder 
                    (intern-soft (upcase element-name)
                                 Placeholder))) sorted-names)
          
          (setq sorted-names (else-return-sorted-list Token))
          (mapc '(lambda (element-name)
                   (else-extract-a-token
                    (intern-soft (upcase element-name)
                                 Token))) sorted-names)
          
          ;; Restore the original language (assuming there was one)
          (if current-language
              (else-establish-language current-language))))))
;;
;; Extract an individual placeholder definition. (non-interactive form
;; - see else-extract-placeholder).
;;
(defun else-extract-a-placeholder (placeholder-definition)
  (let ((selected-definition placeholder-definition)
        (name)
        (temp)
        (tmp)
        (function-name)
        (body)
        (lang-indent-size (cdr (assoc else-tab-size-ref Language-Specifics))))

    (setq name (get selected-definition 'else-original-name))
    (insert "DELETE PLACEHOLDER ")
    (if (string-match " " name)
        (insert (concat "\"" name "\" -"))
      (insert (concat name " -")))
    (newline)
    (indent-to else-Extract-Column)
    (insert (concat "/LANGUAGE=\"" else-Current-Language "\" -"))
    (newline)
    (insert "DEFINE PLACEHOLDER ")
    (if (string-match " " name)
        (insert (concat "\"" name "\" -"))
      (insert (concat name " -")))
    (newline)
    (indent-to else-Extract-Column)
    (insert (concat "/LANGUAGE=\"" else-Current-Language "\" -"))
    (newline)
    (indent-to else-Extract-Column)
    ;; Check to see if the placeholder is a forward reference.
    (setq temp (get selected-definition 'else-placeholder-ref))
    (if (car temp)
        (progn
          (insert "/PLACEHOLDER=")
          (insert (concat "\""
                          (car (cdr temp))
                          "\"")))
      ;; Not a forward reference, so continue with the rest of the information.
      (progn
        (setq temp (get selected-definition 'else-substitute-ref))
        (cond ((char-equal temp ?n)
               (insert "/NOAUTO_SUBSTITUTE -"))
              ((char-equal temp ?a)
               (insert "/AUTO_SUBSTITUTE -")
               (newline)
               (indent-to else-Extract-Column)
               (insert "/SUBSTITUTE_COUNT=")
               (insert (number-to-string
                        (get selected-definition 'else-substitute-count-ref)))
               (insert " - "))
              (t
               (message "invalid substitute value")))
        (newline)
        (indent-to else-Extract-Column)
        (insert "/DESCRIPTION=")
        (insert (concat "\"" (get selected-definition 'else-description-ref)) "\"")
        (setq temp (get selected-definition 'else-duplication-ref))
        (newline)
        (indent-to else-Extract-Column)
        (insert "/DUPLICATION=")
        (cond ((char-equal temp ?c)
               (insert "CONTEXT_DEPENDENT -"))
              ((char-equal temp ?v)
               (insert "VERTICAL -"))
              ((char-equal temp ?h)
               (insert "HORIZONTAL -"))
              (t
               (message "Illegal duplication type detected")))
        (newline)
        (indent-to else-Extract-Column)
        (insert (concat "/SEPARATOR=" "\""
                        (get selected-definition 'else-separator-ref)
                        "\" -"))
        (setq temp (get selected-definition 'else-type-ref))
        (newline)
        (indent-to else-Extract-Column)
        (insert "/TYPE=")
        (cond ((char-equal temp ?m)
               (insert "MENU -"))
              ((char-equal temp ?t)
               (insert "TERMINAL -"))
              ((char-equal temp ?n)
               (insert "NONTERMINAL -"))
              (t
               (message "Illegal service detected")))
        (else-extract-runcode-component)
        (setq body (get selected-definition 'else-body-ref))
        (newline)
        (indent-to else-Extract-Column)
        (while (car body)
          (progn
            (newline)
            (indent-to else-Extract-Column)
            (insert "\"")
            (insert (make-string
                     (* (nth else-body-indent-ref (car body))
                        lang-indent-size)
                     ?\ ))
            (insert (concat (nth else-body-text-ref (car body)) "\""))
            (setq temp (nth else-body-type-ref (car body)))
            (if temp
                (cond ((char-equal temp ?p)
                       (insert "/PLACEHOLDER"))
                      ((char-equal temp ?t)
                       (insert "/TOKEN"))
                      (t
                       (message "Illegal body text type detected"))))
            (setq body (cdr body))))))
    (newline)
    (newline)
    (insert "END DEFINE")
    (newline)
    (newline)))

;;
;; Extract an individual token definition. (non-interactive form - see
;; else-extract-token).
;;
(defun else-extract-a-token (token-definition)
  (let ((selected-definition token-definition)
        (temp)
        (body)
        (name))
    (setq name (get selected-definition 'else-original-name))
    (insert "DELETE TOKEN ")
    (insert (concat name " -"))
    (newline)
    (indent-to else-Extract-Column)
    (insert (concat "/LANGUAGE=\"" else-Current-Language "\" -"))
    (newline)
    (insert "DEFINE TOKEN ")
    (insert (concat name " -"))
    (newline)
    (indent-to else-Extract-Column)
    (insert (concat "/LANGUAGE=\"" else-Current-Language "\" -"))
    (setq temp (get selected-definition 'else-placeholder-ref))
    (if (car temp)
        (progn
          (newline)
          (indent-to else-Extract-Column)
          (insert "/PLACEHOLDER=")
          (insert (car (cdr temp))))
      (progn
        (setq temp (get selected-definition 'else-description-ref))
        (if temp
            (progn
              (newline)
              (indent-to else-Extract-Column)
              (insert "/DESCRIPTION=")
              (insert (concat "\""
                              (get selected-definition
                                   'else-description-ref) "\""))))))
    (else-extract-runcode-component)
    (setq body (get selected-definition 'else-body-ref))
    (newline)
    (indent-to else-Extract-Column)
    (while (car body)
      (progn
        (newline)
        (indent-to else-Extract-Column)
        (insert (concat "\""
                        (nth else-body-text-ref (car body)) "\"")))
      (setq body (cdr body)))
    (newline)
    (newline)
    (insert "END DEFINE")
    (newline)
    (newline)))

;; This routine assumes that it has been called after a regular expression match
;; that indicates a line from the body of a definition has been found
;; ie. \".*\".  The entry for the body form can be of the form " text " with an
;; optional trailer(s) of /TOKEN or /PLACEHOLDER. If the text is meant for
;; substitution i.e. it is not a menu or terminal entry prompt, then we wish to
;; also record indentation information. The algorithm that calculates the number
;; of indents for each line makes an assumption that the first line that is
;; indented establishes a "base" indent level and any subsequent lines have
;; their spacing compared with this count. Therefore, this routine (since it
;; acts on a single line at a time) must take an argument of any current indent
;; level information that may be in force and conversely, provide any indent
;; information that it "discovers" i.e. if there is not indent level currently
;; in force then if the current line contains indent information, that
;; information must be returned to the caller.
;;
;; Further processing is made easier if there are three elements in each 'body'
;; entry, so start the definition off with nil for all three elements and then
;; replace them as needed. The order is /PLACEHOLDER or /TOKEN, indentation and
;; then "any text". It assumes that the match-data from the "body" search string
;; is still valid!
;;
;; 28-Jun-2002: Found a problem when attempting to define bodies that are purely
;; textual in nature and the user doesn't want the indentation rules to be
;; used i.e. definition of a file header with text lines that are heavily
;; indented. Take the (obvious?) approach and use some special character ('@' in
;; this case to denote 'hard' spaces.
;;
(defun else-extract-body (indent-level-size)
  (let ((local-list (list nil 0 nil t))
        (this-line nil)
        (this-indent-size)
        (return-size indent-level-size))
    (progn
      ;; Extract the text and place it into a local variable for further
      ;; manipulation and testing.
      (setq this-line (else-strip-quotes
                       (match-string else-body-text-1)))
      ;; Search the line for the first occurrence of non-whitespace, this is the
      ;; current indent value. Make sure that the match data is preserved
      ;; because we use a regular expression scan here!
      (save-match-data
        (setq this-indent-size (string-match "\\S-" this-line))
        ;; Allow for the case of an empty line i.e. ""
        (if (not this-indent-size)
            (setq this-indent-size 0)))
      ;; If there is an indent, then calculate it as a multiple of the global
      ;; value 'indent-level-size
      (if (> this-indent-size 0)
          (progn
            ;; First, make sure we strip out the white i.e. we don't want to
            ;; copy it through, so copy through from the first non-whitespace to
            ;; the end.
            (setq this-line (substring this-line this-indent-size))
            ;; Calculate the indentation as a multiple of the "base" value but
            ;; allow for the case where the base value hasn't been established
            ;; yet! Note that this calculation also assumes that if the
            ;; indentation is greater than a "multiple" of the base value then
            ;; an assumption is made that the user made a mistake and is
            ;; requesting the next multiple boundary i.e. the base may be 3
            ;; spaces and a line is indented to space 5 - this means that 6 was
            ;; intended.
            (if (> indent-level-size 0)
                (progn
                  (if (> (% this-indent-size indent-level-size) 0)
                      (setcar (nthcdr else-body-indent-ref local-list)
                              (1+ (/ this-indent-size indent-level-size)))
                    (setcar (nthcdr else-body-indent-ref local-list)
                            (/ this-indent-size indent-level-size))))
              ;; else this value is now the base indent size
              (setcar (nthcdr else-body-indent-ref local-list) 1)
              (setq return-size this-indent-size)))
        ;; either no indentation (in which case leave at the default level) or
        ;; the definition may contain 'hard' space characters i.e. '@'. Convert
        ;; all of these to real spaces.
        (progn
          ;; This is only a possibility if the first character(s) are '@'
          ;; i.e. there is no need to place 'hard spaces' after any non-white
          ;; space. 
          (if (and (not (= (length this-line) 0)) 
                   (or (string= (substring this-line 0 1) "@")
                       (and (> (length this-line) 1)
                            (string= (substring this-line 0 2) "\\@"))))
              (progn
                ;; protect 'real' @'s the tradional way by preceding
                ;; them with a '\' character - so just strip off that
                ;; single character and proceed 
                (if (string= (substring this-line 0 2) "\\@")
                    (setq this-line (substring this-line 1 (length this-line)))
                  ;; else the case of @'s being used to designate hard
                  ;; spaces, so strip the @'s and convert them to
                  ;; space characters in this-line.
                  (let (number-ats)
                    ;; Not necessary to test the result - can only get
                    ;; here if there are leading @'s
                    (string-match "@+" this-line)
                    (setq this-line 
                          (replace-match (make-string 
                                          (length (match-string 0 this-line)) ?\ )
                                         t t
                                         this-line))))))))

      ;; Place the text into the last element of the list.
      (setcar (nthcdr else-body-text-ref local-list) this-line)
      ;;
      ;; Now extract any trailing modifiers ie /PLACEHOLDER or /TOKEN
      ;;
      (setq this-line (match-string else-body-text-2))
      (if this-line
          (cond ((string= this-line "/PLACEHOLDER")
                 (setcar (nthcdr else-body-type-ref local-list) ?p))
                ((string= this-line "/TOKEN")
                 (setcar (nthcdr else-body-type-ref local-list) ?t))
                (t
                 (message "Haven't covered this option yet!"))))
      (setq this-line (match-string else-body-text-3))
      (if this-line
          (cond ((string= this-line "/NOFOLLOW")
                 (setcar (nthcdr else-body-menu-follow-on-ref local-list) nil))
                ((string= this-line "/FOLLOW")
                 ;; this is the redundant case given the initialisation of
                 ;; 'local-list.
                 (setcar (nthcdr else-body-menu-follow-on-ref local-list) t))
                (t
                 (message "Haven't covered this option yet!")))))
    ;; Now return what we have "discovered".
    (cons return-size local-list)))

(defun else-extract-chk-language ()
  "Check and change the current language for extraction commands."
  (let ((result t))
    ;; Originally tried anding this test with the current language being
    ;; 'template' but that leads to some inconsistencies. Seems to make sense
    ;; that the rule is: if the current buffer is an LSE file then use the
    ;; language that can be calculated from the file name.
    (if (string-match (concat "\\" else-lse-ext) (buffer-name))
        ;; Case where the user is editing an LSE buffer/file, so calculate
        ;; the desired language based upon the name of the file/buffer
        (let ((file-name-comps (split-string (buffer-name) "\\.")))
          ;; Check if the file is of the form <lang>-cust.lse
          (if (string-match "-cust" (car file-name-comps))
              ;; Yes, it is a custom LSE file, so split further
              (progn
                (setq file-name-comps (split-string 
                                       (car file-name-comps) "-cust"))))

          ;; At this point, the CAR of file-name-comps should be the
          ;; language name.
          (setq result (else-establish-language (car file-name-comps)))
          (if (not result)
              (error "Language %s is not loaded" (car file-name-comps)))))

    result))

;;
;; Extract the "duplication" information from the template definition. Assumes
;; that the match-data information is valid!
;;
(defun else-extract-duplication-info ()
  (let ((this-line))
    (setq this-line
          (else-strip-quotes (match-string else-body-command-2)))
    (cond ((string= this-line "CONTEXT_DEPENDENT")
           ?c)
          ((string= this-line "VERTICAL")
           ?v)
          ((string= this-line "HORIZONTAL")
           ?h)
          (t
           (throw 'compile "illegal duplication type")))))
;;
;; Interactive command to extract an individual placeholder definition.
;;
(defun else-extract-placeholder ()
  "Extract the definition of a placeholder into the buffer at point."
  (interactive)
  (let ((selected-definition)
        (current-language else-Current-Language)
        (name)
        (temp completion-ignore-case))
    ;; Allow a bit of flexibility here, if the user is extracting in a buffer
    ;; that has language defined then operate on that language, otherwise
    ;; prompt the user for a language name and then operate on that language
    ;; definition.
    (if (else-extract-chk-language)
        (progn
          (setq completion-ignore-case t)
          (setq name
                (completing-read "Placeholder Name: " Placeholder))
          (setq name (upcase name))
          (setq selected-definition (else-look-up name ?p))
          (if selected-definition
              (else-extract-a-placeholder selected-definition)
            (message "Placeholder \`%s\` doesn't exist" name))

          (setq completion-ignore-case temp)
          ;; Restore the original language (assuming there was one)
          (if current-language
              (else-establish-language current-language))))))

(defun else-extract-runcode-component ()
  "Extract the run_code components - uses variables declared in caller
defun."
  (let ((temp)
        (tmp)
        (processing-function))
    ;; Do any /RUN_CODE attributes.
    (setq temp (copy-alist (get 
                            selected-definition 'else-elisp-action)))
    ;; If there are run-time encodings then have to handle the fact
    ;; that one or more phases have the same function pointer.
    (while temp
      (setq tmp (pop temp))
      (newline)
      (indent-to else-Extract-Column)
      (insert (format "/RUN_CODE=%s%s"
                      (cdr tmp) 
                      (car tmp)))
      (setq processing-function (cdr tmp))
      ;; Now recurse through the other functions (if any)
      (let ((local-copy (copy-alist temp)))
        (while local-copy
          (setq tmp (pop local-copy))
          (if (equal processing-function (cdr tmp))
              (progn
                (insert (car tmp))
                (setq temp
                      (assq-delete-all (car tmp) temp)))))))))

;;
;; Interactive command to extract and individual token definition.
;;
(defun else-extract-token ()
  "Extract the definition of a token into the buffer at point."
  (interactive)
  (let ((selected-definition)
        (temp completion-ignore-case)
        (name)
        (current-language else-Current-Language))
    ;; Allow a bit of flexibility here, if the user is extracting in a buffer
    ;; that has language defined then operate on that language, otherwise
    ;; prompt the user for a language name and then operate on that language
    ;; definition.
    (if (else-extract-chk-language)
        (progn
          (setq completion-ignore-case t)
          (setq name
                (completing-read "Token Name: " Token))
          (setq name (upcase name))
          (setq selected-definition (else-look-up name ?t))
          (if selected-definition
              (else-extract-a-token selected-definition)
            (message "Token \`%s\` doesn't exist" name))
          (setq completion-ignore-case temp)
          ;; Restore the original language (assuming there was one)
          (if current-language
              (else-establish-language current-language))))))

;;
;; Extract the "type" information of a placeholder ie it is either a MENU,
;; TERMINAL or NONTERMINAL definition.
;;
(defun else-extract-type-info ()
  "Parse the /TYPE attribute of the definition"
  (let ((this-match))
    (setq this-match (else-strip-quotes (match-string else-body-command-2)))
    (cond ((string= this-match "MENU")
           ?m)
          ((string= this-match "TERMINAL")
           ?t)
          ((string= this-match "NONTERMINAL")
           ?n)
          (t (throw 'compile "illegal TYPE value")))))
;;
;; Get the "body" of a definition. This is a list of "strings".
;;
(defun else-get-body (element)
  "Extract the 'body' of the definition."
  (let ()
    (get element 'else-body-ref)))

;;
;; Extract the descriptive text ie /DESCRIPTION="...."
;;
(defun else-get-description (name type)
  "Extract the /DESCRIPTION attribute"
  (let ((place-def (else-look-up name type)))
    (if place-def
        (get place-def 'else-description-ref))))

;;
;; Get a definition from either the Placeholder or Token definition array
;;
(defun else-get-entry (name-string type)
  "Get a definition from either the Placeholder or Token definition set"
  (let ((obarray-name nil))
    (cond ((char-equal type ?t) (setq obarray-name Token))
          ((char-equal type ?p) (setq obarray-name Placeholder)))

    (if obarray-name
        (intern name-string obarray-name))))

(defun else-get-menu-entries (element)
   "Create a list from the menu elements in this placeholder.  Note that if
sub-elements are defined with menu elements, then there are options to follow or
not follow individual elements in the menu list - but these options can be
overriden by the 'else-follow-menus and 'else-nofollow-menus flags! By doing
this we avoid menus that lead to other menus ad infinitum - very boring :-)."
  (let ((the-list (else-get-body element))
        (temp-list)
        (this-element)
        (this-def)
        (ref-type)
        (this-element-type)
        (new-list))
    (setq temp-list the-list)
    (while (> (length temp-list) 0)
      (setq this-element (car temp-list))
      (setq temp-list (cdr temp-list))
      ;; This is a bit weird, but basically we want to put the current element
      ;; into the new list that will be returned. The only exception to this is
      ;; if the entry is itself an menu, in which case the values of that item
      ;; will be substituted (depending upon /follow/nofollow and
      ;; else-follow-menus settings). All items will be prepended and that way
      ;; if we have to remove an element it will be easy.
      (setq new-list (append (cons this-element '()) new-list))
      ;; Check if the element is a placeholder and if so, if it is a menu
      ;; itself.
      (setq this-element-type (elt this-element else-body-type-ref))
      (if (and this-element-type
               (char-equal this-element-type ?p))
          (progn
            (setq this-def (else-look-up 
                            (elt this-element else-body-text-ref)
                            ?p))
            ;; guard against errors...
            (if this-def
                (progn 
                  (setq ref-type (get this-def 'else-type-ref))
                  ;; If the element
                  (if (and (equal ref-type ?m)
                           (not else-nofollow-menus)
                           (or else-follow-menus
                               (elt this-element else-body-menu-follow-on-ref)))
                      ;; There is a menu reference, so check if the placeholder
                      ;; being referenced is also a menu. If it is then replace
                      ;; this element with the elements from the referenced
                      ;; placeholder.
                      (progn
                        ;; Must remove the element at the head of the list as it
                        ;; is being replaced by this list.
                        (setq new-list (cdr new-list))
                        (setq new-list (append (else-get-body this-def)
                                               new-list)))))))))
    (nreverse new-list)))

(defun else-in-placeholder ()
  "Test if `point' is within a placeholder. Validity of text string is checked
   against the placeholder definition arrays. If it is a placeholder
   then set up several global (buffer local) variables to save effort
   elsewhere." 
  (let ((here (point))
        (result nil)
        (start-position)
        (end-position)
        (stop-point)
        (stop-flag))
    (save-excursion
      ;; Start by determining if invoked in a token or
      ;; placeholder. Placeholder's are defined as being enclosed by
      ;; either a [] or {} pair. This is more complicated than it
      ;; first appears because you have to make sure that any
      ;; "placeholder" indicators actually enclose `point' ie don't
      ;; find a placeholder earlier in the same line etc. Also,
      ;; placeholders may have embedded spaces, so again, more
      ;; complications.  Search backward for the first occurrence of
      ;; either '[', '{', whitespace or beginning of line. Things are
      ;; made really messy because it is possible to have point within
      ;; a placeholder that also contains another placeholder eg
      ;; [[abstract] tagged]. So, we perform the scan character by
      ;; character and use a "counting" system to record the "depth".
      ;;
      ;; Then I discovered another complication what about the case:
      ;; [{discriminant_simple_name}... =>]
      ;; here we have to make sure which is the "greater" placeholder
      (end-of-line)
      (setq stop-point (point))
      (beginning-of-line)
      (setq stop-flag nil)
      (while (and (< (point) here) (not stop-flag))
        (progn
          (if (re-search-forward "[{[]" here t)
              (progn
                (backward-char)
                (setq start-position (point))
                (if (not (= (else-match-brace stop-point) start-position))
                    (progn
                      (setq end-position (point))
                      (if (and (< start-position here)
                               (< here end-position))
                          (if (else-look-up (buffer-substring
                                             (+ start-position 1)
                                             end-position) ?p)
                              (progn
                                (setq else-placeholder-start start-position)
                                (setq else-placeholder-end (1+
                                                            end-position))
                                (setq else-definition-type ?p)

                                (setq else-definition-name (buffer-substring
                                                            (1+ start-position)
                                                            end-position))
                                ;; record the definition for any
                                ;; calling routine to use.
                                (setq else-current-definition
                                      (else-look-up else-definition-name ?p))
                                (if (char-equal (following-char) ?})
                                    (setq else-Mandatory-Placeholder t)
                                  (setq else-Mandatory-Placeholder nil)
                                  )
                                ;; Determine if the placeholder is 
                                ;; repeating 
                                (goto-char else-placeholder-end)
                                (setq else-please-duplicate 
                                      (looking-at (regexp-quote "...")))
                                (setq stop-flag t)
                                (setq result t))
                            (goto-char (1+ start-position)))
                        (goto-char (1+ start-position))))
                  (goto-char (1+ start-position))))
            (setq stop-flag t)))))
    result))

(defun else-initialise-symbol (the-definition)
  "Initialise a placeholder/token symbol with default values."
  (put the-definition 'else-placeholder-ref      '(nil nil))
  (put the-definition 'else-type-ref             ?n)
  (put the-definition 'else-separator-ref        "")
  (put the-definition 'else-substitute-ref       ?n)
  (put the-definition 'else-duplication-ref      ?c)
  (put the-definition 'else-topic-ref            nil)
  (put the-definition 'else-description-ref      "")
  (put the-definition 'else-body-ref             nil)
  (put the-definition 'else-substitute-count-ref 1)
  (put the-definition 'else-original-name        "")
  (put the-definition 'else-elisp-action         '()))

(defun else-insert-pseudo-code ()
  "Insert pseudo-code at point"
  (interactive)
  (progn
    (insert "<<   >>")
    (backward-char 4)))


(defun else-kill-placeholder (&optional leave-spacing force dont-kill-empty-lines)
  "Kill the current placeholder. Wrapper for else-delete-placeholder, calls the
lower level defun continuously in cases where the placeholder is defined as an
auto-substitute placeholder."
  (interactive "i\nP")
  (let ((current-location (point))
        (loop-counter)
	(is-auto-sub)
	(here)
        (stop-loop))
    ;; Verify we are with a placeholder.
    (if (else-in-placeholder)
        ;; Only proceed if the conditions are right.....
        (if (or (not else-Mandatory-Placeholder) force)
            (progn
              ;; If the definition is an "auto-substitute" then we have to
              ;; find the matching occurrences and delete them also.....
              (setq is-auto-sub (char-equal 
                                 (get else-current-definition 
                                      'else-substitute-ref)
                                 ?a))
              (if is-auto-sub
                  (progn
                    (setq loop-counter (get else-current-definition 
                                            'else-substitute-count-ref))
                    (setq stop-loop nil)
                    (while (and (not stop-loop) 
                                (> loop-counter 0))
                      (if (search-forward else-definition-name nil t)
                          (progn
                            ;; position back into the placeholder and then
                            ;; kill it (even if it is mandatory).
                            (backward-char)
                            ;; Next call just sets up all of the global
                            ;; variables so that the text can be deleted -
                            ;; don't use a kill-placeholder here because
                            ;; it would destroy the information that is
                            ;; required to be kept.....
                            (if (else-in-placeholder)
                                (progn
                                  (else-delete-placeholder leave-spacing 
                                                           force 
                                                           dont-kill-empty-lines)
                                  (setq loop-counter (1- loop-counter)))))

                        ;; There are no more occurrences of the placeholder
                        ;; name so stop the search.
                        (setq stop-loop t)))))

              ;; Now locate back into the original placeholder and
              ;; delete it - this returns all global variables to
              ;; their original state as the same time.
              (goto-char current-location)
              (else-in-placeholder)
              (else-delete-placeholder leave-spacing 
                                       force 
                                       dont-kill-empty-lines)
              ;; Check to see whether we should auto-position to the
              ;; next placeholder or not. This is mainly useful for
              ;; the VR folks so it is a customisable flag.
              (if (and else-kill-proceed-to-next-placeholder (interactive-p))
                  (if else-only-proceed-within-window
                      (progn
                        (setq here (point))
                        (else-move-n-placeholders 1)
                        ;; Only move to the new placeholder if it is
                        ;; visible in the current window i.e. we don't
                        ;; want the user to become disorientated by a
                        ;; large jump.
                        (if (not (pos-visible-in-window-p))
                            (goto-char here)))

                    ;; otherwise just default to move to the next placeholder.
                    (else-next-placeholder 1))))

          (error "Can't delete - mandatory entry required"))

      ;; Failed to delete because not in a placeholder - check if desired
      ;; behaviour is to find the next(previous) placeholder and kill that
      ;; instance. 
      (if (and else-move-and-execute (interactive-p))
          ;; User must desire to kill the next placeholder. So save them
          ;; the trouble of moving and killing as two separate actions.
          (progn
            (if else-direction
                (else-next-placeholder)
              (else-previous-placeholder))

            (else-kill-placeholder))))))

;;
;; Load a template definition file and "compile" it.
;;
(defun else-load-template ()
  "Load a language definition file and compile it."
  (let ((lang-def-buffer)
        (language-file-name)
        (result nil)
        (language-name)
        (found-template-name nil)
        (auto-mode-alist nil)
        (template-file-loaded nil))
    (progn
      (catch 'quit-loading
        ;; It is possible for the user to have an auto-mode-alist
        ;; entry for files of .lse type that will enable else-mode and
        ;; load the language templates for .lse files - this causes a
        ;; mix-up in the Placeholder/Token definitions if this
        ;; happens. So temporarily disable the entry if it exists in the
        ;; auto-mode-alist when we are auto-loading .lse files -
        ;; re-enable it at the end of this function. Note that this is
        ;; done using the "let" definition for auto-mode-list above.

        ;; Extract the mode-name and use that as the default template file
        ;; name
        (setq language-name mode-name)
        ;; Mode name "may" have spaces, modify them to '-' i.e. Visual
        ;; Basic becomes Visual-Basic.
        (while (string-match " " language-name)
          (setq language-name (replace-match "-" t nil language-name)))

        (setq language-file-name (else-locate-language-file language-name))
        (if (not language-file-name)
            (progn
              ;;
              ;; The file doesn't exist, so maybe its a case of the file
              ;; attribute not reflecting the language type. So ask the user.
              ;;
              (setq language-name
                    (read-from-minibuffer
                     "Enter the language name (no file extensions, please): "))
              (if (cdr (assoc language-name else-Language-Definitions))
                  (progn
                    ;; The language is already loaded, so set the local buffer
                    ;; language name and discontinue processing.
                    (setq else-Current-Language language-name)
                    (setq result t)
                    (throw 'quit-loading t))

                ;; Otherwise, check if a file of that name exists.
                (setq language-file-name 
                      (else-locate-language-file language-name)))))

        ;; At this point we have either skipped early (throw to 'quit-loading)
        ;; because we already have the language loaded for another buffer or we
        ;; have either found the file or not.
        (if language-file-name
            ;; Load the definition file and compile it
            (progn
              (save-excursion
                ;; Guard against deleting a file that already is loaded into the
                ;; current edit session. This 'flag' is used to determine
                ;; whether kill-buffer is called or not before defun
                ;; exit.
                (setq template-file-loaded 
                      (else-is-template-file-present language-file-name))
                (setq lang-def-buffer
                      (find-file-noselect language-file-name))
                (set-buffer lang-def-buffer)
                ;; Make sure we are at the beginning of the buffer. This is done
                ;; because there may be packages such as session.el active.
                (goto-char (point-min))
                ;; Have to make a choice based on what type of file we are
                ;; dealing with.
                (if (string-match else-esl-ext language-file-name)
                    (setq result (else-restore language-name))
                  (setq result (else-compile-buffer)))

                ;; Only delete the buffer if we had to load it explicitly,
                ;; otherwise leave it alone.
                (if (and result (not template-file-loaded))
                    ;; Only want to clean up when it succeeded, otherwise the
                    ;; user probably wants to see the error line.
                    (kill-buffer lang-def-buffer))

                ;; Now load any customisation files. This works by changing the
                ;; name to the form of <language-name>-cust.lse and then
                ;; searches first the local directory and then the load path.
                (setq language-file-name (else-locate-language-file
                                          (concat language-name "-cust.lse")
                                          t))
                (if language-file-name
                    (progn
                      (setq template-file-loaded 
                            (else-is-template-file-present language-file-name))
                      (setq lang-def-buffer
                            (find-file-noselect language-file-name))
                      (set-buffer lang-def-buffer)

                      ;; Make sure we are at the beginning of the buffer. This
                      ;; is done because there may be packages such as
                      ;; session.el active.
                      (goto-char (point-min))

                      ;; compile the customisation buffer, for the moment, just
                      ;; ignore the return of the compilation ie it doesn't
                      ;; matter at the moment.
                      (if (not (else-compile-buffer))
                          (message "Compile of custom file %s failed."
                                   language-file-name))

                      (if (not template-file-loaded)
                          (kill-buffer lang-def-buffer))))))))

      (if result
          (progn
            ;; It is possible that the 'language-name' is actually an
            ;; entered file name and isn't actually the name of the
            ;; language template loaded. If this is the case, then the
            ;; entered language name may be radically case different
            ;; from the actual language name as defined in the
            ;; template file i.e. TEMPLATE versus template. So make
            ;; the basic assumption that there will be only one
            ;; language name, doesn't matter what the casing is.
            (if (not (cdr (assoc language-name
                                 else-Language-Definitions)))
                (progn
                  ;; Look for it
                  (assoc-default language-name 
                                 else-Language-Definitions
                                 'else-test-key)
                  (setq language-name found-template-name)))
            (setq else-Current-Language language-name)))
      result)))


(defun else-test-key (element-car key)
  "Used by else-load-template to locate a language template set."
  (let (index)
    (setq index (string-match (upcase element-car)
                              (upcase key)))
    
    ;; Return the results
    (if (and (= index 0) (= (length element-car) (length key)))
        (setq found-template-name element-car))))


(defun else-locate-language-file (name &optional check-local-dir-first)
  "Search the load-path looking for a language definition file.
Precedence is given to `fast-load' versions ie .esl."
  (let ((load-point nil))
    (progn
      (if (or (string-match (concat "\\" else-lse-ext) name)
              (string-match (concat "\\" else-esl-ext) name))
          ;; OK, has the extension name present, so use that as the search
          ;; locus.
          (progn
            (setq load-point (else-search-load-path name
                                                    check-local-dir-first))
            (if (string-match (concat "\\" else-esl-ext) name)
                (else-check-language-file load-point)))

        ;; The name wasn't supplied with an extension, so try fast load first
        ;; and then slow load last.
        (setq load-point (else-search-load-path (concat name else-esl-ext)
                                                check-local-dir-first))
        (if (not load-point)
            (setq load-point (else-search-load-path (concat name
                                                            else-lse-ext)
                                                    check-local-dir-first))
          (else-check-language-file load-point)))
      load-point)))

(defun else-look-up (name-string type &optional ignore-forward-refs)
  "Look-up the definition of a placeholder/token called NAME-STRING.
'ignore-forward-refs allows functions like 'else-kill-placeholder to
stop the forwarding referrals i.e. we wish to kill what is there not
what might have been there :-)."
  (let ((obarray-name nil)
        definition)
    (cond ((char-equal type ?t) (setq obarray-name Token))
          ((char-equal type ?p) (setq obarray-name Placeholder)))

    (if obarray-name
        (progn
          (setq definition
                (intern-soft (upcase name-string) obarray-name))
          ;; Now check to see if it is a "forward" reference
          (if (and (not ignore-forward-refs)
                   (char-equal type ?p)
                   (car (get definition 'else-placeholder-ref)))
              ;; there is a forward reference
              (else-look-up (car
                             (cdr
                              (get definition 'else-placeholder-ref)))
                            ?p)
            definition))
      (message "Look-up error, type not defined for \`%s\`" name-string))))

(defun else-match-brace (&optional limit)
  (interactive)
  (let ((close-brace)
        (open-brace)
        (match-character)
        (search-back)
        (search-string)
        (stop-flag)
        (total-count 1)
        (current-pos (point)))
    (setq match-character (following-char))
    (cond ((char-equal match-character ?\})
           (setq search-back t)
           (setq open-brace ?\{))
          ((char-equal match-character ?\])
           (setq search-back t)
           (setq open-brace ?\[))
          ((char-equal match-character ?\))
           (setq search-back t)
           (setq open-brace ?\())
          ((char-equal match-character ?\{)
           (setq search-back nil)
           (setq close-brace ?\}))
          ((char-equal match-character ?\[)
           (setq search-back nil)
           (setq close-brace ?\]))
          ((char-equal match-character ?\()
           (setq search-back nil)
           (setq close-brace ?\)))
          (t (message "Not on a valid brace character")))

    (if search-back      ; looking for opening brace
        (progn
          (setq close-brace match-character)
          (setq stop-flag nil)
          (while (and (not (= total-count 0)) (not stop-flag))
            (setq search-string
                  (concat "["
                          (char-to-string close-brace)
                          (char-to-string open-brace)
                          "]"))
            (if (re-search-backward search-string limit t)
                (if (char-equal (following-char) open-brace)
                    (setq total-count (- total-count 1))
                  (setq total-count (+ total-count 1)))
              (progn
                (if (not limit)
                    (message "Match search failed!"))
                (goto-char current-pos)
                (setq stop-flag t)))))

      (progn               ;; Else case, ie looking for a closing brace
        (setq open-brace match-character)
        ;; Don't want to include the character we are on when
        ;; searching starts so move forward one character position.
        (forward-char)
        (setq stop-flag nil)
        (while (and (not (= total-count 0)) (not stop-flag))
          (setq search-string
                (concat "["
                        (char-to-string close-brace)
                        (char-to-string open-brace)
                        "]"))
          (if (re-search-forward search-string limit t)
              (if (char-equal (preceding-char) close-brace)
                  (setq total-count (- total-count 1))
                (setq total-count (+ total-count 1)))
            (progn
              (if (not limit)
                  (message "Match search failed!"))
              (goto-char current-pos)
              (setq stop-flag t))))

        (if (char-equal (preceding-char) close-brace)
            (backward-char))))
    (point)))


;;
;; Block out forward and backward char motion.
;;
(defun else-menu-block-movement ()
  ;;  [Documentation]
  (interactive)
  (let ()))

;;
;; Next line in the menu, wraps when moving past last line
;;
(defun else-menu-previous-line ()
  ;;  [Documentation]
  (interactive)
  (let ()
    (progn
      (if (not (= (point) (point-min)))
          (progn
            (backward-char)
            (beginning-of-line))
        (progn
          (goto-char (1- (point-max)))
          (beginning-of-line))))))

;;
;; Quit the menu pick list processing.
;;
(defun else-menu-quit ()
  "Quit from the menu pick list."
  (interactive)
  (let ()
    (setq else-selected-text nil)
    (exit-recursive-edit)))

;;
;; Next line in the menu, wraps when moving past last line
;;
(defun else-menu-next-line (&optional n)
  ;;  [Documentation]
  (interactive "p")
  (let ((count (or n 1)))
    (progn
      (while (> count 0)
        (end-of-line)
        (if (not (= (point) (1- (point-max))))
            (forward-char)
          (goto-char (point-min)))

        (setq count (1- count))))))

(defun else-extract-item (menu-text)
  "Extract the menu data from a menu line item. Each menu item is
encoded with extraneous information for display to the user i.e. {}'s,
description text etc" 
  (let ((result "")
        (start-char))
    (save-match-data
      ;; Get the first character in the line - this will determine
      ;; what type of information is to be extracted i.e. a
      ;; placeholder will have a '{' character, a token or direct text
      ;; substitution will have a quote character
      (setq start-char (elt menu-text 0))
      (cond ((char-equal start-char ?\")
             ;; Find the last occurrence of a " character.....
             (string-match "\".*\"+" menu-text 0)
             (setq result (substring menu-text 1
                                     (1- (match-end 0)))))
            ((char-equal start-char ?{)
             ;; The menu item is a placeholder, so select text between
             ;; {}'s
             (setq result (substring menu-text 1 (string-match "}" menu-text))))
            (t
             (setq result (substring menu-text 1 (1- (length menu-text))))))
      result)))

;;
;; Select the pick item at `point'.
;;
(defun else-menu-select ()
  "Select the menu pick item at `point'."
  (interactive)
  (let ((start-pos))
    (save-excursion
      (beginning-of-line)
      (setq start-pos (point))
      (end-of-line)
      (setq else-selected-text (else-extract-item
                                (buffer-substring start-pos
                                                  (point))))
      (message "%s" (buffer-substring start-pos
                                      (point))))
    (exit-recursive-edit)))

(defun else-mode (&optional arg)
  "Invoke ELSE mode for the current buffer.
Keybindings:
\\{else-menu-mode-map}"
  (interactive)
  (let ((language-assoc nil)
        (name)
        (did-it-work t))
    (progn
      (setq else-mode
            (if (null arg)
                (not else-mode)
              (> (prefix-numeric-value arg) 0)
              ))
      (if else-mode
          (progn
            ;; Make sure that the Xemacs variables are established. This code is
            ;; not guarded - it doesn't hurt FSF Emacs.
            (if (not (markerp else-move-to-position))
                (setq else-move-to-position (make-marker)))

            ;; Make sure the flag used by the after-change function is reset
            ;; (should never be set at this point anyway).
            (setq else-move-change nil)

            (setq else-Auto-Sub-Active nil)
            ;; Initialise the auto substitute marker list. The marker in this
            ;; list are set to nowhere when they are not active. The structure
            ;; of the list is (n (f . b) ...) where n is the number of marker
            ;; pairs and (f . b) is a marker pair where f is the front marker
            ;; and b is the back marker. This list can grow as required, but
            ;; will set it to a value that should be reasonable for most
            ;; applications.
            (if (not else-Auto-Sub-Marker-List)
                (setq else-Auto-Sub-Marker-List
                      (list 6
                            (cons (make-marker) (make-marker))
                            (cons (make-marker) (make-marker))
                            (cons (make-marker) (make-marker))
                            (cons (make-marker) (make-marker))
                            (cons (make-marker) (make-marker))
                            (cons (make-marker) (make-marker)))))

            ;; Check if a language is loaded for the buffer
            (if else-Current-Language
                (progn
                  (setq name else-Current-Language)
                  (setq language-assoc (cdr (assoc name
                                                   else-Language-Definitions))))

              ;; Possible that the language is already loaded for another
              ;; buffer so check using the mode-name.
              (setq language-assoc (cdr (assoc mode-name
                                               else-Language-Definitions))))
            (setq did-it-work
                  (catch 'foo
                    (if (not language-assoc)
                        (progn
                          (if (else-load-template)
                              (progn
                                (setq name else-Current-Language)
                                (setq language-assoc
                                      (cdr
                                       (assoc name else-Language-Definitions))))
                            ;; It didn't work, so bail out
                            (throw 'foo nil)))

                      ;; Yes, the language is already loaded so make
                      ;; sure we set the following variable!
                      (setq else-Current-Language mode-name))

                    (setq Placeholder (car language-assoc))
                    (setq Token (car (cdr language-assoc)))
                    (setq Language-Specifics (car (cdr (cdr language-assoc))))
                    (else-setup-change-hooks)

                    (if (= (buffer-size) 0)
                        (progn
                          (insert (cdr (assoc else-Initial-String-ref
                                              Language-Specifics)))
                          (goto-char 0)
                          (else-next-placeholder)))

                    ;; Add ELSE to the status bar.
                    (or (assq 'else-mode minor-mode-alist)
                        (setq minor-mode-alist
                              (cons '(else-mode " ELSE") minor-mode-alist)))
                    ;; Set the "self-insert" character array for the buffer
                    ;; ie. else-before-change checks whether a command is a
                    ;; "self-insert" character using the language
                    ;; attributes. We do this to maintain compatibility with
                    ;; Emacs 19.X (note, Elisp change after 19.29 means that
                    ;; only 19.29 -> will work).
                    (else-set-self-insert-vector)
                    ;; Set the result to t, ie we have successfully found
                    ;; a language template.
                    t))

            (if (not did-it-work)
                ;; Things didn't work, so make sure that the mode is not
                ;; falsely enabled.
                (setq else-mode (not else-mode))
              ;; Otherwise, make sure some simple commands are set up in the
              ;; ELSE menu keymap.
              (progn
                (else-enable-dups else-menu-mode-map
                                  'else-expand-placeholder 'else-menu-select)
                (else-enable-dups else-menu-mode-map
                                  'next-line 'else-menu-next-line)
                (else-enable-dups else-menu-mode-map
                                  'previous-line 'else-menu-previous-line)
                (else-enable-dups else-menu-mode-map 'forward-char 'undefined)
                (else-enable-dups else-menu-mode-map 'backward-char 'undefined)

                ;; Now setup the minor mode map. Note the 'or' will
                ;; "short-circuit" if the first form evaluates to 'true'.
                (or (assq 'else-mode minor-mode-map-alist)
                    (setq minor-mode-map-alist
                          (cons (cons 'else-mode else-mode-key-map)
                                minor-mode-map-alist))))))
        ;;
        ;; Else the mode has just been toggled off so clean up on things.
        ;;
        (else-mode-clean-up))
      ;; Return the result to the caller
      did-it-work)))

;;
;; Do a clean up. Consists of deleting buffer local variables and removing
;; functions from various change hooks.
;;
(defun else-mode-clean-up ()
  (let ((sub-length))
    ;; Set the auto sub marker list to point to nowhere
    (if else-Auto-Sub-Marker-List
        (progn
          (setq sub-length (car else-Auto-Sub-Marker-List))
          (while (> sub-length 0)
             (set-marker (car (nth sub-length else-Auto-Sub-Marker-List))
                         nil)
             (set-marker (cdr (nth sub-length else-Auto-Sub-Marker-List))
                         nil)
             (setq sub-length (1- sub-length)))))

    (setq else-Auto-Sub-Active nil)
    (kill-local-variable 'else-Auto-Sub-Active)
    (kill-local-variable 'else-Auto-Sub-Marker-List)
    (kill-local-variable 'else-Current-Language)
    (kill-local-variable 'else-mode)
    (kill-local-variable 'Placeholder)
    (kill-local-variable 'Token)
    (kill-local-variable 'Language-Specifics)
    (kill-local-variable 'else-Current-Language)

    (remove-hook 'before-change-functions
                 'else-before-change
                 t)
    (remove-hook 'after-change-functions
                 'else-after-change
                 t)))

(defun else-move-n-placeholders (&optional arg)
  "Move forward or backward (determined by 'else-direction) 'n placeholders."
  (interactive "p")
  (if else-direction
      (else-next-placeholder arg)
    (else-previous-placeholder arg)))

;;
;; Position `point' in the next available placeholder.
;;
(defun else-next-placeholder (&optional n)
  "Position `point' in the next available placeholder."
  (interactive "p")
  (let ((origin (point))
        (no-more nil)
        (target-pos)
        (target-count)
        (count (or n 1)))

    (catch 'problem
      (if (not else-mode)
          (progn
            (error "ELSE mode not enabled.")
            (throw 'problem nil)))

      (setq target-count count)
      ;; Now move forward "n" placeholders. Will stop at the last one found if
      ;; there are less than "n". Note that we use else-placeholder-start and
      ;; else-placeholder-end in this routine.
      (while (and (not no-more) (> count 0))
        (if (re-search-forward "[[{]" nil t)
            (progn
              (if (else-scan-for-match "[]}]" nil)
                  (progn
                    (backward-char 2)
                    (if (else-in-placeholder)
                        (setq count (1- count))
                      (forward-char 2)))))

          (setq no-more t)))

      ;; User may have asked to go forward more placeholders than exist, so
      ;; either stop when the requested count is reached or position to the last
      ;; one located. If we matched all of the requested placeholders then count
      ;; will be 0, if we ran out of placeholders but still managed to find at
      ;; least one (as manifested by (not (= count target-count)) then position
      ;; point to the last placeholder found.
      (if (or (= count 0) (not (= count target-count)))
          ;; position mid-way in the placeholder
          (setq target-pos (+ else-placeholder-start
                              (/ (- else-placeholder-end else-placeholder-start)
                                 2)))
        (setq target-pos origin))

      (goto-char target-pos)
      ;; If found a new placeholder to go to, set the overlay onto it
      (if (and else-experimental-code-flag
               (not (equal origin target-pos)))
          (else-set-overlay-here))

      ;; Callers expect to be told the whether the function succeeded
      ;; or not by testing whether point has moved - not a very good
      ;; idea at all! 
      target-pos)))

;;
;; Move `point' to the previous placeholder.
;;
(defun else-previous-placeholder (&optional n)
  "Move `point' to any previously available placeholder."
  (interactive "p")
  (let ((origin (point))
        (no-more nil)
        (target-pos)
        (target-count)
        (count (or n 1)))

    (catch 'problem
      (if (not else-mode)
          (progn
            (error "ELSE mode not enabled.")
            (throw 'problem nil)))

      (setq target-count count)
      ;; This bit of code makes sure that we get out of any "current"
      ;; placeholder i.e. otherwise we won't move.
      (if (else-in-placeholder)
          (progn
            ;; Find the beginning of a placeholder string
            (else-scan-for-match "[[{]" nil t)
            ;; step back onto the actual character of the placeholder in
            ;; preparation for the next backward search.
            (if (> (point) (+ (point-min) 2))
                (backward-char 2))))

      ;; Now position to the count'th placeholder
      (while (and (> count 0) (not no-more))
        ;; Locate the start of a brace or bracket and then test if it is a
        ;; placeholder. Continue this process until either a valid placeholder
        ;; has been found or the search has hit up against the beginning of
        ;; buffer
        (if (re-search-backward "[[{]" nil t)
            (progn
              (forward-char 1)
              ;; Test if in a valid placeholder.
              (if (else-in-placeholder)
                  (setq count (1- count)))

              ;; always go back one character to make sure we are not still in
              ;; the placeholder.
              (backward-char 1))

          ;; search has failed, so we must be up against the beginning of buffer.
          (setq no-more t)))

      ;; User may have asked to go backward for more placeholder than exist, so
      ;; either stop when the count is reached or position to the last one
      ;; located. If we matched all of the requested placeholders then count
      ;; will be 0, if we ran out of placeholders but still managed to find at
      ;; least one (as manifested by (not (= count target-count)) then position
      ;; point to the last placeholder found.
      (if (or (= count 0) (not (= count target-count)))
          (setq target-pos (+ else-placeholder-start
                              (/ (- else-placeholder-end
                                    else-placeholder-start) 2)))
        (setq target-pos origin))
      (goto-char target-pos)
      
      ;; If found a new placeholder to go to, set the overlay onto it
      (if (and else-experimental-code-flag
               (not (equal origin target-pos)))
          (else-set-overlay-here)))))

;;
;; This routine is called to process the placeholder/token depending upon
;; the definition of the element.
;;
(defun else-process-definition (element def-type)
  (let ((body-list)
        (service-type)
        (selected-element)
        (result t)
        (selected-type)
        (dummy)
        (placeholder-reference)
        (place-name)
        (action-struct (get element 'else-elisp-action)))
    ;; Check for /BEFORE processing - note the protection against an elisp
    ;; function that is not defined.
    (condition-case err
        (if (assoc else-before-key action-struct)
            (funcall (intern-soft (cdr (assoc else-before-key 
                                              action-struct)))))
      (void-function
       (message "Symbol's function definition is void: %s" 
                (cdr (assoc else-before-key action-struct))))
      (error 
       (message "%s" (error-message-string err))))

    (if (char-equal def-type ?p)
        (progn
          (setq service-type (get element 'else-type-ref))
          (cond ((char-equal service-type ?m)
                 (progn
                   (setq dummy (else-get-menu-entries element))
                   ;; Display the menu choices to the user
                   (else-display-menu dummy)

                   ;; Now have to search out which entry in the menu body of
                   ;; the definition. The interesting aspect of this is that we
                   ;; have a list consisting of 'body elements' i.e. a list of
                   ;; data and the text of the selected item. However, the text
                   ;; is just one of the elements in a 'body element' object. So
                   ;; the code has to search the list of body elements looking
                   ;; for an element that contains the selected textual string.
                   (if else-selected-text
                       (progn
                         (setq body-list dummy)
                         (while (not (string=
                                      else-selected-text
                                      (nth else-body-text-ref
                                           (car body-list))))
                           (setq body-list (cdr body-list)))
                         (setq selected-type (nth else-body-type-ref
                                                  (car body-list)))
                         ;; if its a /placeholder or /token then look-up
                         ;; otherwise it must be a text substitution.
                         (if selected-type
                             (setq selected-element
                                   (else-look-up else-selected-text
                                                 selected-type))
                           (setq selected-element nil))
                         (if selected-element
                             (setq result (else-process-definition
                                           selected-element
                                           (nth else-body-type-ref
                                                (car body-list))))
                           (else-substitute else-selected-text nil)))
                     ;;
                     ;; Else no menu element selected (User quit or
                     ;; something) 
                     ;;
                     (setq result nil))))
                ((char-equal service-type ?n)
                 (else-substitute element def-type))
                ((char-equal service-type ?t)
                 ;; Display the prompt string to the user
                 (else-display-menu (else-get-body element) t)
                 (setq result nil))))
      ;; else case, the definition must be a token. There are two
      ;; possibilities here, the token may be just referencing a
      ;; placeholder or it may contain "self-insert" lines of text. 
      (setq placeholder-reference (get element 'else-placeholder-ref))
      ;; Check if there is a forward reference to a placeholder
      (if (car (cdr placeholder-reference))
          (progn
            (setq place-name (car (cdr placeholder-reference)))
            (if (else-look-up place-name ?p)
                (else-process-definition (else-look-up place-name ?p) ?p)))
        ;; else there is not a forward reference, so process the
        ;; 'body' of element
        (else-substitute element def-type)))
    (if result
        ;; Check for /BEFORE processing
        (condition-case err
            (if (assoc else-after-key action-struct)
                (funcall (intern-soft (cdr (assoc else-after-key
                                                  action-struct)))))
          (void-function
           (message "Symbol's function definition is void: %s" 
                    (cdr (assoc else-after-key action-struct))))
          (error 
           (message "%s" (error-message-string err)))))
    result))

;;
;; This routines reads a placeholder/token definition from the current buffer,
;; it assumes that the match-data contains a successful result of searching
;; for a definition command sequence.
;;
(defun else-read-a-definition ()
  (let ((definition-type nil)
        (definition-complete nil)
        (definition-name)
        (this-line)
        (this-definition nil)
        (already-exists nil)
        (language-name "")
        (current-language else-Current-Language)
        (defining-type)
        (original-name "")
        (action-struct '())
        (elisp-function-name)
        (body-text-info (cons 0 nil)))

    ;; Get the definition type ie either PLACEHOLDER, TOKEN or LANGUAGE
    (setq defining-type (match-string else-defining-type))
    (cond ((string= defining-type "PLACEHOLDER")
           (setq definition-type ?p))
          ((string= defining-type "TOKEN")
           (setq definition-type ?t))
          ((string= defining-type "LANGUAGE")
           (else-read-language-definition))
          (t
           ;; Note this error will never be thrown as the string
           ;; matching precludes catching errors in this line... 
           (throw 'compile "illegal definition type")))
    (if definition-type
        (progn
          ;;
          ;; Grab the definition name then move on
          ;;
          (setq original-name 
                (else-strip-quotes (match-string else-defining-name)))
          (setq definition-name (upcase original-name))
          ;;
          ;; Must extract the "Language" for which the definition is being made
          ;; so extract the Language name and set the appropriate variables
          ;; prior to any other operations
          (forward-line)
          (if (else-scan-for-match else-body-string nil)
              (progn
                (setq this-line (match-string else-body-command-1))
                (if (string= this-line "/LANGUAGE")
                    (progn
                      (setq language-name
                            (else-strip-quotes
                             (match-string else-body-command-2)))
                      (if (else-establish-language language-name)
                          (progn
                            (if (else-look-up definition-name definition-type)
                                ;; The definition already exists, but also have
                                ;;  to checked if 'deleted'
                                (if (else-look-up definition-name
                                                  definition-type)
                                    (progn
                                      (message "\`%s\` already exists!"
                                               definition-name)
                                      (setq already-exists t)))))
                        (message "Language definition \`%s\` doesn't exist!"
                                 language-name)
                        ;; Want to stop the processing at this point,
                        ;; so use already-exists 
                        (setq already-exists t))))))

          (if (not already-exists)
              (progn
                ;; Get an entry in the appropriate obarray for the definition
                (setq this-definition (else-get-entry
                                       definition-name definition-type))

                ;; Now process the "body" of the template definition - before
                ;; doing though, create the data structure that will hold the
                ;; various members of a definition etc - these will have
                ;; "default" values and that way if a user decides not to
                ;; specify something there will be no "errors" later on.
                (else-initialise-symbol this-definition)

                ;; save the original-name
                (put this-definition 'else-original-name original-name)

                (forward-line)
                (while (and (not definition-complete)
                            (not (= (point) (point-max))))
                  (progn
                    (if (else-scan-for-match else-body-string nil)
                        (progn
                          (cond
                           ((setq this-line (match-string else-body-command-1))
                            ;; Have a line of the form /[...]=, so process it.
                            (cond ((string= this-line "/DUPLICATION")
                                   (put this-definition 'else-duplication-ref 
                                        (else-extract-duplication-info)))
                                  
                                  ((string= this-line "/SEPARATOR")
                                   (put this-definition 'else-separator-ref
                                        (else-strip-quotes
                                         (match-string else-body-command-2))))
                                  
                                  ((string= this-line "/DESCRIPTION")
                                   (put this-definition 'else-description-ref
                                        (else-strip-quotes
                                         (match-string else-body-command-2))))

                                  ((string= this-line "/TYPE")
                                   (put this-definition 'else-type-ref
                                        (else-extract-type-info)))

                                  ((string= this-line "/PLACEHOLDER")
                                   (put this-definition 'else-placeholder-ref
                                        (list t (else-strip-quotes
                                                 (match-string else-body-command-2)))))

                                  ((string= this-line "/SUBSTITUTE_COUNT")
                                   (put this-definition 'else-substitute-count-ref 
                                        (string-to-number
                                         (else-strip-quotes
                                          (match-string else-body-command-2))))
                                   (if (equal (get this-definition
                                                   'else-substitute-count-ref) 0)
                                       (throw 
                                        'compile "illegal value for substitute count")))

                                  ((string= this-line "/RUN_CODE")
                                   (setq elisp-function-name 
                                         (format "%s" (match-string 
                                                       else-body-command-2)))
                                   (setq this-line (match-string
                                                    else-body-command-3))
                                   ;; Now do a series of if statements based
                                   ;; upon the options - they may all be set
                                   (if (string-match else-before-key this-line)
                                       (push (cons else-before-key 
                                                   elisp-function-name) 
                                             action-struct))
                                   (if (string-match else-after-key this-line)
                                       (push (cons else-after-key 
                                                   elisp-function-name) 
                                             action-struct))
                                   (if (string-match else-oninsert-key this-line)
                                       (push (cons else-oninsert-key 
                                                   elisp-function-name) 
                                             action-struct)))))
                           ((setq this-line (match-string else-body-command-4))
                            (cond ((string= this-line "/NOAUTO_SUBSTITUTE")
                                   (put this-definition 'else-substitute-ref ?n))
                                  ((string= this-line "/AUTO_SUBSTITUTE")
                                   (put this-definition 'else-substitute-ref ?a))
                                  ))
                           ((setq this-line (match-string else-body-text-1))
                            (setq body-text-info
                                  (else-extract-body (car body-text-info)))
                            (put this-definition 'else-body-ref
                                 (cons (cdr body-text-info)
                                       (get this-definition 'else-body-ref))))
                           ((setq this-line (match-string else-body-end))
                            (setq definition-complete t)))))
                    (forward-line)))        ; Go to the start of the next line.

                ;; Definition is now complete - the action-struct hasn't been
                ;; put into the definition as yet though, so do that now.
                (put this-definition 'else-elisp-action 
                     (reverse (copy-alist action-struct)))

                ;; reverse the body list
                (if (get this-definition 'else-body-ref)
                    (put this-definition 'else-body-ref
                         (reverse (get this-definition 'else-body-ref))))))

          ;; restore the 'original' language i.e. might have template language
          ;; enabled for a .lse file that contains definitions for some other
          ;; language - so compiling a definition shouldn't destroy the
          ;; current language settings for the buffer.
          (else-establish-language current-language)))))

;;
;; Read or process the language definition of a template language.
;;
(defun else-read-language-definition ()
  "Parse a language definition statement."
  (let ((language-name "")
        (current-language else-Current-Language)
        (definition-complete nil)
        (this-line)
        (tab-size-specified nil))
    (progn
      (setq language-name
            (else-strip-quotes (match-string else-defining-name)))
      ;; Check if there is a set of existing definitions for this
      ;; language, if not then create a blank set of variables.
      (if (else-check-and-init-globals language-name)
          (message "Language %s exists, assuming attribute modification" 
                   language-name))

      ;; The caller may be defining an entirely new language definition or
      ;; just modifying attributes of a current definition. In both cases we
      ;; will modify the attributes of the language definition that
      ;; was set by the previous call. This allows the overriding of
      ;; attributes such as indentation setting in the users
      ;; customisation file.
      (else-establish-language language-name)

      (forward-line)
      (while (and (not definition-complete)
                  (not (= (point) (point-max))))
        (progn
          (if (else-scan-for-match else-body-string nil)
              (progn
                (setq this-line (match-string else-body-command-1))
                (if this-line
                    (cond ((string= this-line "/INITIAL_STRING")
                           (setcdr (assoc else-Initial-String-ref
                                          Language-Specifics)
                                   (else-strip-quotes
                                    (match-string else-body-command-2))))
                          ((string= this-line "/PUNCTUATION_CHARACTERS")
                           (setcdr (assoc else-Punctuation-ref
                                          Language-Specifics)
                                   (else-strip-quotes
                                    (match-string else-body-command-2)))
                           (setcdr (assoc else-Punctuation-Length-ref
                                          Language-Specifics)
                                   (length (cdr (assoc else-Punctuation-ref
                                                       Language-Specifics)))))
                          ((string= this-line "/SELF_INSERT_CHARACTERS")
                           (setcdr (assoc else-Self-Insert-Characters-ref
                                          Language-Specifics)
                                   (else-strip-quotes
                                    (match-string else-body-command-2)))
                           (setcdr (assoc else-Self-Insert-Characters-Length-ref
                                          Language-Specifics)
                                   (length (cdr (assoc else-Self-Insert-Characters-ref
                                                       Language-Specifics)))))
                          ((string= this-line
                                    "/VALID_IDENTIFIER_CHARACTERS")
                           (setcdr (assoc else-Valid-Idents-ref
                                          Language-Specifics)
                                   (else-strip-quotes
                                    (match-string else-body-command-2)))
                           (setcdr (assoc else-Valid-Idents-Length-ref
                                          Language-Specifics)
                                   (length (cdr (assoc else-Valid-Idents-ref
                                                       Language-Specifics)))))
                          ((string= this-line
                                    "/INDENT_SIZE")
                           ;; Expect a numerical value here.
                           (setcdr (assoc else-tab-size-ref
                                          Language-Specifics)
                                   (string-to-number
                                    (else-strip-quotes
                                     (match-string
                                      else-body-command-2))))
                           (setq tab-size-specified t))
                          ((string= this-line
                                    "/VERSION")
                           (setcdr (assoc else-language-version-ref
                                          Language-Specifics)
                                   (else-strip-quotes
                                    (match-string else-body-command-2))))
                          (t
                           (throw 'compile
                                  "language specifier not recognised")))

                  ;; The else case, there is either an error or its the
                  ;; language definition end command, which is tested
                  ;; purely by something being in else-body-end slot
                  ;; of the match-data.
                  (if (match-string else-body-end)
                      (progn
                        (setq definition-complete t)
                        ;; Check to see if a /INDENT_SIZE attribute
                        ;; was specified or not, issue a warning
                        ;; message if not
                        (if (not tab-size-specified)
                            (message "/INDENT_SIZE not specified assuming %s spaces"
                                     (cdr (assoc else-tab-size-ref
                                                 Language-Specifics)))))
                    (throw 'compile "Unknown Error"))))))
        (forward-line))             ; Go to the start of the next line.

      ;; restore the 'original' language i.e. might have template
      ;; language enabled for a .lse file that contains definitions
      ;; for some other language - so compiling a definition shouldn't
      ;; destroy the current language settings for the buffer.
      (else-establish-language current-language))))

;; This routine is called after the process has defined a definition which
;; should replace the placeholder/token being replaced. It assumes that the
;; string being expanded has been deleted and placed into the
;; else-deleted-string variable.
;;
;; Add some basic code here that makes sure Context_dependent work better!
;; ie
;; if <<there is nothing else preceeding the placeholder>> AND
;;    <<we can locate a separator string immediately preceeding>> then
;;    Vertical substitution is appropriate
;; else
;;    Perform Horizontal substitution
;; end if
;;
(defun else-replicate-placeholder-string (duplication-type
                                          indent-column
                                          element-def)
  (let ((separator-type)
        (newline-at-start)
        (cur-column indent-column))
    (progn
      ;; Extract the separator string, test whether it contains a ^M
      ;; at the beginning and then split it across any embedded ^M's
      ;; to create a list of separator texts - I imagine this later
      ;; case will almost never happen?
      (setq separator-type (get element-def 'else-separator-ref))

      ;; Test for leading newline

      (if (> (length separator-type) 0)
          (setq newline-at-start (char-equal (aref separator-type 0) ?\r))
        (setq newline-at-start nil))

      (cond ((char-equal duplication-type ?v)
             ;; (setq separator-type (get element-def 'else-separator-ref))
             (if (> (length separator-type) 0)
                 (progn
                   ;; Now split the string around any embedded
                   ;; newlines - this effectively deletes any leading
                   ;; newline character.
                   (setq separator-type (split-string separator-type 
                                                      (string ?\r)))
                   (if newline-at-start
                       (newline))
                   
                   (while separator-type
                     (insert (car separator-type))
                     (setq separator-type (cdr separator-type))
                     (if separator-type
                       (newline)))))
             (newline)
             (indent-to cur-column)
             (insert (concat "[" else-definition-name "]"))
             (insert "..."))

            ((char-equal duplication-type ?h)
             (if separator-type
                 (progn
                   (insert separator-type)
                   (insert (concat "[" else-definition-name "]"))
                   (insert "..."))))

            ;; This case 'can't' happen because the duplication
            ;; requirements are a pre-requisite for this defun.
            ((char-equal duplication-type ?c)
             (message "Invalid duplication in else-replicate-placeholder-string"))))))


(defun else-restore (language-name)
  "Read the Emacs Lisp objects that represent a pre-compiled language definition."
  (let ((this-definition)
        (entire)
        (read-type)
        (obarray-name)
        (continue t)
        (result t))
    (progn
      (save-excursion
        (setq else-read-marker (point-marker)))

      (condition-case nil
          (progn
            (else-check-and-init-globals language-name)
            (setq Language-Specifics (read else-read-marker))
            (while continue
              (setq entire (read else-read-marker))
              (if entire
                  (progn
                    (setq read-type (nth 1 entire))
                    (cond ((char-equal read-type ?p)
                           ;; Actions for a placeholder
                           (setq obarray-name Placeholder))
                          ((char-equal read-type ?t)
                           ;; Actions for a token
                           (setq obarray-name Token))
                          (t
                           (setq result nil)))

                    (unintern (car entire) obarray-name)
                    (setq this-definition (else-get-entry (car entire) read-type))
                    (setplist this-definition (nth 2 entire))))))
        (end-of-file nil))
      result)))

(defun else-return-sorted-list (this-obarray)
  "Extract all of the names from the obarray, sort them alphabetically
and return them as a list."
  (let ((sorted-list)
        (case-fold-search nil))
    ;; Put all of the names into a list
    (mapatoms '(lambda (obarray-element) 
                 (push (get obarray-element 'else-original-name) 
                       sorted-list)) this-obarray)

    ;; Now sort the list alphabetically
    ;;(setq sorted-list (sort sorted-list 'string<))
    (setq sorted-list 
          (sort sorted-list 
                (lambda (left right)
                  (< (compare-strings left 0 nil
                                      right 0 nil
                                      else-ignore-case-in-name-sorts)
                     0))))))

;;
;; Generalised search routine that provides access to either regular searches
;; or regular expression searches either forward or backward of `point' but
;; ultimately search limited to the current line.
;;
(defun else-scan-for-match (match-string regexp &optional direction-reverse)
  (let ((current-position) (search-limit))

    ;;
    ;; Find the limit of the search based upon whether it is in the
    ;; forward (default or reverse direction and provide it as a limit
    ;; for the search. Note that if the search is successful then point
    ;; is positioned after the end of the word that has been matched.
    ;;
    (setq current-position (point))
    (if direction-reverse
        (beginning-of-line)
      (end-of-line))
    (setq search-limit (point))
    (goto-char current-position)
    (if regexp
        (if direction-reverse
            (search-backward match-string search-limit t)
          (search-forward match-string search-limit t))

      (if direction-reverse
          (re-search-backward match-string search-limit t)
        (re-search-forward match-string search-limit t)))))

(defun else-search-load-path (name &optional check-local-dir-first)
  "Search load path for file 'name', returns path plus name.
If 'check-local-dir-first' is t then it will check for the file in the
'current' directory prior to searching the load path."
  (let ((search-path load-path)
        (found nil)
        (this-attempt))
    (progn
      (if check-local-dir-first
          (progn
            (setq this-attempt (expand-file-name
                                (concat "./" name)))
            (setq found (file-exists-p this-attempt))))

      (while (and search-path
                  (not found))
        (setq this-attempt (expand-file-name
                            (concat (car search-path)
                                    "/"
                                    name)))
        (setq found (file-exists-p this-attempt))
        (setq search-path (cdr search-path)))

      (if found
          this-attempt
        nil))))


(defun else-set-overlay-here ()
  "If there is an active overlay then send the menu choices string to
the hook function."
  (let ((definition-type nil)
        (possible-choices nil)
        (menu-string ""))
    (if (overlayp else-placeholder-overlay)
        ;; The overlay already exists, so just move it
        (move-overlay else-placeholder-overlay 
                      (1+ else-placeholder-start)  
                      (1- else-placeholder-end))
      ;; else create the overlay and associate the face to it
      (setq else-placeholder-overlay 
            (make-overlay (1+ else-placeholder-start)
                          (1- else-placeholder-end)))
      (overlay-put else-placeholder-overlay
                   'face 'else-placeholder-face))

    ;; Now check if the placeholder is a menu item, if so then load up
    ;; the menu information and call the VoiceCoder hook.
    (setq definition-type (get else-current-definition
                               'else-type-ref))
    (setq possible-choices nil)
    (if (char-equal definition-type ?m)
        ;; It is a menu, so get all of the possible choices and then
        ;; send them to the functions that have registered on the
        ;; hook.
        (progn
          (setq possible-choices (else-get-menu-entries
                                  else-current-definition))
          (dolist (match-element possible-choices)
            (setq menu-string 
                  (concat menu-string 
                          (else-display-menu-element match-element)))))
      ;; the else is an empty menu string i.e. if the placeholder
      ;; isn't a menu then send a blank string to the hook functions
      )
    (run-hook-with-args 'else-menu-display-functions
                        menu-string)))


(defun else-set-self-insert-vector ()
  "Initialise else-before-change character look-up vectors."
  (let ((local-string)
        (local-string-length)
        (index-value))
    ;; Create a vector of 256 character code length - will this cause problems
    ;; with "other language" programming?
    (setq Language-Self-Insert-Characters-Vector
          (make-vector else-character-vector-length nil))
    (setq Language-Valid-Identifier-Characters-Vector
          (make-vector else-character-vector-length nil))
    (setq Language-Punctuation-Characters-Vector
          (make-vector else-character-vector-length nil))
    (setq local-string (cdr (assoc else-Valid-Idents-ref
                                   Language-Specifics)))
    (setq local-string-length (cdr (assoc else-Valid-Idents-Length-ref
                                          Language-Specifics)))
    (while (> local-string-length 0)
      (setq local-string-length (1- local-string-length))
      (setq index-value (aref local-string local-string-length))
      (aset Language-Valid-Identifier-Characters-Vector
            index-value
            t))

    (setq local-string (cdr (assoc else-Self-Insert-Characters-ref
                                   Language-Specifics)))
    (setq local-string-length (cdr (assoc else-Self-Insert-Characters-Length-ref
                                          Language-Specifics)))
    (while (> local-string-length 0)
      (setq local-string-length (1- local-string-length))
      (setq index-value (aref local-string local-string-length))
      (aset Language-Self-Insert-Characters-Vector
            index-value
            t))

    (setq local-string (cdr (assoc else-Punctuation-ref
                                   Language-Specifics)))
    (setq local-string-length (cdr (assoc else-Punctuation-Length-ref
                                          Language-Specifics)))
    (while (> local-string-length 0)
      (setq local-string-length (1- local-string-length))
      (setq index-value (aref local-string local-string-length))
      (aset Language-Punctuation-Characters-Vector
            index-value
            t))))

;; Set up the before and after change functions and add them to the
;; appropriate change hooks. Note that the standard change hooks must be made
;; "local" to the buffer so as not to interfere with buffers that don't have
;; ELSE mode enabled.
(defun else-setup-change-hooks ()
  (let ()
    (make-local-hook 'after-change-functions)
    (make-local-hook 'before-change-functions)
    (add-hook 'before-change-functions
              'else-before-change
              t
              t)
    (add-hook 'after-change-functions
              'else-after-change
              nil
              t)

    (setq else-before-var nil)
    (setq else-after-var nil)))

(defun else-show-placeholder-names ()
  "Display names of all of the Placeholders in the current language template
set, sort them alphabetically and display them in a temporary buffer."
  (interactive)
  (let ((placeholder-list)
        (desc)
        (forward-ref)
        (element)
        (placeholder-length 0)
        (list-index)
        (output-format))
    (if (not else-mode)
      (progn
        (error "ELSE mode not enabled for this buffer."))
      (with-output-to-temp-buffer "*Available Placeholders*"
        ;; Put all of the placeholder names into an alphabetically sorted list
        (setq placeholder-list (else-return-sorted-list Placeholder))

        ;; Nice to have good formatting for the output - determine the
        ;; longest placeholder name and incorporate the length into the format
        ;; string. Start with a "default" length of the column header
        (setq list-index 0)
        (setq placeholder-length (length "Placeholder"))
        (while (< list-index (length placeholder-list))
          (if (> (length (nth list-index placeholder-list)) placeholder-length)
              (setq placeholder-length 
                    (length (nth list-index placeholder-list))))
          (setq list-index (+ 1 list-index)))

        (setq output-format (concat "%" (number-to-string placeholder-length)
                                    "s     %s" ))
        ;; Now insert them into the buffer at point? Attempt some nice
        ;; formatting at the same time
        (princ (format (concat "%" (number-to-string placeholder-length)
                               "s %s ******")
                       "****** Placeholders for" 
                       else-Current-Language))
        (terpri)
        (terpri)
        (princ (format output-format "Placeholder" "Description"))
        ;; newline to output buffer
        (terpri)
        (while placeholder-list
          ;; Get the element as it is used multiple times 
          (setq element (else-look-up (car placeholder-list) ?p))

          ;; Get the description 
          (setq desc (get element 'else-description-ref))

          ;; The placeholder may not have a description because it references
          ;; a placeholder, so take appropriate action in that
          ;; situation.
          (setq forward-ref (get element 'else-placeholder-ref))

          (if (and (string= desc "") (car forward-ref))
              ;; It is a forward reference, so derive the description
              ;; from that placeholder
              (setq desc (get (else-look-up (car (cdr forward-ref)) ?p)
                              'else-description-ref)))
          ;; Now format the output string
          (princ (format output-format (car placeholder-list) desc))
          (terpri)
          (setq placeholder-list (cdr placeholder-list)))))))

(defun else-show-token-names ()
  "Display names of all of the Tokens in the current language template
set, sort them alphabetically and display them in a temporary buffer."
  (interactive)
  (let ((token-list)
        (desc)
        (forward-ref)
        (element)
        (token-length 0)
        (list-index)
        (output-format))
    (if (not else-mode)
      (progn
        (error "ELSE mode not enabled for this buffer."))
      (with-output-to-temp-buffer "*Available Tokens*"
        ;; Put all of the token names into an alphabetically sorted list
        (setq token-list (else-return-sorted-list Token))

        ;; Nice to have good formatting for the output - determine the
        ;; longest token name and incorporate the length into the format
        ;; string. Start with a "default" length of the column header
        (setq list-index 0)
        (setq token-length (length "Token"))
        (while (< list-index (length token-list))
          (if (> (length (nth list-index token-list)) token-length)
              (setq token-length (length (nth list-index token-list))))
          (setq list-index (+ 1 list-index)))

        (setq output-format (concat "%" (number-to-string token-length)
                                    "s     %s" ))
        ;; Now insert them into the buffer at point? Attempt some nice
        ;; formatting at the same time
        (princ (format (concat "%" (number-to-string token-length)
                               "s %s ******")
                       "****** Tokens for" 
                       else-Current-Language))
        (terpri)
        (terpri)
        (princ (format output-format "Token" "Description"))
        ;; newline to output buffer
        (terpri)
        (while token-list
          ;; Get the element as it is used multiple times 
          (setq element (else-look-up (car token-list) ?t))

          ;; Get the description 
          (setq desc (get element 'else-description-ref))

          ;; The token may not have a description because it references
          ;; a placeholder, so take appropriate action in that
          ;; situation.
          (setq forward-ref (get element 'else-placeholder-ref))

          (if (and (string= desc "") (car forward-ref))
              ;; It is a forward reference, so derive the description
              ;; from that placeholder
              (setq desc (get (else-look-up (car (cdr forward-ref)) ?p)
                              'else-description-ref)))
          ;; Now format the output string
          (princ (format output-format (car token-list) desc))
          (terpri)
          (setq token-list (cdr token-list)))))))

(defun else-store-element (s-symbol)
  "Called by mapatom to save the symbol to a file.
The file is indicated by 'else-read-marker'"
  (let ((this-definition)
        (property-list)
        (entire))
    (progn
      (setq this-definition s-symbol)
      (setq property-list (symbol-plist this-definition))
      (setq entire (list (symbol-name this-definition)
                         else-type-of-symbols
                         property-list))
      (print entire else-read-marker))))

;;
;; If ARG contains enclosing quotes, then strip them off.
;;
(defun else-strip-quotes (arg)
  (let ()
    (if (string= (substring arg 0 1) "\"")
        (substring arg 1 (1- (length arg)))
      arg)))

;; Called to provide a textual substitution of the current token or
;; placeholder with either a single line of text (because it was
;; selected from a menu) or the body of a placeholder.
;; element - either a string or a symbol
;; defn-type - token or placeholder type of the element being replaced.
(defun else-substitute (element defn-type)
  "[text]"
  (let ((lang-indent-size (cdr (assoc else-tab-size-ref Language-Specifics)))
        (adjust-factor 0)
        (duplication-direction 0)
        (text-to-insert "")
        (cur-column 0)
        (here))
    (condition-case nil
        (progn
          ;; Now process each line to be inserted. First though, turn off
          ;; processing by the before/after change hook functions by ELSE. 
          (remove-hook 'before-change-functions 'else-before-change t)
          (remove-hook 'after-change-functions  'else-after-change  t)

          ;; Grab a copy of the string that is about to be deleted - we may
          ;; use it later on, so here is a convenient place to take a copy.
          (setq else-deleted-string  (buffer-substring
                                      else-placeholder-start
                                      else-placeholder-end))

          ;; Delete the element that we are replacing - in both cases
          ;; we expect the else-placeholder-start/end variables to be
          ;; setup - this is a precondition of entry to this
          ;; defun. Note also that the definition type
          ;; i.e. placeholder or token of what is being deleted is not
          ;; necessarily the argument value of defn-type to this defun
          ;; i.e. at this point we may have followed several levels of
          ;; "indirection" to arrive at an actual point of
          ;; substitution. So depend on a global variable for the
          ;; element type of what we are deleting
          ;; (else-definition-type).
          (delete-region else-placeholder-start else-placeholder-end)
          (if (char-equal else-definition-type ?p)
              (progn
                (if else-please-duplicate
                    (progn 
                      (delete-char 3)
                      ;; What is the context of the duplication?
                      ;; i.e. if there is only whitespace prior to the
                      ;; placeholder then the duplication context is
                      ;; vertical, otherwise it is horizontal - the
                      ;; only way this can be overridden is if the
                      ;; placeholder definition contains something
                      ;; other than CONTEXT_DEPENDENT. So test for
                      ;; this first.
                      ;; If it is context dependent, then work out
                      ;; what the context is otherwise just assume the
                      ;; value in the definition.
                      (setq duplication-direction (get else-current-definition
                                                       'else-duplication-ref))
                      (if (char-equal duplication-direction ?c)
                          (progn
                            ;; Make sure the search doesn't change point
                            (setq here (point))
                            (if (not (else-scan-for-match "[^ \t]" nil t))
                                (setq duplication-direction ?v)
                              (setq duplication-direction ?h))
                            (goto-char here))))))
            ;; 
            )
          (setq else-deleted-column (current-column))

          ;; Now take action based upon the target substitution. What
          ;; we want here is to have a list of text line(s) to
          ;; insert. In the case of defun argument element being a
          ;; placeholder, then we need to extract all of the body text
          ;; into a list, when the defun argument is a single line
          ;; (line from a menu selection) then place that into the
          ;; same list element - this way we can process both cases
          ;; using the same code. Note that the resultant list, no
          ;; matter where the source came from has to contain elements
          ;; that have the same structure as each line of the body of
          ;; a placeholder.
          (if (symbolp element)
              (setq text-to-insert (get element 'else-body-ref))
            ;; object must be a string, so construct a standard single
            ;; line entry for the list i.e. it should have a structure
            ;; as defined in else-extract-body. This is easy because
            ;; we know that the element is a string for direct
            ;; insertion, so it is not a /PLACEHOLDER or /TOKEN, has a
            ;; zero level indent and the line is the string text.
            (setq text-to-insert (list (list nil 0 element))))

          (setq cur-column (current-column))
          (while text-to-insert
            (progn
              ;; Insert the correct number of spaces for the
              ;; indentation of the line.
              (insert (make-string
                       (* (nth else-body-indent-ref (car text-to-insert))
                          lang-indent-size)
                       ?\ ))
              (insert (nth else-body-text-ref (car text-to-insert)))
        
              ;; Now check for more lines of text to be inserted.
              (setq text-to-insert (cdr text-to-insert))
              (if text-to-insert
                  (progn
                    (newline)
                    (indent-to cur-column))))))

      (error nil)) ;; end of the condition-case
    (add-hook 'before-change-functions 'else-before-change t t)
    (add-hook 'after-change-functions  'else-after-change  nil t)
    ;; If the item just expanded was a placeholder and it should be
    ;; replicated then do it. 
    (if (and (char-equal else-definition-type ?p) else-please-duplicate)
        (else-replicate-placeholder-string duplication-direction 
                                           else-deleted-column 
                                           else-current-definition))))

(defun else-summary ()
  (interactive)
  (let ()
    (progn
      (message "s-elect, q-uit, n-ext line, p-revious line"))))

(defun else-toggle-direction ()
  "Toggle the value of the direction flag 'else-direction."
  (interactive)
  (setq else-direction (not else-direction)))


(defun else-uncomment-placeholders ()
  "Remove any comment prefixes from lines containing valid placeholders.
This function reverses the effects of else-comment-placeholders."
  (interactive)
  (let ((region-start)
        (region-end))
    (progn
      (save-excursion
        (goto-char (point-min))
        (while (not (= (point) (else-next-placeholder)))
          (progn
            (beginning-of-line)
            (if (looking-at (regexp-quote comment-start))
                (progn
                  (setq region-start (point))
                  (end-of-line)
                  (setq region-end (point))
                  (uncomment-region  region-start region-end)))
            (end-of-line)))))))

(defun else-find-template (template-name obarray-name)
  "Try completion for template-name in obarray-name."
  (let ((succeeded t)
        (matched-string)
        (match-data)
        (is-completions-displayed))
    (progn
      ;; Make sure that any existing completions display is deleted.
      (setq is-completions-displayed
            (get-buffer-window "*Completions*" 'visible))
      (if is-completions-displayed
          (progn
            (save-excursion
              (delete-window is-completions-displayed))))

      (setq matched-string (upcase template-name))
      (setq match-data (try-completion
                        matched-string
                        obarray-name))
      (cond ((or (equal match-data t)
                 (equal match-data nil))
             ;; Actions for an exact match
             (setq succeeded nil))
            (t
             ;; All else i.e. there is "longest" possible string
             (if (equal (length match-data)
                        (length matched-string))
                 (progn
                   ;; The two match, so therefore we have matched up
                   ;; until the unique portion but still not finished
                   ;; the completion i.e. there is more than one
                   ;; possible completion.
                   (with-output-to-temp-buffer "*Completions*"
                     (display-completion-list
                      (all-completions matched-string Placeholder)))
                   (setq succeeded nil))
               (progn
                 (setq match-data (downcase match-data))
                 (insert match-data)))))
      succeeded)))

(defun else-insert-placeholder ()
  "Given a string, lookup the template in the current language. When found,
insert the string at point."
  (interactive)
  (let ((here (point))
        (stop-location)
        (matched-string))
    (progn
      (beginning-of-line)
      (setq stop-location (point))
      (goto-char here)
      ;; Look backwards for either a "{" or a "[" character.
      (if (re-search-backward "[{[]" stop-location t)
          (progn
            (forward-char)
            (setq matched-string (buffer-substring (point) here))
            (delete-region (point) here)
            (if (not (else-find-template matched-string Placeholder))
                (progn
                  (insert matched-string)
                  (goto-char here))))
        (progn
          ;; This is the else case. Assume that a token is desired.
          (forward-word -1)
          (setq matched-string (buffer-substring (point) here))
          (delete-region (point) here)
          (if (not (else-find-template matched-string Token))
              (progn
                (insert matched-string)
                (goto-char here))))))))

(defun else-is-template-file-present (language-file-name)
  "Search the list of all buffers checking whether the file that each
visits is the target file - return t if there is a match."
  (let ((local-list (buffer-list))
        (result nil)
        this-buffer
        (abs-file-name (expand-file-name language-file-name)))
    ;; Search the list until empty or a match is found
    (while (and local-list (not result))

      (setq this-buffer (car local-list))
      (setq local-list (cdr local-list))

      (setq result (string= abs-file-name (buffer-file-name this-buffer))))
    result))

(defun else-wrap-region (begin end)
  "'wrap' a region of code with a placeholder definition.
The marked region will be moved into the first placeholder in the
second line of the placeholder being used."
  (interactive "r")
  (let ((template-name)
        (text-being-wrapped (buffer-substring begin end))
        (start-region)
        (end-region))
    (if (not else-mode)
        (error "Must have a language loaded to run this function.")
      (setq template-name
            (completing-read "Placeholder: " Placeholder))
      (delete-region begin end)
      (newline)
      (forward-line -1)
      (indent-for-tab-command)
      (setq start-region (point-marker))
      (insert (concat "{" template-name "}"))
      (else-previous-placeholder)
      (else-expand-placeholder)
      (goto-char start-region)
      (beginning-of-line)
      (forward-line)
      (else-next-placeholder)
      (insert text-being-wrapped)
      (setq end-region (point-marker))
      (indent-region start-region end-region nil))))

(defgroup ELSE nil
  "Custom variables for Emacs Language Sensitive Editor"
  :tag "Emacs LSE"
  :prefix "else"
  :group 'tools)

(defcustom else-prompt-time 3
  "Prompter Screen display time in seconds"
  :type 'integer
  :group 'ELSE)

(defcustom else-kill-proceed-to-next-placeholder nil
  "Should else-kill-placeholder goto next placeholder after a kill or not"
  :type 'boolean
  :group 'ELSE)

(defcustom else-set-lineno nil
  "Turn line numbering in the Menu buffer on or off. Requires setnu.el."
  :type 'boolean
  :group 'ELSE)

(defcustom else-move-and-execute nil
  "If set, then if a command fails then a movement-<execute> pair is assumed
where <execute> is the requested operation and the movement is determined by the
else-direction flag - note that the 'command' is currently restricted to expand
and kill operations only."
  :type 'boolean
  :group 'ELSE)

(defcustom else-direction t
  "If expand-or-move is enabled, this flag determines the direction of movement
   on  - next placeholder
   off - previous placeholder"
  :type 'boolean
  :group 'ELSE)

(defcustom else-only-proceed-within-window t
  "Move after a kill only if the next placeholder is visible in the current window.
This flag controls jumps when they are part of a composite action by ELSE
i.e. in kill-placeholder, if the kill-proceed flags is set then this flag
allows the move to the next placeholder only if it is visible in the current
window."
  :type 'boolean
  :group 'ELSE)

(defcustom else-follow-menus nil
  "If true then menu definitions are 'followed' or expanded until no sub-entry
menu is found and all are combined into a single menu selection
display at the 'top level'. If nil, then menu's are not expanded and
the user has to traverse sub-menu entries (useful when combining
menu's leads to huge menu selections)."
  :type 'boolean
  :group 'ELSE)

(defcustom else-nofollow-menus nil
  "If true then menu definitions are *not* 'followed'. If nil,
'else-follow-menus and then /NOFOLLOW or /FOLLOW attributes are tested to
determine the behaviour of menu displays. If set to t then this flag has
precedence over all other settings dealing with this bechaviour."
  :type 'boolean
  :group 'ELSE)

(make-variable-buffer-local 'else-follow-menus)
(make-variable-buffer-local 'else-nofollow-menus)

(defcustom else-experimental-code-flag nil
  "Protect experimental regions of the code. Leave set to nil unless you are sure
you can live with the consequences. Current behaviour being protected by the flag
is:
1. Creating an overlay for the last placeholder visited by else-next-placeholder
and else-previous-placeholder - this is code for VoiceCoder requirements."
  :type 'boolean
  :group 'ELSE)

(defface else-placeholder-face 
  '((((type tty) (class color)) (:foreground "green"))
     (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "ELSE mode face used for placeholder strings."
  :group 'ELSE)

(defcustom else-menu-display-functions nil
  "A hook by which functions communicate their interest in menu
selections when ELSE moves into a placeholder that is a menu
placeholder."
  :type '(repeat (cons string symbol))
  :group 'ELSE)

(defcustom else-ignore-case-in-name-sorts t
  "Ignore case (t) in sorting token names for display using
else-show-token-names"
  :type 'boolean
  :group 'ELSE)
 
;; make a unique copy for this buffer
(make-variable-buffer-local 'else-placeholder-overlay)

(provide 'else-mode)

;;; elsemode.el ends here
;; LocalWords:  elsemode Milliken dosuser peterm msg esl lse setq progn concat
;; LocalWords:  keymaps keymap eg ada lang keypress obarray mapatom nthcdr alist
;; LocalWords:  NOAUTOSUBSTITUTE da mapatoms se sl utils obarrays thru Ok defun
;; LocalWords:  fName Unintern cdr assoc NOAUTO NOLIST Placeholder's lineno VR
;; LocalWords:  regexp cust Keybindings dir ARG uit revious placeholders struct
;; LocalWords:  VoiceCoder count'th barfing XEmacs cond xemacs fns Monnier wrt
;; LocalWords:  setnu

;;; itunes.el --- control itunes from emacs
;;
;; ~/share/emacs/pkg/osx/osx-itunes.el ---
;;
;; $Id: osx-itunes.el,v 1.23 2004/04/15 02:41:10 harley Exp $
;;

;; Author:    Harley Gorrell <harley@mahalito.net>
;; URL:       http://www.mahalito.net/~harley/elisp/osx-itunes.el
;; License:   GPL v2
;; Keywords:  osx, osascript, itunes

;;; Commentary:
;;  * Now I dont have to touch a mouse to fiddle with itunes.
;;    Handy when you are remotely logged into the machine,
;;    or using a really large Emacs frame.
;;      (setq itunes-key [f5]) ;; or some other key.
;;      (require 'osx-itunes)  ;; this will bind the key.
;;  * Press the itunes key and then one of:
;;      key again            : toggle info window.
;;      +/-/up/down/mute/0-9 : volume control
;;      left/right           : prev/next track
;;      space                : toggles playing
;;
;;  * This was tested on Emacs 21.3, iTunes 4.2 and OSX 10.3
;;  * This meets my needs, but playlist selection could be added
;;    as well as fontification

;;; History:
;;  2003-03-22: jhg : written
;;  2004-04-11: jhg : revised and status window added.

(require 'osx-osascript)

;;
(defvar itunes-volume-step 5
  "*Amount to step the volume by.")

(defvar itunes-key [f6]
  "The prefix key for itunes access.
Call `itunes-key-setup' when changed to have a correct keymap.")
(defvar itunes-key-map nil
  "The key map for itunes.")

(defvar itunes-buff nil
  "The iTunes buffer.")
(defvar itunes-buff-name " *iTunes*"
  "The name of the itunes buffer.")
(defvar itunes-buff-window nil
  "The window displaying the iTunes buffer.")

(defvar itunes-info nil
  "The last info from itunes.")

;;; Code:

(defun itunes-do (&rest pgm)
  "Tell iTunes to run the osascript PGM."
  (osascript-run-str-elispify `("
set retval to false
tell application \"iTunes\"
" ,@pgm "
end tell
elispify(retval)
")))

;; FIXME: avoid two calls and put the update into this call.
;; (itunes-window-update)

(defun itunes-buff ()
  "Get or create the itunes buffer."
  (get-buffer-create itunes-buff-name))

;;; Simple itunes commands

(defun itunes-activate ()
  "Tell itunes to activate."
  (interactive)
  (itunes-do "activate"))
;; (itunes-activate)

(defun itunes-quit ()
  "Tell itunes to quit."
  (interactive)
  (itunes-do "quit"))
;; (itunes-quit)

(defun itunes-play ()
  "Tell itunes to start playing."
  (interactive)
  (itunes-do "play"))
;; (itunes-play)

(defun itunes-playpause ()
  "Tell itunes to toggle between play and pause."
  (interactive)
  (itunes-do "playpause"))
;; (itunes-playpause)

(defun itunes-next-track ()
  "Tell itunes to skip to the next track."
  (interactive)
  (itunes-do "next track")
  (itunes-window-update))
;; (itunes-next-track)

(defun itunes-prev-track ()
  "Tell itunes to skip to the previous track."
  (interactive)
  (itunes-do "back track")
  (itunes-window-update))
;; (itunes-next-track)

(defun itunes-play-url (url)
  "Tell itunes to play the URL."
  (interactive "sURL: ")
  (itunes-do (format "open location \"%s\"" url))
  (itunes-window-update))

;;; Volume

(defun itunes-volume-set (vol)
  "Tell itunes to set the volume to VOL."
  (interactive "NiTunes volume:")
  (itunes-do "set sound volume to " (format "%s" vol)))
;; (itunes-volume-set 30)

(defun itunes-volume-adjust (adjust)
  "Tell itunes to adjust the volume by ADJUST."
  (interactive "NiTunes volume adjust:")
  (itunes-do "set sound volume to { get sound volume } + "
             (format "%s" adjust)))
;; (itunes-volume-adjust -10)

(defun itunes-vol+ ()
  "Tell itunes to pump up the volume."
  (interactive)
  (itunes-volume-adjust (+ itunes-volume-step)))
(defun itunes-vol- ()
  "Tell itunes to chill out."
  (interactive)
  (itunes-volume-adjust (- itunes-volume-step)))
(defun itunes-mute-toggle ()
  "Tell itunes to toggle the mute."
  (interactive)
  (itunes-do "set mute to not (get mute)"))
;; (itunes-mute-toggle)

(defun itunes-get-info-sexp ()
  "Tell itunes to tell us about what it is playing.
This uses the elispify function."
  (let* ((osascript-keep-output t)
         (flist '(
                  "album" "artist" "bit rate" "bpm" "comment" "enabled"
                  "genre" "kind" "name" "played date" "rating" "size" "start"
                  "track number" "track count" "year"
                  "finish" "played count" "time"
                  ))
         (script nil)
         str)
    ;;
    (while flist
      (let ((fname (car flist)))
        (setq str (format "{\"%s\", %s of currtrack}%s"
                          fname fname (if script "," ""))))
      (setq script (cons str script))
      (setq flist (cdr flist)))
    (setq itunes-info
          (itunes-do
           "set currtrack to current track\n"
           "set retval to {" script "}"))
    itunes-info))

(defun itunes-now-playing ()
  "Insert a sig line of what is now playing."
  (interactive)
  (let ((info (itunes-get-info-sexp)))
    (when info
      (insert "\n-- \nNow playing: "
              (itunes-info-val "name"  ) " / "
              (itunes-info-val "album" ) " / "
              (itunes-info-val "artist") "\n"))))
;; (itunes-now-playing)
;; (global-set-key [f6] 'itunes-now-playing)

(defun itunes-window-show ()
  "Show the itunes window."
  (interactive)
  ;;(setq itunes-buff (itunes-buff))
  (let ((itunes-buff-window (itunes-buff)))
    (unless (windowp itunes-buff-window)
      ;; bottom window
      (select-window (frame-first-window))
      (other-window -1)
      ;; cut into two if big enough  (This is three lines)
      (when (< (* 2 window-min-height) (window-height))
        (split-window)
        (other-window 1))
      (set-window-buffer (selected-window) (itunes-buff))
      ;; make it tiny
      (enlarge-window (- window-min-height (window-height)))
      nil)
    itunes-buff-window))
;; (itunes-window-show)

(defun itunes-window-hide ()
  "Hide the itunes window."
  (delete-windows-on (itunes-buff)))
;; (itunes-window-hide)

(defun itunes-window-toggle ()
  "Toggle the visiblity of the itunes window."
  (interactive)
  (let ((thiswin (selected-window)))
    (if (get-buffer-window (itunes-buff))
      (itunes-window-hide)
      (progn
        (itunes-window-show)
        (sit-for 0)
        (itunes-window-update)))
    (select-window thiswin)))
;; (itunes-window-toggle)

(defun itunes-window-update ()
  "Update the itunes window if visible."
  (interactive)
  (if (get-buffer-window (itunes-buff))
    (itunes-window-paint-prep (itunes-get-info-sexp))))
;; (itunes-window-update)

;;;;;

(defun itunes-info-val (key &optional default info)
  "Assoc for `itunes-info' KEY optional DEFAULT INFO."
  (unless default
    (setq default ""))
  (unless info
    (setq info itunes-info))
  (let ((val (cadr (assoc key info))))
    ;; some unknowns are "0"s
    (if (and val (not (equal val 0)))
      val
      default)))

;; (itunes-info-val "track count" "??")

(defun itunes-window-paint-prep (&optional info)
  "An internal function to prepare INFO for painting.
You should modify `itunes-window-paint' instead."
  (unless info
    (setq info (itunes-get-info-sexp)))
  (let ((swin (selected-window)))
    ;; prepare the buffer
    (set-buffer (itunes-buff))
    (erase-buffer)
    ;; Call the user function
    (itunes-window-paint info)
    ;; Clean up the window
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (select-window swin)))

(defun itunes-window-paint (info)
  "Fill the buffer with itunes INFO.  The buffer is empty.
This is the function you want to customize."
  (insert
   (format
    "Song  : %-40s %5s  Rating : %-10s\n"
    (itunes-elide (itunes-info-val "name" 0) 40)
    (format "%s/%s"
            (itunes-info-val "track number" "??")
            (itunes-info-val "track count"  "??") )
    (itunes-fmt-rating (itunes-info-val "rating")) ))
  (insert
   (format
    "Album : %-40s %5s  Year   : %-10s\n"
    (itunes-elide (itunes-info-val "album") 40)
    (itunes-info-val "time")
    (let ((y (itunes-info-val "year")))
      (if (equal y 0) "" y)) ))
  (insert
   (format
    "Artist: %-46s  BitRate: %-10s"
    (itunes-elide (itunes-info-val "artist") 46)
    (itunes-info-val "bit rate") ))
  nil)
;; (itunes-window-paint-prep itunes-info)

(defun itunes-info-print-all (&optional info)
  "Print all the itunes INFO into the current buffer for debugging."
  (unless info
    (setq info itunes-info))
  (setq info (sort info (function (lambda (a b) (string< (car a) (car b))))))
  (while info
    (insert (format "%-20s : %s\n" (caar info) (cadar info)))
    (setq info (cdr info))))
;; (itunes-info-print-all)

(defun itunes-fmt-rating (rating)
  "Format the RATING as 0-5 stars."
  (if (numberp rating)
      (make-string (/ (max 0 (min 100 rating)) 20) ?*)
      ""))
;; (itunes-fmt-rating 100)

(defun itunes-elide (str len)
  "Elide the string STR to LEN if it is too long..."
  (unless (stringp str)
    (setq str (format "%s" str)))
  (let ((strlen (length str)))
    (if (<= strlen len)
      str ;; no trim
      (if (<= len 15)
        (substring str 0 len) ;; hard trunc
        (concat (substring str 0 (- len 3)) "...")))))
;; (itunes-elide "12345678901234567890" 17)

;;;;;

(defun itunes-key-setup (&optional key)
  "Setup the KEY map for telling itunes what to do."
  (unless key
    (setq key itunes-key))
  (setq itunes-key-map (make-sparse-keymap))
  (defalias 'itunes-key-prefix itunes-key-map)
  (global-set-key key 'itunes-key-prefix)
  (let ((map itunes-key-map))
    ;; press twice to show/hide
    (define-key map key     'itunes-window-toggle)
    (define-key map "g"     'itunes-window-update)
    ;; vol
    (define-key map "+"     'itunes-vol+)
    (define-key map "="     'itunes-vol+)
    (define-key map [up]    'itunes-vol+)
    (define-key map "_"     'itunes-vol-)
    (define-key map "-"     'itunes-vol-)
    (define-key map [down]  'itunes-vol-)
    (define-key map "m"     'itunes-mute-toggle)
    (define-key map " "     'itunes-playpause)
    (let ((v 0))
      (while (< v 10)
        (define-key map (format "%s" v)
          `(lambda () (interactive)
             (itunes-volume-set (* 10 ,v))))
        (setq v (1+ v))))
      ;;
    (define-key map [right] 'itunes-next-track)
    (define-key map [left]  'itunes-prev-track)
    ;;
    (define-key map "u"     'itunes-play-url)
    nil))

;; Create the keymap
(itunes-key-setup)

(provide 'osx-itunes)

;;; osx-itunes.el ends here

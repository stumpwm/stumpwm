;;; -*- Mode: Lisp -*-
;;; Written by Julian Stecklina, based on sample-stumpwmrc.lisp.

;;; This is a sample Wmii-like .stumpwmrc file using Super as modifier
;;; (which happens to be the Windows key on my keyboard). It doesn't
;;; cover the whole Wmii command set, but it will ease the transition
;;; to StumpWM for people coming from Wmii.
;;; 
;;; The "normal" StumpWM commands are still available with their
;;; default keybindings (see the manual) and you will probably need
;;; them. So go read the manual. :)

(in-package :stumpwm)

;;; A mode line showing all groups in its first and all windows in the
;;; current group in the second line.

(setq *screen-mode-line-format* (format nil "%g~%%W"))

;;; Wmii-like keybindings

(defvar *terminal* "xterm"
  "The command used to start a terminal. It should understand the -e
  parameter.")

;; Use focus-follows-mouse, like wmii does.
(setq *mouse-focus-policy* :sloppy)

;; Change the prefix key to something else. The default is C-t. Use
;; this to access stumpwm's original keybindings.
;(set-prefix-key (kbd "Menu"))

;;; If you like Meta (most probably alt on your keyboard) more than
;;; Super (which is the Windows key on mine), change 's-' into 'M-'.
(define-key *top-map* (kbd "s-RET") (format nil "exec ~A" *terminal*))
(define-key *top-map* (kbd "s-S-RET") "exec-in-terminal")
(define-key *top-map* (kbd "s-C") "delete")
(define-key *top-map* (kbd "s-p") "exec")
(define-key *top-map* (kbd "s-d") "vsplit")
(define-key *top-map* (kbd "s-D") "hsplit")
(define-key *top-map* (kbd "s-R") "remove")
(define-key *top-map* (kbd "s-SPC") "pull-hidden-next")

;;; s-DIGIT moves or creates to a numbered group.
(dotimes (i 9)
  (define-key *top-map* (kbd (format nil "s-~A" (1+ i)))
    (format nil "gselect-or-create ~A" (1+ i))))

;;; s-[hjkl] navigate through frames. If you press shift, it will move
;;; the current window in that direction.
(loop for (vi-key name) in '(("k" "up")
			     ("j" "down")
			     ("h" "left")
			     ("l" "right"))
     do (let ((key-combo (format nil "s-~A" vi-key))
	      (shifted-key-combo (format nil "s-~A" (string-upcase vi-key))))
	  (define-key *top-map* (kbd key-combo)
	    (format nil "move-focus ~A" name))
	  (define-key *top-map* (kbd shifted-key-combo)
	    (format nil "move-window ~A" name))))

(defcommand gselect-or-create (group-number) ((:number "Group number: "))
  (gselect
   (or (select-group (current-screen) (format nil "~A" group-number) )
       (let ((group (add-group (current-screen)
			       (format nil "unnamed~A" group-number))))
	 ;; number should be free, since select-group failed.
	 (setf (group-number group) group-number)
	 group))))

(defcommand exec-in-terminal (cmd) ((:string "Command: "))
  (run-shell-command (format nil "~A -e ~A" *terminal* cmd)))

;;; EOF

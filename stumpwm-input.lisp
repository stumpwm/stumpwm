;; Copyright (C) 2003 Shawn Betts
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; This file handles input stuff
;;
;; Code:
(in-package #:stumpwm)

;;; Utility key conversion functions

(defun keycode->character (code mods)
  (let ((idx (if (member :shift mods) 1 0)))
  (xlib:keysym->character *display* (xlib:keycode->keysym *display* code idx) 0)))

;;; line and key reading functions

(defun setup-input-window (screen)
  "Set the input window up to read input"
  (let* ((height (+ (xlib:font-descent (screen-font screen))
		    (xlib:font-ascent (screen-font screen))))
	 (screen-width (xlib:drawable-width (xlib:screen-root (screen-number screen))))
	 (win (screen-input-window screen)))
    (pprint '(setup input window))
    ;; Window dimensions
    (xlib:map-window win)
    (setf (xlib:window-priority win) :above)
    (setf (xlib:drawable-y win) 0
	  (xlib:drawable-height win) height
	  (xlib:drawable-x win) (- screen-width
				   100
				   (* (xlib:drawable-border-width win) 2)
				   (* *message-window-padding* 2))
	  (xlib:drawable-width win) (+ 100 (* *message-window-padding* 2)))
    ;; Ready to recieve input
    (xlib:grab-keyboard (screen-input-window screen) :owner-p nil
			:sync-keyboard-p nil :sync-pointer-p nil)))

(defun shutdown-input-window (screen)
  (pprint '(shutdown input window))
  (xlib:ungrab-keyboard *display*)
  (xlib:unmap-window (screen-input-window screen)))

(defun read-key-handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (labels ((key-press (&rest event-slots &key root code state &allow-other-keys)
		      (cons code state)))
    (case event-key
      (:key-release
       't)
      (:key-press
       (apply #'key-press event-slots))
      (t nil))))

(defun read-key (screen)
  "Return a dotted pair (code . state) key."
  (do ((ret nil (xlib:process-event *display* :handler #'read-key-handle-event :timeout nil)))
      ((consp ret) ret)))

(defun read-line (screen)
  "Read a line of input through rp and return it."
    (labels ((key-loop ()
		(let (input)
		  (do ((key (read-key screen) (read-key screen)))
		      (nil)
		    (multiple-value-bind (inp ret) (process-input screen input (car key) (cdr key))
		      (setf input inp)
		      (case ret
			('done
			 (return (values input 'done)))
			('abort
			 (return (values input 'abort)))))))))
      (setup-input-window screen)
      (multiple-value-bind (input ret) (key-loop)
	(shutdown-input-window screen)
	(unless (eq ret 'abort)
	  ;; Return the input bucket as a string
	  (concatenate 'string input)))))

(defun draw-input-bucket (screen input)
  "Draw to the screen's input window the contents of input."
  (let ((gcontext (create-message-window-gcontext screen))
	(win (screen-input-window screen))
	(width (max 100 (xlib:text-width (screen-font screen) input)))
	(screen-width (xlib:drawable-width (xlib:screen-root (screen-number screen)))))
    (xlib:clear-area win :x
		     (+ *message-window-padding*
			(xlib:text-width (screen-font screen) input)))
    (xlib:with-state (win)
		     (setf (xlib:drawable-x win) (- screen-width width
						    (* (xlib:drawable-border-width win) 2)
						    (* *message-window-padding* 2))
			   (xlib:drawable-width win) (+ width (* *message-window-padding* 2))))
    (xlib:draw-image-glyphs win gcontext
			    *message-window-padding*
			    (xlib:font-ascent (screen-font screen))
			    input)))

(defun process-input (screen input code state)
  "Process the key (code and state), given the current input
buffer. Returns a new modified input buffer."
  (labels ((process-key (inp code state)
	      "Call the appropriate function based on the key
pressed. Return 'done when the use has signalled the finish of his
input (pressing Return), nil otherwise."
	      (cond ((eq (xlib:keycode->keysym *display* code 0)
			 (xlib:keysym #\Return))
		     (values inp 'done))
		    ((eq (xlib:keycode->keysym *display* code 0)
			 (xlib:keysym #\Backspace))
		     (if (cdr inp)
			 (rplacd (last inp 2) '())
		       (setf inp nil))
		     (values inp nil))
		    ((and (eq (xlib:keycode->keysym *display* code 0)
			      (xlib:keysym #\g))
			  (member :control (xlib:make-state-keys state)))
		     (values inp 'abort))
		    (t (let* ((mods (xlib:make-state-keys state))
			      (ch (keycode->character code mods)))
			 (if (and (characterp ch) (char>= ch #\Space) (char<= ch #\~))
			     (setf inp (conc1 inp ch)))
			 (values inp nil))))))
    (multiple-value-bind (inp ret) (process-key input code state)
      (case ret
	('done
	  (values inp 'done))
	 ('abort
	  (values inp 'abort))
	 (t
	  (draw-input-bucket screen inp)
	  (values inp t))))))

;;;;; UNUSED

(defun update-modifier-map (screen)
  (let ((mods (xlib:modifier-mapping *display*)))
    (setf (modifiers-alt (screen-modifiers screen)) nil)
    (setf (modifiers-meta (screen-modifiers screen)) nil)
    (setf (modifiers-hyper (screen-modifiers screen)) nil)
    (setf (modifiers-super (screen-modifiers screen)) nil)
    (loop for mod in (:mod1 :mod2 :mod3 :mod4 :mod5)
	  for code in (cdddr mods)
	  do (let ((key (xlib:keycode->keysym *display* code 0)))
	       (cond ((or (eql key :left-meta) (eql key :right-meta))
		      (setf (modifiers-meta (screen-modifiers screen)) mod))
		     ((or (eql key :left-alt) (eql key :right-alt))
		      (setf (modifiers-alt (screen-modifiers screen)) mod))
		     ((or (eql key :left-super) (eql key :right-super))
		      (setf (modifiers-alt (screen-modifiers screen)) mod))
		     ((or (eql key :left-hyper) (eql key :right-hyper))
		      (setf (modifiers-alt (screen-modifiers screen)) mod)))))
    ;; If alt is defined but meta isn't set meta to alt and clear alt
    (when (and (modifiers-alt (screen-modifiers screen))
	       (null modifiers-meta (screen-modifiers screen)))
      (setf (modifiers-meta (screen-modifiers screen)) (modifiers-alt (screen-modifiers screen)))
      (setf (modifiers-alt (screen-modifiers screen)) nil))))
	
(defun x11mod->stumpmod (screen state)
  (let ((mod nil))
    (when (member state (modifiers-alt (screen-modifiers screen)))
      (push :alt mod))
    (when (member state (modifiers-meta (screen-modifiers screen)))
      (push :meta mod))
    (when (member state (modifiers-hyper (screen-modifiers screen)))
      (push :hyper mod))
    (when (member state (modifiers-super (screen-modifiers screen)))
      (push :super mod))
    (when (member state :control)
      (push :control mod))
    mod))

(defun mod->string (state)
  "Convert a stump modifier list to a string"
  (let ((alist '((:alt . "A-") (:meta . "M-") (:hyper . "H-") (:super . "S-"))))
    (apply #'concatenate 'string (mapcar (lambda (x) (cdr (assoc x alist))) state))))

(defun keycode->string (code state)
  (concatenate 'string (mod->string state)
	       (ext:keysym-preferred-name (xlib:keycode->keysym *display* code 0))))
  
(defun cook-keycode (code state)
  (values (xlib:keycode->keysym *display* code 0) (x11mod->stumpmod state)))

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

(defun read-key-handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (labels ((key-press (&rest event-slots &key root code state &allow-other-keys)
		      (let ((screen (find-screen root)))
			(pprint '(key press handle))
			(read-input screen code state))))
    (case event-key
      (:key-release
       t)
      (:key-press
       (apply #'key-press event-slots))
      (t nil))))

(defun read-line (screen)
  "Read a line of input through rp and return it"
  (pprint '(setup input window))
  (setup-input-window screen)
  ;; Read all the keys
  (pprint '(read keystrokes))
  (do ((ret nil (xlib:process-event *display* :handler #'read-key-handle-event :timeout nil)))
      ((eq ret 'done))
    (pprint ret))
  (pprint '(shutdown input window))
  (shutdown-input-window screen)
  ;; Return the input bucket as a string
  (concatenate 'string (screen-input-bucket screen)))

(defun setup-input-window (screen)
  "Set the input window up to read input"
  (let* ((height (+ (xlib:font-descent (screen-font screen))
		    (xlib:font-ascent (screen-font screen))))
	 (screen-width (xlib:drawable-width (xlib:screen-root (screen-number screen))))
	 (win (screen-input-window screen)))
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
    ;; empty the bucket
    (setf (screen-input-bucket screen) '())
    ;; Ready to recieve input
    (xlib:grab-keyboard (screen-input-window screen) :owner-p nil
			:sync-keyboard-p nil :sync-pointer-p nil)))

(defun shutdown-input-window (screen)
  (xlib:ungrab-keyboard *display*)
  (xlib:unmap-window (screen-input-window screen)))

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

(defun keycode->character (code mods)
  (let ((idx (if (member :shift mods) 1 0)))
  (xlib:keysym->character *display* (xlib:keycode->keysym *display* code idx) 0)))

(defun handle-key (screen code state)
  "Call the appropriate function based on the key pressed. Return
'done when the use has signalled the finish of his input (pressing
Return), nil otherwise."
  (if (eq (xlib:keycode->keysym *display* code 0)
	  (xlib:keysym #\Return))
      'done
    (let* ((mods (xlib:make-state-keys state))
	   (ch (keycode->character code mods)))
      (print "state:")
      (print mods)
      (if (and (characterp ch) (char>= ch #\Space) (char<= ch #\~))
	  (setf (screen-input-bucket screen)
		(conc1 (screen-input-bucket screen) ch)))
      nil)))
    

(defun read-input (screen code state)
  "Read a key into the screen's input bucket."
  (print "read-input")
  (if (eq 'done (handle-key screen code state))
      ;; Return 'done
      'done
    (let ((gcontext (create-message-window-gcontext screen))
	  (key (xlib:keysym->character *display* (xlib:keycode->keysym *display* code 0) state)))
      (xlib:clear-area (screen-input-window screen))
      (print (screen-input-bucket screen))
      (xlib:draw-image-glyphs (screen-input-window screen) gcontext
			      *message-window-padding*
			      (xlib:font-ascent (screen-font screen))
			      (screen-input-bucket screen))
    ;; Return true
      t)))

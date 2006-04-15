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
(in-package :stumpwm)

(defstruct input-line
  string position history history-bk)

(defvar *input-map* 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd (string #\Backspace)) 'input-delete-backward-char)
    (define-key map (kbd "C-d") 'input-delete-forward-char)
    (define-key map (kbd "C-f") 'input-forward-char)
    (define-key map (kbd "C-b") 'input-backward-char)
    (define-key map (kbd "C-a") 'input-move-beginning-of-line)
    (define-key map (kbd "C-e") 'input-move-end-of-line)
    (define-key map (kbd "C-k") 'input-kill-line)
    (define-key map (kbd "C-u") 'input-kill-to-beginning)
    (define-key map (kbd "C-p") 'input-history-back)
    (define-key map (kbd "C-n") 'input-history-forward)
    (define-key map (kbd (string #\Return)) 'input-submit)
    (define-key map (kbd "C-g") 'input-abort)
    (define-key map t 'input-self-insert)
    map))

(defvar *input-history* nil
  "History for the input line.")

;;; Utility key conversion functions

(defun keycode->character (code mods)
  (let ((idx (if (member :shift mods) 1 0)))
    (xlib:keysym->character *display* (xlib:keycode->keysym *display* code idx) 0)))

;;; line and key reading functions

(defun setup-input-window (screen prompt input)
  "Set the input window up to read input"
  (let* ((height (+ (xlib:font-descent (screen-font screen))
		    (xlib:font-ascent (screen-font screen))))
	 (win (screen-input-window screen)))
    (dformat "Setup input window~%")
    ;; Window dimensions
    (xlib:map-window win)
    (setf (xlib:window-priority win) :above
	  (xlib:drawable-height win) height)
    ;; Draw the prompt
    (draw-input-bucket screen prompt input)
    ;; Ready to recieve input
    (xlib:grab-keyboard (screen-input-window screen) :owner-p nil
			:sync-keyboard-p nil :sync-pointer-p nil)))

(defun shutdown-input-window (screen)
  (dformat "Shutdown input window~%")
  (xlib:ungrab-keyboard *display*)
  (xlib:unmap-window (screen-input-window screen)))

(defun read-key-handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  (labels ((key-press (&rest event-slots &key root code state &allow-other-keys)
		      (declare (ignore event-slots root))
		      ;; FIXME: don't use a cons
		      (cons code state)))
    (case event-key
      (:key-release
       't)
      (:key-press
       (apply #'key-press event-slots))
      (t nil))))

(defun read-key ()
  "Return a dotted pair (code . state) key."
  (do ((ret nil (xlib:process-event *display* :handler #'read-key-handle-event :timeout nil)))
      ((consp ret) ret)))

(defun make-input-string (initial-input)
  (make-array (length initial-input) :element-type 'character :initial-contents initial-input
	      :adjustable t :fill-pointer t))
  
(defun read-one-line (screen prompt &optional (initial-input ""))
  "Read a line of input through stumpwm and return it. returns nil if the user aborted."
  (let ((input (make-input-line :string (make-input-string initial-input)
				:position (length initial-input)
				:history -1)))
    (labels ((key-loop ()
	       (loop for key = (read-key) then (read-key)
		  when (not (is-modifier (xlib:keycode->keysym *display* (car key) 0)))
		  when (process-input screen prompt input (car key) (cdr key))
		  return (input-line-string input))))
      (setup-input-window screen prompt input)
      (catch 'abort
	(unwind-protect
	     (key-loop)
	  (shutdown-input-window screen))))))

(defun read-one-char (screen)
  "Read a single character."
  (grab-keyboard screen)
  (prog1
      (let ((k (do ((k (read-key) (read-key)))
		   ((not (is-modifier (xlib:keycode->keysym *display* (car k) 0))) k))))
	(keycode->character (car k) (xlib:make-state-keys (cdr k))))
    (ungrab-keyboard)))

(defun draw-input-bucket (screen prompt input &optional errorp)
  "Draw to the screen's input window the contents of input."
  (let* ((gcontext (create-message-window-gcontext screen))
	 (win (screen-input-window screen))
	 (prompt-width (xlib:text-width (screen-font screen) prompt))
	 (string (input-line-string input))
	 (pos (input-line-position input))
	 (width (+ prompt-width
		   (max 100 (xlib:text-width (screen-font screen) string)))))
    (xlib:clear-area win :x (+ *message-window-padding*
			       prompt-width
			       (xlib:text-width (screen-font screen) string)))
    (xlib:with-state (win)
		     (setf (xlib:drawable-width win) (+ width (* *message-window-padding* 2)))
		     (setup-win-gravity screen win *input-window-gravity*))
    (xlib:draw-image-glyphs win gcontext
			    *message-window-padding*
			    (xlib:font-ascent (screen-font screen))
			    prompt)
    (xlib:draw-image-glyphs win gcontext
			    (+ *message-window-padding* prompt-width)
			    (xlib:font-ascent (screen-font screen))
			    string)
    ;; draw a block cursor
    (invert-rect screen win 
		 (+ *message-window-padding*
		    prompt-width
		    (xlib:text-width (screen-font screen) (subseq string 0 pos)))
		 0
		 (xlib:text-width (screen-font screen) (if (>= pos (length string))
							   " "
							   (string (char string pos))))
		 (+ (xlib:font-descent (screen-font screen))
		    (xlib:font-ascent (screen-font screen))))
    ;; draw the error 
    (when errorp
      (invert-rect screen win 0 0 (xlib:drawable-width win) (xlib:drawable-height win))
      (xlib:display-force-output *display*)
      ;; FIXME: there's no usleep
      (loop with time = (get-internal-run-time)
	    until (> (- (get-internal-run-time) time) 50))
      (invert-rect screen win 0 0 (xlib:drawable-width win) (xlib:drawable-height win)))))

(defun code-state->key (code state)
  (let* ((mods (xlib:make-state-keys state))
	 (sym (xlib:keycode->keysym *display* code 0))
	 (upcase-sym (xlib:keycode->keysym *display* code 1))
	 ;; make sure there is such a keysym
	 (char (or (and sym upcase-sym
			(xlib:keysym->character *display* (if (find :shift mods) upcase-sym sym)))
		   ;; some keysyms aren't mapped to characters (why not?)
		   ;; so use this in that case.
		   #\Null)))
    (when char
      (make-key :char (char-code char) :control (and (find :control mods) t) :shift (and (find :shift mods)
											 (eql sym upcase-sym))))))

(defun input-delete-backward-char (input key)
  (declare (ignore key))
  (let ((pos (input-line-position input)))
    (cond ((or (<= (length (input-line-string input)) 0)
	       (<= pos 0))
	   :error)
	  (t
	   (replace (input-line-string input) (input-line-string input)
		    :start2 pos :start1 (1- pos))
	   (decf (fill-pointer (input-line-string input)))
	   (decf (input-line-position input))))))

(defun input-delete-forward-char (input key)
  (declare (ignore key))
  (let ((pos (input-line-position input)))
    (cond ((>= pos
	       (length (input-line-string input)))
	   :error)
	  (t
	   (replace (input-line-string input) (input-line-string input)
		    :start1 pos :start2 (1+ pos))
	   (decf (fill-pointer (input-line-string input)))))))

(defun input-forward-char (input key)
  (declare (ignore key))
  (incf (input-line-position input))
  (when (> (input-line-position input)
	   (length (input-line-string input)))
    (setf (input-line-position input) (length (input-line-string input)))))

(defun input-backward-char (input key)
  (declare (ignore key))
  (decf (input-line-position input))
  (when (< (input-line-position input) 0)
    (setf (input-line-position input) 0)))

(defun input-move-beginning-of-line (input key)
  (declare (ignore key))
  (setf (input-line-position input) 0))

(defun input-move-end-of-line (input key)
  (declare (ignore key))
  (setf (input-line-position input) (length (input-line-string input))))

(defun input-kill-line (input key)
  (declare (ignore key))
  (setf (fill-pointer (input-line-string input)) (input-line-position input)))

(defun input-kill-to-beginning (input key)
  (declare (ignore key))
  (replace (input-line-string input) (input-line-string input)
	   :start2 (input-line-position input) :start1 0)
  (decf (fill-pointer (input-line-string input)) (input-line-position input))
  (setf (input-line-position input) 0))

(defun input-history-back (input key)
  (declare (ignore key))
  (when (= (input-line-history input) -1)
    (setf (input-line-history-bk input) (input-line-string input)))
  (incf (input-line-history input))
  (if (>= (input-line-history input)
	  (length *input-history*))
      (progn
	(decf (input-line-history input))
	:error)
    (setf (input-line-string input) (make-input-string (elt *input-history* (input-line-history input)))
	  (input-line-position input) (length (input-line-string input)))))

(defun input-history-forward (input key)
  (declare (ignore key))
  (decf (input-line-history input))
  (cond ((< (input-line-history input) -1)
	 (incf (input-line-history input))
	 :error)
	((= (input-line-history input) -1)
	 (setf (input-line-string input) (input-line-history-bk input)
	       (input-line-position input) (length (input-line-string input))))
	(t
	 (setf (input-line-string input) (make-input-string (elt *input-history* (input-line-history input)))
	       (input-line-position input) (length (input-line-string input))))))

(defun input-submit (input key)
  (declare (ignore input key))
  :done)

(defun input-abort (input key)
  (declare (ignore input key))
  (throw 'abort nil))

(defun input-self-insert (input key)
  (if (key-mods-p key)
      :error
    (progn
      (vector-push-extend #\_ (input-line-string input))
      (replace (input-line-string input) (input-line-string input)
	       :start2 (input-line-position input) :start1 (1+ (input-line-position input)))
      (setf (char (input-line-string input) (input-line-position input)) (code-char (key-char key)))
      (incf (input-line-position input)))))

(defun process-input (screen prompt input code state)
  "Process the key (code and state), given the current input
buffer. Returns a new modified input buffer."
  (labels ((process-key (code state)
			"Call the appropriate function based on the key
pressed. Return 'done when the use has signalled the finish of his
input (pressing Return), nil otherwise."
			(let* ((key (code-state->key code state))
			       (command (and key (lookup-key *input-map* key t))))
			  (if command
			      (funcall command input key)
			    :error))))
    (case (process-key code state)
      (:done 
       (push (input-line-string input) *input-history*)
       :done)
      (:abort
       (throw :abort t))
      (:error
       ;; FIXME draw inverted text
       (draw-input-bucket screen prompt input t)
       nil)
      (t
       (draw-input-bucket screen prompt input)
       nil))))

;;;;; UNUSED

(defun update-modifier-map (screen)
  (let ((mods (xlib:modifier-mapping *display*)))
    (setf (modifiers-alt (screen-modifiers screen)) nil)
    (setf (modifiers-meta (screen-modifiers screen)) nil)
    (setf (modifiers-hyper (screen-modifiers screen)) nil)
    (setf (modifiers-super (screen-modifiers screen)) nil)
    (loop for mod in '(:mod1 :mod2 :mod3 :mod4 :mod5)
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
	       (null (modifiers-meta (screen-modifiers screen))))
      (setf (modifiers-meta (screen-modifiers screen)) (modifiers-alt (screen-modifiers screen)))
      (setf (modifiers-alt (screen-modifiers screen)) nil))))
	
;; (defun x11mod->stumpmod (screen state)
;;   (let ((mod nil))
;;     (when (member state (modifiers-alt (screen-modifiers screen)))
;;       (push :alt mod))
;;     (when (member state (modifiers-meta (screen-modifiers screen)))
;;       (push :meta mod))
;;     (when (member state (modifiers-hyper (screen-modifiers screen)))
;;       (push :hyper mod))
;;     (when (member state (modifiers-super (screen-modifiers screen)))
;;       (push :super mod))
;;     (when (member state :control)
;;       (push :control mod))
;;     mod))

(defun mod->string (state)
  "Convert a stump modifier list to a string"
  (let ((alist '((:alt . "A-") (:meta . "M-") (:hyper . "H-") (:super . "S-"))))
    (apply #'concatenate 'string (mapcar (lambda (x) (cdr (assoc x alist))) state))))

;; (defun keycode->string (code state)
;;   (concatenate 'string (mod->string state)
;; 	       (string (keysym->character *display*
;; 					  (xlib:keycode->keysym *display* code 0)
;; 					  state))))
  
;; (defun cook-keycode (code state)
;;   (values (xlib:keycode->keysym *display* code 0) (x11mod->stumpmod state)))

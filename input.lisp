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

(defvar *input-map* nil
  "Input window bindings")

(when (null *input-map*)
  (setf *input-map*
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "DEL") 'input-delete-backward-char)
	  (define-key map (kbd "C-d") 'input-delete-forward-char)
	  (define-key map (kbd "Delete") 'input-delete-forward-char)
	  (define-key map (kbd "C-f") 'input-forward-char)
	  (define-key map (kbd "Right") 'input-forward-char)
	  (define-key map (kbd "M-f") 'input-forward-word)
	  (define-key map (kbd "C-b") 'input-backward-char)
	  (define-key map (kbd "Left") 'input-backward-char)
	  (define-key map (kbd "M-b") 'input-backward-word)
	  (define-key map (kbd "C-a") 'input-move-beginning-of-line)
	  (define-key map (kbd "C-e") 'input-move-end-of-line)
	  (define-key map (kbd "C-k") 'input-kill-line)
	  (define-key map (kbd "C-u") 'input-kill-to-beginning)
	  (define-key map (kbd "C-p") 'input-history-back)
	  (define-key map (kbd "Up") 'input-history-back)
	  (define-key map (kbd "C-n") 'input-history-forward)
	  (define-key map (kbd "Down") 'input-history-forward)
	  (define-key map (kbd "RET") 'input-submit)
	  (define-key map (kbd "C-g") 'input-abort)
	  (define-key map (kbd "C-y") 'input-yank-selection)
	  (define-key map t 'input-self-insert)
	  map)))

(defvar *input-history* nil
  "History for the input line.")

;;; keysym functions

(defun is-modifier (keysym)
  "Return t if keycode is a modifier"
  ;; FIXME: This should depend on all the codes returned by ALL-MODIFIER-CODES
  (let ((mods '("Mode_switch"
		"Shift_L" "Shift_R"
		"Control_L" "Control_R"
		"Caps_Lock" "Shift_Lock" 
		"Meta_L" "Meta_R"
		"Alt_L" "Alt_R"
		"Super_L" "Super_R"
		"Hyper_L" "Hyper_R")))
    (member keysym (mapcar #'keysym-name->keysym mods))))

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

(defun input-handle-key-press-event (&rest event-slots &key root code state &allow-other-keys)
  (declare (ignore event-slots root))
  ;; FIXME: don't use a cons
  (cons code state))

(defun input-handle-selection-event (&rest event-slots &key window selection property &allow-other-keys)
  ;; FIXME: don't use a cons
  (if property
      (xlib:get-property window property :type :string :result-type 'string :transform #'xlib:card8->char :delete-p t)
      ""))

(defun read-key-handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
    (case event-key
      (:key-release
       't)
      (:key-press
       (apply 'input-handle-key-press-event event-slots))
      (t nil)))

(defun read-key-or-selection-handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
    (case event-key
      (:key-release
       't)
      (:key-press
       (apply 'input-handle-key-press-event event-slots))
      (:selection-notify
       (apply 'input-handle-selection-event event-slots))
      (t nil)))

(defun read-key ()
  "Return a dotted pair (code . state) key."
  (do ((ret nil (xlib:process-event *display* :handler #'read-key-handle-event :timeout nil)))
      ((consp ret) ret)))

(defun read-key-or-selection ()
  (do ((ret nil (xlib:process-event *display* :handler #'read-key-or-selection-handle-event :timeout nil)))
      ((or (stringp ret)
	   (consp ret))
       ret)))

(defun make-input-string (initial-input)
  (make-array (length initial-input) :element-type 'character :initial-contents initial-input
	      :adjustable t :fill-pointer t))
  
(defun read-one-line (screen prompt &optional (initial-input ""))
  "Read a line of input through stumpwm and return it. returns nil if the user aborted."
  (let ((input (make-input-line :string (make-input-string initial-input)
				:position (length initial-input)
				:history -1)))
    (labels ((key-loop ()
	       (loop for key = (read-key-or-selection) do
		  (cond ((stringp key)
			 ;; handle selection
			 (input-insert-string input key)
			 (draw-input-bucket screen prompt input))
			;; skip modifiers
			((is-modifier (xlib:keycode->keysym *display* (car key) 0)))
			((process-input screen prompt input (car key) (cdr key))
			 (return (input-line-string input)))))))
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
  (let* ((mods    (xlib:make-state-keys state))
	 (sym     (xlib:keycode->keysym *display* code 0))
	 (upsym   (xlib:keycode->keysym *display* code 1))
	 (shift-p (and (find :shift mods) t)))
    ;; If a keysym has a shift modifier, then use the uppercase keysym
    ;; and remove remove the shift modifier.
    (make-key :keysym (if (and shift-p (not (eql sym upsym)))
			  upsym
			  sym)
	      :control (and (find :control mods) t)
	      :shift (and shift-p (eql sym upsym))
              :meta (and (intersection mods (modifiers-meta *modifiers*)) t)
              :alt (and (intersection mods (modifiers-alt *modifiers*)) t)
              :hyper (and (intersection mods (modifiers-hyper *modifiers*)) t)
              :super (and (intersection mods (modifiers-super *modifiers*)) t))))

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

(defun input-forward-word (input key)
  (declare (ignore key))
  (let* ((p1 (position-if 'alphanumericp (input-line-string input) :start (input-line-position input)))
	 (p2 (and p1 (position-if-not 'alphanumericp (input-line-string input) :start p1))))
    (setf (input-line-position input) (or p2 (length (input-line-string input))))))

(defun input-backward-word (input key)
  (declare (ignore key))
  (let* ((p1 (position-if 'alphanumericp (input-line-string input) :end (input-line-position input) :from-end t))
	 (p2 (and p1 (position-if-not 'alphanumericp (input-line-string input) :end p1 :from-end t))))
    (setf (input-line-position input) (or (and p2 (1+ p2))
					  0))))

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
  (unless (= (input-line-position input) (length (input-line-string input)))
    (set-x-selection (subseq (input-line-string input) (input-line-position input))))
  (setf (fill-pointer (input-line-string input)) (input-line-position input)))

(defun input-kill-to-beginning (input key)
  (declare (ignore key))
  (unless (= (input-line-position input) 0)
    (set-x-selection (subseq (input-line-string input) 0 (input-line-position input))))
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

(defun input-insert-string (input string)
  (check-type string string)
  ;; FIXME: obviously this is substandard
  (map nil (lambda (c) (input-insert-char input c))
       string))

(defun input-insert-char (input char)
  (vector-push-extend #\_ (input-line-string input))
  (replace (input-line-string input) (input-line-string input)
	   :start2 (input-line-position input) :start1 (1+ (input-line-position input)))
  (setf (char (input-line-string input) (input-line-position input)) char)
  (incf (input-line-position input)))

(defun input-self-insert (input key)
  (let ((char (xlib:keysym->character *display* (key-keysym key))))
    (if (or (key-mods-p key) (null char))
	:error
	(input-insert-char input char))))

(defun input-yank-selection (input key)
  (declare (ignore key))
  ;; if we own the selection then just insert it.
  (if *x-selection*
      (input-insert-string input *x-selection*)
      (xlib:convert-selection :primary :string (screen-input-window (current-screen)) :stumpwm-selection)))

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

(defun all-modifier-codes ()
  (multiple-value-bind
        (shift-codes lock-codes control-codes mod1-codes mod2-codes mod3-codes mod4-codes mod5-codes)
      (xlib:modifier-mapping *display*)
    (append shift-codes
            lock-codes
            control-codes
            mod1-codes
            mod2-codes
            mod3-codes
            mod4-codes
            mod5-codes)))

(defun get-modifier-map ()
  (labels ((find-mod (mod codes)
             (find (xlib:keysym->keycodes *display* (keysym-name->keysym mod)) codes)))
    (let ((modifiers (make-modifiers)))
      (multiple-value-bind 
            (shift-codes lock-codes control-codes mod1-codes mod2-codes mod3-codes mod4-codes mod5-codes)
          (xlib:modifier-mapping *display*)
        (declare (ignore shift-codes lock-codes control-codes))
        (loop for mod in '(:mod-1 :mod-2 :mod-3 :mod-4 :mod-5)
           for codes in (list mod1-codes mod2-codes mod3-codes mod4-codes mod5-codes)
           do 
           (cond ((or (find-mod "Meta_L" codes)
                      (find-mod "Meta_R" codes))
                  (push mod (modifiers-meta modifiers)))
                 ((or (find-mod "Alt_L" codes)
                      (find-mod "Alt_R" codes))
                  (push mod (modifiers-alt modifiers)))
                 ((or (find-mod "Super_L" codes)
                      (find-mod "Super_R" codes))
                  (push mod (modifiers-super modifiers)))
                 ((or (find-mod "Hyper_L" codes)
                      (find-mod "Hyper_R" codes))
                  (push mod (modifiers-hyper modifiers)))))
        ;; If alt is defined but meta isn't set meta to alt and clear alt
        (when (and (modifiers-alt modifiers)
                   (null (modifiers-meta modifiers)))
          (setf (modifiers-meta modifiers) (modifiers-alt modifiers)
                (modifiers-alt modifiers) nil))
        modifiers))))

(defun update-modifier-map ()
  (setf *modifiers* (get-modifier-map)
        *all-modifiers* (all-modifier-codes)))
	
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

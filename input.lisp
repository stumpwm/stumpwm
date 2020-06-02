;; Copyright (C) 2003-2008 Shawn Betts
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
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; This file handles input stuff
;;
;; Code:
(in-package :stumpwm)

(export '(*input-history-ignore-duplicates*
          *input-completion-style*
          *input-map*
          *numpad-map*
          completing-read
          input-delete-region
          input-goto-char
          input-insert-char
          input-insert-string
          input-point
          input-substring
          input-validate-region
          read-one-char
          read-one-line))


;;; General Utilities

(defun take (n list)
  "Returns a list with the first n elements of the given list, and the
remaining tail of the list as a second value."
  (loop for l on list
        repeat n
        collect (car l) into result
        finally (return (values result l))))

;; This could use a much more efficient algorithm.
;; But for our purposes with small lists it's likely ok.
(defun longest-common-prefix (seqs &key (test #'eql))
  "Returns the length of the longest common prefix of the sequences."
  (flet ((longest-common-prefix-2 (seq1 seq2)
           (if-let ((i (mismatch seq1 seq2 :test test)))
             i
             (length seq1))))
    (apply #'min (map-product #'longest-common-prefix-2 seqs seqs))))


(defstruct input-line
  string position history history-bk password)


;;; completion styles

(defgeneric input-completion-reset (completion-style completions)
  (:documentation "A completion style should implement this function
and reset its state when called."))

(defgeneric input-completion-complete (completion-style input direction)
  (:documentation "A completion style should implement this function
and complete the input by mutating it."))

(defclass input-completion-style-cyclic ()
  ((completions :initform nil :type list)
   (idx :initform 0 :type fixnum)))

(defmethod input-completion-reset ((cs input-completion-style-cyclic) completions)
  (setf (slot-value cs 'completions) completions
        (slot-value cs 'idx) 0))

(defmethod input-completion-complete ((cs input-completion-style-cyclic) input direction)
  (with-slots (completions idx) cs
    (let ((completion-count (length completions)))
      (if completions
          (progn
            (let ((elt (nth idx completions)))
              (input-delete-region input 0 (input-point input))
              (input-insert-string input (if (listp elt) (first elt) elt))
              (input-insert-char input #\Space))
            ;; Prepare the next completion
            (setf idx (mod (+ idx (if (eq direction :forward) 1 -1)) completion-count))
          :error)))))

(defun make-input-completion-style-cyclic ()
  (make-instance 'input-completion-style-cyclic))

(defclass input-completion-style-unambiguous ()
  ((display-limit :initarg :display-limit :initform 64 :type fixnum)
   (completions :initform nil :type list)))

(defmethod input-completion-reset ((cs input-completion-style-unambiguous) completions)
  (setf (slot-value cs 'completions)
        (take (slot-value cs 'display-limit) completions)))

(defmethod input-completion-complete ((cs input-completion-style-unambiguous) input direction)
  (declare (ignore direction))
  (with-slots (completions) cs
    (if (null completions)
        :error
        (let ((n (longest-common-prefix completions)))
          (input-delete-region input 0 (input-point input))
          (input-insert-string input (subseq (first completions) 0 n))
          (if (null (rest completions))
              (unmap-message-window (current-screen))
              (echo-string-list (current-screen) completions))))))

(defun make-input-completion-style-unambiguous (&key (display-limit 64))
  (make-instance 'input-completion-style-unambiguous
                 :display-limit display-limit))

(defvar *input-map*
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") 'input-delete-backward-char)
    (define-key map (kbd "M-DEL") 'input-backward-kill-word)
    (define-key map (kbd "C-d") 'input-delete-forward-char)
    (define-key map (kbd "M-d") 'input-forward-kill-word)
    (define-key map (kbd "Delete") 'input-delete-forward-char)
    (define-key map (kbd "C-f") 'input-forward-char)
    (define-key map (kbd "Right") 'input-forward-char)
    (define-key map (kbd "M-f") 'input-forward-word)
    (define-key map (kbd "C-b") 'input-backward-char)
    (define-key map (kbd "Left") 'input-backward-char)
    (define-key map (kbd "M-b") 'input-backward-word)
    (define-key map (kbd "C-a") 'input-move-beginning-of-line)
    (define-key map (kbd "Home") 'input-move-beginning-of-line)
    (define-key map (kbd "C-e") 'input-move-end-of-line)
    (define-key map (kbd "End") 'input-move-end-of-line)
    (define-key map (kbd "C-k") 'input-kill-line)
    (define-key map (kbd "C-u") 'input-kill-to-beginning)
    (define-key map (kbd "C-p") 'input-history-back)
    (define-key map (kbd "Up") 'input-history-back)
    (define-key map (kbd "C-n") 'input-history-forward)
    (define-key map (kbd "Down") 'input-history-forward)
    (define-key map (kbd "RET") 'input-submit)
    (define-key map (kbd "C-g") 'input-abort)
    (define-key map (kbd "ESC") 'input-abort)
    (define-key map (kbd "C-y") 'input-yank-selection)
    (define-key map (kbd "C-Y") 'input-yank-clipboard)
    (define-key map (kbd "TAB") 'input-complete-forward)
    (define-key map (kbd "ISO_Left_Tab") 'input-complete-backward)
    (define-key map t 'input-self-insert)
    map)
  "This is the keymap containing all input editing key bindings.")


(defvar *input-history* nil
  "History for the input line.")

(defvar *input-shell-history* nil
  "History for shell lines.")

(defvar *input-last-command* nil
  "The last input command.")

(defvar *input-completions* nil
  "The list of completions")

(defvar *input-completion-style* (make-input-completion-style-cyclic)
  "The completion style to use.
A completion style has to implement input-completion-reset
and input-completion-complete.
Available completion styles include
@table @asis
@item make-input-completion-style-cyclic
@item make-input-completion-style-unambiguous
@end table")

(defvar *input-history-ignore-duplicates* nil
  "Do not add a command to the input history if it's already the first in the list.")
(defvar *numpad-map* '((87 10 . 16) (88  11 . 16) (89 12 . 16) (106 61 . 16)
                       (83 13 . 16) (84  14 . 16) (85 15 . 16) (86  21 . 17)
                       (79 16 . 16) (80  17 . 16) (81 18 . 16) (63  17 . 17)
                       (82 20 . 16) (104 36 . 16) (91 60 . 16) (90  19 . 16))
  "A keycode to keycode map to re-wire numpads when the numlock key is active")

;;; keysym functions

(defun is-modifier (keycode)
  "Return t if keycode is a modifier"
  (or (find keycode *all-modifiers* :test 'eql)
      ;; Treat No Symbol keys as modifiers (and therefore ignorable)
      (= (xlib:keycode->keysym *display* keycode 0) 0)))

(defun keycode->character (code mods)
  (let ((idx (if (member :shift mods) 1 0)))
    (xlib:keysym->character *display* (xlib:keycode->keysym *display* code idx) 0)))

;;; line and key reading functions

(defun setup-input-window (screen prompt input)
  "Set the input window up to read input"
  (let* ((height (+ (font-height (screen-font screen))
                    (* *message-window-y-padding* 2)))
         (win (screen-input-window screen)))
    ;; Window dimensions
    (xlib:with-state (win)
      (setf (xlib:window-priority win) :above
            (xlib:drawable-height win) height))
    (xlib:map-window win)
    (draw-input-bucket screen prompt input)))

(defun shutdown-input-window (screen)
  (xlib:unmap-window (screen-input-window screen)))

;; Hack to avoid clobbering input from numpads with numlock on.
(defun input-handle-key-press-event (&rest event-slots
                                     &key event-key code state
                                       &allow-other-keys)
  (declare (ignore event-slots))
  (let ((numlock-on-p (= 2 (logand 2 (nth-value 4 (xlib:keyboard-control *display*)))))
         (numpad-key (assoc code *numpad-map*)))
    (when (and numlock-on-p numpad-key)
      (setf code (first (rest numpad-key))
            state (rest (rest numpad-key))))
    (list* event-key code state)))

(defun input-handle-selection-event (&key window selection property &allow-other-keys)
  (declare (ignore selection))
  (if property
      (xlib:get-property window property :type :string :result-type 'string :transform #'xlib:card8->char :delete-p t)
      ""))

(defun input-handle-click-event (&key root-x root-y &allow-other-keys)
  (list :button-press root-x root-y))

(defun read-key-handle-event (&rest event-slots &key event-key &allow-other-keys)
  (case event-key
    ((or :key-release :key-press)
     (apply 'input-handle-key-press-event event-slots))
    (t nil)))

(defun read-key-or-selection-handle-event (&rest event-slots &key event-key &allow-other-keys)
  (case event-key
    ((or :key-release :key-press)
     (apply 'input-handle-key-press-event event-slots))
    (:selection-notify
     (apply 'input-handle-selection-event event-slots))
    (t nil)))

(defun read-key-or-click-handle-event (&rest event-slots &key event-key &allow-other-keys)
  (case event-key
    ((or :key-release :key-press)
     (apply 'input-handle-key-press-event event-slots))
    (:button-press
     (apply 'input-handle-click-event event-slots))
    (t nil)))

(defun read-key ()
  "Return a dotted pair (code . state) key."
  (loop for ev = (xlib:process-event *display* :handler #'read-key-handle-event :timeout nil) do
       (when (and (consp ev)
                  (eq (first ev) :key-press))
           (return (rest ev)))))

(defun read-key-or-click ()
  (loop for ev = (xlib:process-event *display* :handler #'read-key-or-click-handle-event :timeout nil)
     do
       (when (consp ev)
         (when (eq (first ev) :key-press)
           (return (values nil (rest ev) nil nil)))
         (when (eq (first ev) :button-press)
           (return (values t nil (second ev) (third ev)))))))

(defun read-key-no-modifiers ()
  "Like read-key but never returns a modifier key."
  (loop for k = (read-key)
       while (is-modifier (car k))
       finally (return k)))

(defun read-key-no-modifiers-or-click ()
  (loop
     (multiple-value-bind (has-click k x y)
         (read-key-or-click)
       (if has-click
           (return (values t nil x y))
           (unless (is-modifier (car k))
             (return (values nil k nil nil)))))))

(defun read-key-or-selection ()
  (loop for ev = (xlib:process-event *display* :handler #'read-key-or-selection-handle-event :timeout nil) do
       (cond ((stringp ev)
              (return ev))
             ((and (consp ev)
                   (eq (first ev) :key-press))
              (return (rest ev))))))

(defun make-input-string (initial-input)
  (make-array (length initial-input) :element-type 'character :initial-contents initial-input
              :adjustable t :fill-pointer t))

(defun completing-read (screen prompt completions &key (initial-input "") require-match)
  "Read a line of input through stumpwm and return it with TAB
completion. Completions can be a list, an fbound symbol, or a
function. If its an fbound symbol or a function then that function is
passed the substring to complete on and is expected to return a list
of matches. If require-match argument is non-nil then the input must
match with an element of the completions."
  (check-type completions (or list function symbol))
  
  (let ((line (read-one-line screen prompt
                             :completions completions
                             :initial-input initial-input
                             :require-match require-match)))
    (when line (string-trim " " line))))

(defun read-one-line (screen prompt &key completions (initial-input "") require-match password)
  "Read a line of input through stumpwm and return it. Returns nil if the user aborted."
  (let ((*input-last-command* nil)
        (*input-completions* (if (or (functionp completions)
                                     (and (symbolp completions)
                                          (fboundp completions)))
                                 (funcall completions initial-input)
                                 completions))
        (input (make-input-line :string (make-input-string initial-input)
                                :position (length initial-input)
                                :history -1
                                :password password)))
    (labels ((match-input ()
               (let* ((in (string-trim " " (input-line-string input)))
                      (compls (input-find-completions in completions)))
                 (and (consp compls)
                      (string= in (if (consp (car compls))
                                      (caar compls)
                                      (car compls))))))
             (key-loop ()
               (with-focus (screen-input-window screen)
                 (loop for key = (read-key-or-selection)
                    do
                      (cond ((stringp key)
                             ;; handle selection
                             (input-insert-string input key)
                             (draw-input-bucket screen prompt input))
                            ;; skip modifiers
                            ((is-modifier (car key)))
                            ((process-input screen prompt input (car key) (cdr key))
                             (if (or (not require-match)
                                     (match-input))
                                 (return (input-line-string input))
                                 (draw-input-bucket screen prompt input "[No match]" t))))))))
      (draw-input-bucket screen prompt input)
      (setup-input-window screen prompt input)
      (catch :abort
        (unwind-protect (key-loop)
          (shutdown-input-window screen))))))

(defun read-one-char (screen)
  "Read a single character from the user."
  (with-focus (screen-key-window screen)
    (let ((k (read-key-no-modifiers)))
      (keycode->character (car k) (xlib:make-state-keys (cdr k))))))

(defun read-one-char-or-click (group)
  "Read a single character from the user or a click."
  (with-focus (screen-key-window (group-screen group))
    (multiple-value-bind (has-click k x y)
        (read-key-no-modifiers-or-click)
      (if has-click
          (values t nil x y)
          (values nil (keycode->character (car k) (xlib:make-state-keys (cdr k))) nil nil)))))

(defun prompt-text-y (index font y-padding)
  "Calculate the y position of text in a prompt."
  (+ y-padding
     (* (font-height font) index)
     (font-ascent font)))

(defun potential-string-expansion (string expansion)
  "This takes a string and a possible expansion and checks to see if it could be, 
treating hyphens as delimiters between words. This attempts to emulate emacs. 
For example the string \"t-a-o\" would match any string whose first word begins
with t, second with a, and third with o."
  (let ((word-list-one (cl-ppcre:split "-" string))
	(word-list-two (cl-ppcre:split "-" expansion)))
    (when (<= (length word-list-one) (length word-list-two))
      (not (member :impossible (mapcar (lambda (w1 w2)
					 (if (uiop:string-prefix-p w1 w2)
					     :possible
					     :impossible))
				       word-list-one
				       word-list-two))))))

(defun emacs-style-command-complete (string)
  (loop for completion in (all-commands)
	when (potential-string-expansion string completion)
	  collect completion))

(defun get-completion-preview-list (input-line all-completions)
  (if (string= "" input-line)
      '()
      (multiple-value-bind (completions more)
          (take *maximum-completions*
                (remove-duplicates
		 (remove-if
		  (lambda (str)
		    (or (string= str "")
			(< (length str) (length input-line))
			(not (potential-string-expansion input-line str))))
		  all-completions)
		 :test #'string=))
	(if more
            (append (butlast completions)
                    (list (format nil "... and ~D more" (1+ (length more)))))
            completions))))

(defun draw-input-bucket (screen prompt input &optional (tail "") errorp)
  "Draw to the screen's input window the contents of input."
  (let* ((gcontext (screen-message-gc screen))
         (win (screen-input-window screen))
         (font (screen-font screen))
         (prompt-lines (ppcre:split #\Newline prompt))
         (prompt-lines-length (length prompt-lines))
         (input-line (input-line-string input))
         (completions (get-completion-preview-list input-line *input-completions*))
         (completions-length (length completions))
         (prompt-offset (text-line-width font
                                         (first (last prompt-lines))
                                         :translate #'translate-id))
         (line-content (input-line-string input))
         (string (if (input-line-password input)
                     (make-string (length line-content) :initial-element #\*)
                     line-content))
         (string-width (loop for char across string
                          summing (text-line-width (screen-font screen)
                                                   (string char)
                                                   :translate #'translate-id)))
         (space-width  (text-line-width (screen-font screen) " "    :translate #'translate-id))
         (tail-width   (text-line-width (screen-font screen) tail   :translate #'translate-id))
         (full-string-width (+ string-width space-width))
         (pos (input-line-position input))
         (width (max (loop :for line :in (append prompt-lines completions)
                        :maximize (text-line-width font line :translate #'translate-id))
                     (+ prompt-offset
                        (max 100 (+ full-string-width space-width tail-width))))))
    (when errorp (rotatef (xlib:gcontext-background gcontext)
                          (xlib:gcontext-foreground gcontext)))
    (xlib:with-state (win)
      (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext))
        (xlib:draw-rectangle win gcontext 0 0
                             (xlib:drawable-width win)
                             (xlib:drawable-height win) t))
      (setf (xlib:drawable-width win) (+ width (* *message-window-padding* 2)))
      (setf (xlib:drawable-height win) (+ (* prompt-lines-length (font-height font))
                                          (* *message-window-y-padding* 2)
                                          (* completions-length (font-height font))))
      (setup-win-gravity screen win *input-window-gravity*)

      ;; Display the input window text.
      (loop for i from 0 below (+ prompt-lines-length completions-length)
         if (< i prompt-lines-length)
         do (draw-image-glyphs win gcontext font
                               *message-window-padding*
                               (prompt-text-y i font *message-window-y-padding*)
                               (nth i prompt-lines)
                               :translate #'translate-id
                               :size 16)
         else
         do (draw-image-glyphs win gcontext font
                               *message-window-padding*
                               (prompt-text-y i font *message-window-y-padding*)
                               (nth (- i prompt-lines-length) completions)
                               :translate #'translate-id
                               :size 16))
      ;; Pad the input to the left.
      (loop with x = (+ *message-window-padding* prompt-offset)
         for char across string
         for i from 0 below (length string)
         for char-width = (text-line-width (screen-font screen) (string char) :translate #'translate-id)
         if (= pos i)
         do (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext)
                                          :background (xlib:gcontext-foreground gcontext))
              (draw-image-glyphs win gcontext (screen-font screen)
                                 x
                                 (prompt-text-y (1- prompt-lines-length)
                                                font
                                                *message-window-y-padding*)
                                 (string char)
                                 :translate #'translate-id
                                 :size 16))
         else
         do (draw-image-glyphs win gcontext (screen-font screen)
                               x
                               (prompt-text-y (1- prompt-lines-length)
                                              font
                                              *message-window-y-padding*)
                               (string char)
                               :translate #'translate-id
                               :size 16)
         end
         do (incf x char-width)
         finally (when (>= pos (length string))
                   (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext)
                                                 :background (xlib:gcontext-foreground gcontext))
                     (draw-image-glyphs win gcontext (screen-font screen)
                                        x
                                        (prompt-text-y (1- prompt-lines-length)
                                                       font
                                                       *message-window-y-padding*)
                                        " "
                                        :translate #'translate-id
                                        :size 16))))
      (draw-image-glyphs win gcontext (screen-font screen)
                         (+ *message-window-padding* prompt-offset full-string-width space-width)
                         (prompt-text-y (1- prompt-lines-length)
                                        font
                                        *message-window-y-padding*)
                         tail
                         :translate #'translate-id
                         :size 16))
    (when errorp
      (sleep 0.05)
      (rotatef (xlib:gcontext-background gcontext)
               (xlib:gcontext-foreground gcontext))
      (draw-input-bucket screen prompt input tail))))

(defun code-state->key (code state)
  (let* ((mods    (xlib:make-state-keys state))
         (shift-p (and (find :shift mods) t))
         (altgr-p (and (intersection (modifiers-altgr *modifiers*) mods) t))
         (base    (if altgr-p 2 0))
         (sym     (xlib:keycode->keysym *display* code base))
         (upsym   (xlib:keycode->keysym *display* code (+ base 1))))
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


;;; input string utility functions

(defun input-submit (input key)
  (declare (ignore input key))
  :done)

(defun input-complete-and-submit (input key)
  (declare (ignore key))
  (let* ((split (split-seq (input-line-string input) " "))
	 (c (emacs-style-command-complete (car split))))
    (when (and (= 1 (length split))
	         (= 1 (length c)))
      (input-replace-line input (car c)))
    (define-key *input-map* (kbd "SPC") 'input-self-insert)
    (define-key *input-map* (kbd "RET") 'input-submit)
    :done))

(defun input-abort (input key)
  (declare (ignore input key))
  (throw :abort nil))

(defun input-goto-char (input point)
  "Move the cursor to the specified point in the string"
  (setf (input-line-position input) (min (max 0 point)
                                         (length (input-line-string input)))))

(defun input-insert-string (input string)
  "Insert @var{string} into the input at the current
position. @var{input} must be of type @var{input-line}. Input
functions are passed this structure as their first argument."
  (check-type string string)
  (loop for c across string
        do (input-insert-char input c)))

(defun input-replace-line (input new)
  (let ((replace-with (if (listp new) (coerce new 'string) new)))
    (setf (input-line-position input) 0) ; set position to kill from
    (input-kill-line input nil)
    (loop for c across replace-with
	  do (input-insert-char input c))))

(defun input-point (input)
  "Return the position of the cursor."
  (check-type input input-line)
  (input-line-position input))

(defun input-validate-region (input start end)
  "Return a value pair of numbers where the first number is < the
second and neither excedes the bounds of the input string."
  (values (max 0 (min start end))
          (min (length (input-line-string input))
               (max start end))))

(defun input-delete-region (input start end)
  "Delete the region between start and end in the input string"
  (check-type input input-line)
  (check-type start fixnum)
  (check-type end fixnum)
  (multiple-value-setq (start end) (input-validate-region input start end))
  (replace (input-line-string input) (input-line-string input)
           :start2 end :start1 start)
  (decf (fill-pointer (input-line-string input)) (- end start))
  (cond
    ((< (input-line-position input) start))
    ((< (input-line-position input) end)
     (setf (input-line-position input) start))
    (t
     (decf (input-line-position input) (- end start)))))

(defun input-insert-char (input char)
  "Insert @var{char} into the input at the current
position. @var{input} must be of type @var{input-line}. Input
functions are passed this structure as their first argument."
  (vector-push-extend #\_ (input-line-string input))
  (replace (input-line-string input) (input-line-string input)
           :start2 (input-line-position input) :start1 (1+ (input-line-position input)))
  (setf (char (input-line-string input) (input-line-position input)) char)
  (incf (input-line-position input)))

(defun input-substring (input start end)
  "Return a the substring in INPUT bounded by START and END."
  (subseq (input-line-string input) start end))

(defun input-insert-space (input key)
  (declare (ignore key))
  (let ((char (xlib:keysym->character *display* (key-keysym (kbd "SPC")))))
    (if (or (not (characterp char)) (null char))
	:error
	(input-insert-char input char))))


;;; "interactive" input functions

(defun input-find-completions (str completions)
  (if (or (functionp completions)
          (and (symbolp completions)
               (fboundp completions)))
      (funcall completions str)
      (remove-if-not (lambda (elt)
                       (when (listp elt)
                         (setf elt (car elt)))
                       (and (<= (length str) (length elt))
                            (string= str elt
                                     :end1 (length str)
                                     :end2 (length str))))
                     completions)))

(defun input-complete (input direction)
  (unless (find *input-last-command* '(input-complete-forward
                                       input-complete-backward))
    (input-completion-reset *input-completion-style* (input-find-completions (input-substring input 0 (input-point input)) *input-completions*)))
  (input-completion-complete *input-completion-style* input direction))

(defun input-complete-forward (input key)
  (declare (ignore key))
  (input-complete input :forward))

(defun input-complete-backward (input key)
  (declare (ignore key))
  (input-complete input :backward))

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

(defun input-forward-kill-word (input key)
  (declare (ignore key))
  (let* ((p1 (position-if 'alphanumericp (input-line-string input) :start (input-line-position input)))
         (p2 (and p1 (position-if-not 'alphanumericp (input-line-string input) :start p1))))
    (input-delete-region input (input-point input) (or p2 (length (input-line-string input))))))

(defun input-backward-kill-word (input key)
  (declare (ignore key))
  (let* ((p1 (position-if 'alphanumericp (input-line-string input) :end (input-line-position input) :from-end t))
         (p2 (and p1 (position-if-not 'alphanumericp (input-line-string input) :end p1 :from-end t))))
    (input-delete-region input (input-point input) (or (and p2 (1+ p2)) 0))))

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

(defun input-self-insert (input key)
  (let ((char (xlib:keysym->character *display* (key-keysym key))))
    (if (or (key-mods-p key) (null char)
            (not (characterp char)))
        :error
        (input-insert-char input char))))

(defun input-insert-hyphen-or-space (input key)
  (declare (ignore key))
  (let ((toggle (member #\space (coerce (input-line-string input) 'list)))
	;; toggle relies on the fact that there can be no spaces in a command
	(completion
	  (emacs-style-command-complete
	   (car (split-seq (input-line-string input) " ")))))
    (if (and (not toggle) (= 1 (length completion)))
	(progn
	  (input-replace-line input (car completion))
	  (input-insert-char input #\space))
	(let ((char (if toggle #\space #\-)))
	  (if (or (not (characterp char)) (null char))
	      :error
	      (input-insert-char input char))))))

(defun input-yank-selection (input key)
  (declare (ignore key))
  ;; if we own the selection then just insert it.
  (if (getf *x-selection* :primary)
      (input-insert-string input (getf *x-selection* :primary))
      (xlib:convert-selection :primary
                              :string (screen-input-window (current-screen))
                              :stumpwm-selection)))

(defun input-yank-clipboard (input key)
  (declare (ignore key))
  (if (getf *x-selection* :clipboard)
      (input-insert-string input (getf *x-selection* :clipboard))
      (xlib:convert-selection :clipboard
                              :string (screen-input-window (current-screen))
                              :stumpwm-selection)))


;;; Misc functions

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
                   (prog1
                       (funcall command input key)
                     (setf *input-last-command* command)
                     (draw-input-bucket screen prompt input))
                   :error))))
    (case (process-key code state)
      (:done
       (unless (or (input-line-password input)
                   (and *input-history-ignore-duplicates*
                        (string= (input-line-string input) (first *input-history*))))
         (push (input-line-string input) *input-history*))
       :done)
      (:abort
       (throw :abort t))
      (:error
       ;; FIXME draw inverted text
       (draw-input-bucket screen prompt input "" t)
       nil)
      (t
       (draw-input-bucket screen prompt input)
       nil))))

(defun all-modifier-codes ()
  "Return all the keycodes that are associated with a modifier."
  (flatten (multiple-value-list (xlib:modifier-mapping *display*))))

(defun get-modifier-map ()
  (labels ((find-mod (mod codes)
             (let* ((keysym (keysym-name->keysym mod))
                    (keycodes (multiple-value-list (xlib:keysym->keycodes *display* keysym))))
               (intersection keycodes codes))))
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
                     (push mod (modifiers-hyper modifiers)))
                    ((find-mod "Num_Lock" codes)
                     (push mod (modifiers-numlock modifiers)))
                    ((find-mod "ISO_Level3_Shift" codes)
                     (push mod (modifiers-altgr modifiers)))))
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
  "Convert a stump modifier list to a string."
  (let ((alist '((:alt . "A-") (:meta . "M-") (:hyper . "H-") (:super . "S-"))))
    (apply #'concatenate 'string (mapcar (lambda (x) (cdr (assoc x alist))) state))))

;; (defun keycode->string (code state)
;;   (concatenate 'string (mod->string state)
;;             (string (keysym->character *display*
;;                                        (xlib:keycode->keysym *display* code 0)
;;                                        state))))

;; (defun cook-keycode (code state)
;;   (values (xlib:keycode->keysym *display* code 0) (x11mod->stumpmod state)))

(defun y-or-n-p (message)
  "Ask a \"y or n\" question on the current screen and return T if the
user presses 'y'."
  (message "~a(y or n) " message)
  (char= (read-one-char (current-screen))
        #\y))

(defun yes-or-no-p (message)
  "ask a \"yes or no\" question on the current screen and return T if the
user presses 'yes'"
  (loop for line = (string-trim
                    '(#\Space)
                    (read-one-line (current-screen)
                                   (format nil "~a(yes or no) " message)
                                   :completions
                                   '("yes" "no")))
        until (find line '("yes" "no") :test 'string-equal)
        do (message "Please answer yes or no.")
           (sleep 1)
        finally (return (string-equal line "yes"))))

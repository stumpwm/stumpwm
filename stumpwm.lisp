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
;; To start stumpwm, load this file stumpwm-input.lisp,
;; stumpwm-input.lisp, and stumpwm-user.lisp and evaluate:
;; (stumpwm:stumpwm "" :display 0) This is assuming you want to
;; connect to display 0.
;;
;; Code:

(defpackage #:stumpwm
  (:use #:common-lisp)
  (:export stumpwm))
     
;(require :clx)

(in-package #:stumpwm)


;;; Hooks

(defun char->keysym (ch)
  "Convert a char to a keysym"
  (first (xlib:character->keysyms ch)))

(defvar *map-window-hook* '()
  "A hook called whenever a window is mapped.")

(defvar *unmap-window-hook* '()
  "A hook called whenever a window is withdrawn.")

(defvar *new-window-hook* '()
  "A hook called whenever a window is created.")

(defvar *destroy-window-hook* '()
  "A hook called whenever a window is destroyed.")

(defvar *focus-window-hook* '()
  "A hook called when a window is given focus.")

(defvar *unfocus-window-hook* '()
  "A hook called when a window loses focus.")

;; Data types and globals used by stumpwm

(defvar *display* nil
  "The display for the X server")

(defvar *font-name* "9x15bold"
  "The name of the font to use when stumpwm displays messages.")

(defvar *prefix-key* (char->keysym #\t)
  "The key to use as the prefix key")

(defvar *prefix-modifiers* '(:control)
  "The modifier list for the prefix key")

(defvar *window-format-fn*
  (lambda (screen w)
    (format nil "~D~C~A"
	    (window-number screen w)
	    (cond ((xlib:window-equal w (screen-current-window screen))
		   #\*)
		  ((and (xlib:window-p (second (screen-mapped-windows screen)))
			(xlib:window-equal w (second (screen-mapped-windows screen))))
		   #\+)
		  (t #\-))
	    (window-name w)))
		   "The function called when printing a window list. It is passed the
screen and window. It should return a string.")

(defstruct key-binding
  "A structure to map from a keystroke to a command."
  (key nil :type xlib:keysym)
  (mods nil :type list)
  (fn nil :type function))

(defparameter *key-binding-alist*
  (list (cons (char->keysym #\n) 'focus-next-window)
	(cons (char->keysym #\p) 'focus-prev-window)
	(cons (char->keysym #\w) 'echo-windows)
	(cons (char->keysym #\k) 'delete-current-window)
	(cons (char->keysym #\b) 'banish-pointer)
	(cons (char->keysym #\') 'select-window)
	(cons (char->keysym #\t) 'other-window)
	(cons (char->keysym #\g) (lambda (s))) ; abort
	(cons (char->keysym #\0) (lambda (s) (select-window-number s 0)))
	(cons (char->keysym #\1) (lambda (s) (select-window-number s 1)))
	(cons (char->keysym #\2) (lambda (s) (select-window-number s 2)))
	(cons (char->keysym #\3) (lambda (s) (select-window-number s 3)))
	(cons (char->keysym #\4) (lambda (s) (select-window-number s 4)))
	(cons (char->keysym #\5) (lambda (s) (select-window-number s 5)))
	(cons (char->keysym #\6) (lambda (s) (select-window-number s 6)))
	(cons (char->keysym #\7) (lambda (s) (select-window-number s 7)))
	(cons (char->keysym #\8) (lambda (s) (select-window-number s 8)))
	(cons (char->keysym #\9) (lambda (s) (select-window-number s 9))))
  "An alist of keysym function pairs.")

;; FIXME: This variable is set only once but it needs to be set after
;; the display is opened. So should it have +'s around it even though
;; it's defined as a variable?
(defvar +wm-delete-window+ nil
  "The atom used to delete a window.")

;; Window states
(defconstant +withdrawn-state+ 0)
(defconstant +normal-state+ 1)
(defconstant +iconic-state+ 3)  

;; Message window constants
(defvar *message-window-padding* 5)

;; line editor
(defvar *editor-bindings* 
  "A list of key-bindings for line editing."
  nil)

(defstruct frame
  number
  x
  y
  width
  height
  window)

(defstruct modifiers
  (meta nil)
  (alt nil)
  (hyper nil)
  (super nil))
  

(defstruct screen
  number
  frame-tree
  ;; 
  modifiers
  font
  current-frame
  current-window
  ;; A list of all mapped windows. Used for navigating windows.
  mapped-windows
  ;; A hash table for stumpwm properties on any absorbed windows.
  window-table
  ;; Windows for various input and output
  key-window
  message-window
  input-window
  frame-window)

(defvar *screen-list* '()
  "List of screens")

;; Misc. utility functions
(defun conc1 (list arg)
  "Append arg to the end of list"
  (nconc list (list arg)))

(defun sort1 (list sort-fn)
  "Return a sorted copy of list."
  (let ((copy (copy-list list)))
    (sort copy sort-fn)))

(defun mapcar-hash (fn hash)
  "Just like maphash except it accumulates the result in a list and
calls fn on the value for the key hash-key, not the pair."
  (let ((accum nil))
    (labels ((mapfn (key val) (push (funcall fn val) accum)))
      (maphash #'mapfn hash))
    accum))

;; Screen helper functions

(defun screen-height (screen)
  (let ((root (xlib:screen-root (screen-number screen))))
    (xlib:drawable-height root)))

(defun screen-width (screen)
  (let ((root (xlib:screen-root (screen-number screen))))
    (xlib:drawable-width root)))

(defun find-screen (root)
  "Return the screen containing the root window"
  (find-if (lambda (s)
	     (eql (xlib:screen-root (screen-number s))
		  root))
	   *screen-list*))


;; Miscellaneous functions

(defun window-screen (w)
  "Return the screen associated with window w."
  (find-screen (xlib:drawable-root w)))

(defun window-name (win)
  (concatenate 'string (mapcar #'code-char (xlib:get-property win :WM_NAME))))

(defun window-number (screen win)
  (gethash :number (gethash win (screen-window-table screen))))

(defun sort-windows (screen)
  "Return a copy of the screen's window list sorted by number."
  (sort1 (screen-mapped-windows screen)
	 (lambda (a b)
	   (< (window-number screen a)
	      (window-number screen b)))))

(defun process-new-window (win)
  "When a new window is created (or when we are scanning initial
windows), this function dresses the window up and gets it ready to be
managed."
  ;; Listen for events
  (setf (xlib:window-event-mask win) '(:structure-notify
				       :property-change
				       :colormap-change
				       :focus-change)))

(defun process-existing-windows (screen)
  "Windows present when stumpwm starts up must be absorbed by stumpwm."
  (let ((children (xlib:query-tree (xlib:screen-root (screen-number screen)))))
    (dolist (win children)
      (let ((map-state (xlib:window-map-state win))
	    (wm-state (first (xlib:get-property win :WM_STATE))))
	;; Don't process our key window or override-redirect windows.
	(unless (or (xlib:window-equal win (screen-key-window screen))
		    (eq (xlib:window-override-redirect win) :on))
	  (if (or (eql map-state :viewable)
		  (eql wm-state +iconic-state+))
	      (progn
		(print win)
		(print "Processing...")
		(process-new-window win)
		;; Simulate a map request on the new window
		(handle-map-request win))))))))


;; Initialization functions

(defun init-screen (screen-number)
  "Given a screen number, returns a screen structure with initialized members"
  ;; Listen for the window manager events on the root window
  (setf (xlib:window-event-mask (xlib:screen-root screen-number))
	'(:substructure-redirect
	  :substructure-notify
	  :property-change))
  (let* ((white (xlib:screen-white-pixel screen-number))
	 (black (xlib:screen-black-pixel screen-number))
	 (input-window (xlib:create-window :parent (xlib:screen-root screen-number)
					   :x 0 :y 0 :width 20 :height 20
					   :background black
					   :border white
					   :border-width 1
					   :colormap (xlib:screen-default-colormap
						      screen-number)
					   :event-mask '(:key-press)))
	 (key-window (xlib:create-window :parent (xlib:screen-root screen-number)
					 :x 0 :y 0 :width 1 :height 1
					 :background white
					 :border white
					 :border-width 0
					 :colormap (xlib:screen-default-colormap
						    screen-number)
					 :event-mask '(:key-press)))
	 (frame-window (xlib:create-window :parent (xlib:screen-root screen-number)
					   :x 0 :y 0 :width 1 :height 1
					   :background white
					   :border white
					   :border-width 0
					   :colormap (xlib:screen-default-colormap
						      screen-number)
					   :event-mask '()))
	 (message-window (xlib:create-window :parent (xlib:screen-root screen-number)
					     :x 0 :y 0 :width 1 :height 1
					     :background black
					     :border white
					     :border-width 1
					     :colormap (xlib:screen-default-colormap
							screen-number)
					     :event-mask '())))
    ;; Map the key window and select key press events on it
    (xlib:map-window key-window)
    ;; Create our screen structure
    (make-screen :number screen-number
		 :frame-tree (list (make-frame 
				    :number 0
				    :x 0
				    :y 0
				    :width (xlib:screen-width screen-number)
				    :height (xlib:screen-height screen-number)))
		 :font (xlib:open-font *display* *font-name*)
		 :current-frame 0
		 :window-table (make-hash-table)
		 :key-window key-window
		 :message-window message-window
		 :input-window input-window
		 :frame-window frame-window)))

(defun init-atoms ()
  (setf +wm-delete-window+ (xlib:find-atom *display* 'WM_DELETE_WINDOW)))

(defun error-handler (display error-key &rest key-vals &key asynchronous &allow-other-keys)
  "Handle X errors"
  (case error-key
    (:access-error
     (pprint '(another window manager is running)))
    (t
     (pprint (list 'error error-key key-vals)))))

;; (stumpwm "" :display 0)
(defun stumpwm (host &key display protocol)
  "Start the stump window manager."
  (setf *display* (xlib:open-display host :display display :protocol protocol))
  ;; set our input handler
  (setf (xlib:display-error-handler *display*) #'error-handler)
  ;; In the event of an error, we always need to close the display
  (unwind-protect
      (progn
	;; Initialize the necessary atoms
	(init-atoms)
	;; Initialize all the screens
	(setf *screen-list* (mapcar #'init-screen (xlib:display-roots *display*)))
	;; Process existing windows on each screen
	(mapcar #'process-existing-windows *screen-list*)
	(stumpwm-internal-loop))
    (xlib:close-display *display*)))

(defun stumpwm-internal-loop ()
  "The internal loop that waits for events and handles them"
  (handle-events *display*))


;; Event handler functions
(defparameter *event-fn-table* (make-hash-table)
  "A hash of event types to functions")

(defmacro define-stump-event-handler (event keys &body body)
  (let ((fn-name (gensym)))
  `(labels ((,fn-name (&rest event-slots &key ,@keys &allow-other-keys)
		      (declare (ignorable event-slots))
		      ,@body))
     (setf (gethash ,event *event-fn-table*) #',fn-name))))


(define-stump-event-handler :map-notify (event-window window override-redirect-p)
  (unless (eql event-window window)
    (handle-map-notify window)))

(define-stump-event-handler :configure-request (stack-mode parent window above-sibling x y width height border-width value-mask)
  (pprint value-mask)
  (handle-configure-request window x y width height border-width stack-mode value-mask))

(define-stump-event-handler :map-request (parent window)
  (handle-map-request window))

(define-stump-event-handler :unmap-notify (send-event-p event-window window configure-p)
  (unless (or send-event-p
	      (xlib:window-equal window event-window))
    ;; There are two kinds of unmap notify events:
    ;; the straight up ones where event-window and
    ;; window are the same, and substructure unmap
    ;; events when the event-window is the parent
    ;; of window. Since the parent of all stumpwm
    ;; windows is the root window, use it to find
    ;; the screen.
    (handle-unmap-notify (find-screen event-window) window)))

(define-stump-event-handler :create-notify (parent window x y width height border-width override-redirect-p)
  (handle-create-notify window x y width height border-width override-redirect-p))

(define-stump-event-handler :destroy-notify (send-event-p event-window window)
  (unless (or send-event-p
	      (xlib:window-equal event-window window))
    ;; Ignore structure destroy notifies and only
    ;; use substructure destroy notifiers. This way
    ;; event-window is the root window.
    (handle-destroy-notify (find-screen event-window) window)))

(define-stump-event-handler :key-press (code state window root)
  (let ((screen (find-screen root)))
;;     (if (xlib:window-equal (screen-input-window screen) window)
;; 	;; It's from our input window
;; 	(read-input screen code state)
      ;; It's a command
      (handle-key-press screen window code state)))

(defun handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (pprint (list 'handling 'event event-key))
  (let ((eventfn (gethash event-key *event-fn-table*)))
    (when eventfn
      (apply eventfn event-slots))
    t))

(defun handle-events (display)
  "Reads any events from the queue and processes them."
  (loop
   (xlib:process-event *display* :handler #'handle-event :timeout 5)))

(defun geometry-hints (screen win)
  "Return hints for max width and height and increment hints. These
hints have been modified to always be defined and never be greater
than the root window's width and height."
  (let* ((x 0)
	 (y 0)
	 (width (- (xlib:drawable-width (xlib:screen-root (screen-number screen)))
		   (* 2 (xlib:drawable-border-width win))))
	 (height (- (xlib:drawable-height (xlib:screen-root (screen-number screen)))
		    (* 2 (xlib:drawable-border-width win))))
	 (inc-x 1)
	 (inc-y 1)
	 (hints (xlib:wm-normal-hints win))
	 (hints-width (xlib:wm-size-hints-max-width hints))
	 (hints-height (xlib:wm-size-hints-max-height hints))
	 (hints-inc-x (xlib:wm-size-hints-width-inc hints))
	 (hints-inc-y (xlib:wm-size-hints-height-inc hints)))
    ;; Adjust the defaults if the window is a transient_for window.
    (when (xlib:get-property win :WM_TRANSIENT_FOR)
      (setf x (truncate (- (/ width 2)
			   (/ (xlib:drawable-width win) 2)))
	    y (truncate (- (/ height 2)
			   (/ (xlib:drawable-height win) 2)))
	    width (xlib:drawable-width win)
	    height (xlib:drawable-height win)))
    ;; Update our defaults if the window has the hints
    (when (and hints-width
	       (< hints-width width))
      (setf width hints-width))
    (when (and hints-height
	       (< hints-height height))
      (setf height hints-height))
    (when hints-inc-x
      (setf inc-x hints-inc-x))
    (when hints-inc-y
      (setf inc-y hints-inc-y))
    ;; Now return our findings as a list
    (list x y width height inc-x inc-y)))

(defun maximize-window (win)
  "Maximize the window."
  (let* ((screen (window-screen win))
	 (hints (geometry-hints screen win))
	 (x (first hints))
	 (y (second hints))
	 (width (third hints))
	 (height (fourth hints))
	 (inc-x (fifth hints))
	 (inc-y (sixth hints)))
    ;; Move the window
    (setf (xlib:drawable-x win) x
	  (xlib:drawable-y win) y)
    ;; Resize the window
    (setf (xlib:drawable-width win)
	  (+ (xlib:drawable-width win)
	     (* inc-x (truncate (/ (- width (xlib:drawable-width win)) inc-x))))
	  (xlib:drawable-height win)
	  (+ (xlib:drawable-height win)
	     (* inc-y (truncate (/ (- height (xlib:drawable-height win)) inc-y)))))
    (xlib:display-force-output *display*)))

(defun handle-configure-request (w x y width height border-width stack-mode value-mask)
  (labels ((has-x (mask) (= 1 (logand mask 1)))
	   (has-y (mask) (= 2 (logand mask 2)))
	   (has-w (mask) (= 4 (logand mask 4)))
	   (has-h (mask) (= 8 (logand mask 8)))
	   (has-bw (mask) (= 16 (logand mask 16)))
    	   (has-stackmode (mask) (= 64 (logand mask 64))))
    (let ((screen (window-screen w)))
      (xlib:with-state (w)
		       (pprint value-mask)
		       (when (has-x value-mask)
			 (pprint 'x)
			 (setf (xlib:drawable-x w) x))
		       (when (has-y value-mask)
			 (pprint 'y)
			 (setf (xlib:drawable-y w) y))
		       (when (has-h value-mask)
			 (pprint 'h)
			 (setf (xlib:drawable-height w) height))
		       (when (has-w value-mask)
			 (pprint 'w)
			 (setf (xlib:drawable-width w) width))
		       (when (has-bw value-mask)
			 (pprint 'bw)
			 (setf (xlib:drawable-border-width w) border-width))
		       ;; After honouring the request, maximize it
		       (when (member w (screen-mapped-windows screen))
			 (maximize-window w))))))

;; FIXME: I think some of this code should be put in the map hook
;; instead of hardcoded.
(defun handle-map-request (w)
  (let ((screen (window-screen w)))
    ;; Add the window to our mapped window list.
    (add-window screen w)
    ;; Create the window-table entry, adding it's number
    (let ((num (find-free-window-number screen w)))
      (setf (gethash w (screen-window-table screen)) (make-hash-table))
      (setf (gethash :number (gethash w (screen-window-table screen))) num))
    ;; Maximize the window
    (maximize-window w)
    ;; Map the window
    (xlib:map-window w)
    ;; Grab the prefix key
    (grab-keys-on-window w)
    ;; Run the map window hook on it
    (run-hook-with-args *map-window-hook* w)
    ;; Give it focus
    (focus-window w)))

(defun handle-map-notify (w)
  "Perform any tasks on the window, now that it has been mapped."
  )

(defun handle-unmap-notify (screen win)
  ;; Remove the window from the list of mapped windows.
  (setf (screen-mapped-windows screen)
	(delete win (screen-mapped-windows screen)))
  ;; Clean up the window's entry in screen-window-table
  (remhash win (screen-window-table screen))
  ;; Run the unmap hook on the window
  (run-hook-with-args *unmap-window-hook* win)
  ;; If the current window was unmapped, then switch to another
  ;; window in our mapped window list.
  (when (and (xlib:window-equal (screen-current-window screen) win)
	     (not (null (screen-mapped-windows screen))))
    (let ((cwin (car (screen-mapped-windows screen))))
      (focus-window cwin))))


(defun handle-create-notify (win x y width height border-width override-redirect-p)
  (unless override-redirect-p
    (process-new-window win)
    (run-hook-with-args *new-window-hook* win)))

(defun handle-destroy-notify (screen win)
  ;; In some cases, we get a destroy notify before an unmap notify, so
  ;; simulate an unmap notify (for now).
  (handle-unmap-notify screen win)
  (run-hook-with-args *destroy-window-hook* win))

(defun is-modifier (keysym)
  "Return t if keycode is a modifier"
  (member keysym (list (char->keysym :character-set-switch)
		       (char->keysym :left-shift)
		       (char->keysym :right-shift)
		       (char->keysym :left-control)
		       (char->keysym :right-control)
		       (char->keysym :caps-lock)
		       (char->keysym :shift-lock)
		       (char->keysym :left-meta)
		       (char->keysym :right-meta)
		       (char->keysym :left-alt)
		       (char->keysym :right-alt)
		       (char->keysym :left-super)
		       (char->keysym :right-super)
		       (char->keysym :left-hyper)
		       (char->keysym :right-hyper))))

(defun handle-command-key (screen code state)
  "Find the command mapped to the (code state) and executed it."
  (let* ((key (xlib:keycode->keysym *display* code 0))
	 (fn (assoc key *key-binding-alist*)))

    (print key)
    (print fn)
    (if (null fn)
	(print "no match...")
      (progn
	(print "FOUND IT!")
	(funcall (cdr fn) screen)))))


(defun grab-pointer (screen)
  "Grab the pointer and set the pointer shape."
  (let* ((white (xlib:make-color :red 1.0 :green 1.0 :blue 1.0))
	 (black (xlib:make-color :red 0.0 :green 0.0 :blue 0.0))
	 (cursor-font (xlib:open-font *display* "cursor"))
	 (cursor (xlib:create-glyph-cursor :source-font cursor-font
					   :source-char 64
					   :mask-font cursor-font
					   :mask-char 65
					   :foreground black
					   :background white)))
    (xlib:grab-pointer (screen-key-window screen) nil :owner-p nil
		       :cursor cursor)))

(defun ungrab-pointer ()
  "Remove the grab on the cursor and restore the cursor shape."
  (xlib:ungrab-pointer *display*))

(defun handle-key-press (screen window code state)
  ;; Unmap the message window if it is mapped
  (unless (eq (xlib:window-map-state (screen-message-window screen)) :unmapped)
    (xlib:unmap-window (screen-message-window screen)))
  ;; grab the keyboard
  (grab-pointer screen)
  (xlib:grab-keyboard (screen-key-window screen) :owner-p nil
		      :sync-keyboard-p nil :sync-pointer-p nil)
  (print "Awaiting command key")
  ;; Listen for key
  (let ((key (do ((k (read-key screen) (read-key screen)))
		 ((not (is-modifier (xlib:keycode->keysym *display* (car k) 0))) k))))
    (print "Handling command")
    ;; We've read our key, so we can release the keyboard.
    (ungrab-pointer)
    (xlib:ungrab-keyboard *display*)
    (xlib:display-force-output *display*)
    (handle-command-key screen (car key) (cdr key))))


;;; Window management functions 

(defun grab-keys-on-window (win)
  (xlib:grab-key win (xlib:keysym->keycodes *display* *prefix-key*)
		 :modifiers *prefix-modifiers* :owner-p t
		 :sync-pointer-p nil :sync-keyboard-p nil))

(defun ungrab-keys-on-window (win)
  (xlib:ungrab-key win :any :modifiers '(:any)))

;;; Hook functionality

(defun run-hook-with-args (hook &rest args)
  "Call each function in HOOK and pass args to it" 
  (dolist (fn hook)
    (apply fn args)))

(defun run-hook (hook)
  "Call each function in HOOK."
  (run-hook-with-args hook))

(defmacro add-hook (hook fn)
  "Add a function to a hook."
  `(setf ,hook (adjoin ,fn ,hook)))


;;; Navigation

(defun find-free-window-number (screen window)
  (let* ((nums (sort (mapcar-hash (lambda (val) (gethash :number val))
				  (screen-window-table screen))
		     #'<))
	 (new-num (loop for n on nums
			when (and (numberp (second n))
				  (numberp (first n))
				  (not (= (second n) (1+ (first n)))))
			do (return (1+ (first n))))))
    (pprint nums)
    (if new-num
	new-num
      ;; there was no space between the numbers, so use the last + 1
      (if (car (last nums))
	  (1+ (car (last nums)))
	0))))

	  

(defun add-window (screen window)
  "add window to the head of the mapped-windows list."
  ;(assert (not (member window (screen-mapped-windows screen))))
  (push window (screen-mapped-windows screen)))
  
(defun move-window-to-head (screen window)
  "Move window to the head of the mapped-windows list."
  ;(assert (member window (screen-mapped-windows screen)))
  (setf (screen-mapped-windows screen) (delete window (screen-mapped-windows screen)))
  (push window (screen-mapped-windows screen)))

(defun focus-window (window)
  "Give the window focus. This means the window will be visible,
maximized, and given focus."
  (let ((screen (window-screen window)))
    (setf (xlib:window-priority window) :above)
    (xlib:set-input-focus *display* window :pointer-root)
    ;; Move the window to the head of the mapped-windows list
    (move-window-to-head screen window)
    ;; If another window was focused, then call the unfocus hook for
    ;; it.
    (when (screen-current-window screen)
      (run-hook-with-args *unfocus-window-hook* (screen-current-window screen)))
    (setf (screen-current-window screen) window)
    (run-hook-with-args *focus-window-hook* window)))

;; FIXME: This doesn't quite work yet because when no window is
;; focused the root window has focus, not the key window. Just gotta
;; write that code somewhere.
(defun current-screen ()
  "Return the current screen. The current screen is the screen whose
window has focus. If no window has focus it is the screen that last
focus of a window."
  (let* ((win (xlib:input-focus *display*))
	 (screen (member-if (lambda (s)
			      (or (xlib:window-equal win (screen-current-window s))
				  (xlib:window-equal win (screen-key-window s))))
			    *screen-list*)))
    ;; We MUST be able to figure out the current screen by this method
    (assert screen)
    ;; Return the current screen
    (car screen)))

;;; Echoing strings

(defun create-message-window-gcontext (screen)
  "Create a graphic context suitable for printing characters."
  (xlib:create-gcontext :drawable (screen-message-window screen)
			:font (screen-font screen)
			:foreground
			(xlib:screen-white-pixel (screen-number screen))
			:background
			(xlib:screen-black-pixel (screen-number screen))))

(defun max-width (font l)
  "Return the width of the longest string in L using FONT."
  (loop for i in l
	maximize (xlib:text-width font i)))

(defun setup-message-window (screen l)
  (let ((height (* (length l)
		   (+ (xlib:font-ascent (screen-font screen))
		      (xlib:font-descent (screen-font screen)))))
	(width (max-width (screen-font screen) l))
	(screen-width (xlib:drawable-width (xlib:screen-root (screen-number screen))))
	(win (screen-message-window screen)))
    ;; Now that we know the dimensions, raise and resize it.
    (xlib:map-window (screen-message-window screen))
    (setf (xlib:drawable-y win) 0
	  (xlib:drawable-height win) height
	  (xlib:drawable-x win) (- screen-width width
				   (* (xlib:drawable-border-width win) 2)
				   (* *message-window-padding* 2))
	  (xlib:drawable-width win) (+ width (* *message-window-padding* 2))
	  (xlib:window-priority win) :above)
    ;; Clear the window
    (xlib:clear-area win)))

(defun invert-rect (screen win x y width height)
  "invert the color in the rectangular area. Used for highlighting text."
  (let ((gcontext (xlib:create-gcontext :drawable win
					:foreground
					(xlib:screen-white-pixel (screen-number screen))
					:function boole-xor)))
    (xlib:draw-rectangle win gcontext x y width height t)))

(defun echo-window-list (screen l)
  "Print each window in l to the screen and highlight the current window."
  (let* ((height (+ (xlib:font-descent (screen-font screen))
		    (xlib:font-ascent (screen-font screen))))
	 (names (mapcar (lambda (w)
			  (funcall *window-format-fn* screen w)) l))
	 (gcontext (create-message-window-gcontext screen))
	 (message-win (screen-message-window screen)))
    (setup-message-window screen names)
    ;; Loop through each window and print its name. If we come across
    ;; the current window, then highlight it.
    (loop for win in l
	  for name in names
	  ;; We need this so we can track the row for each window
	  for i from 0 to (length l)
	  do (xlib:draw-image-glyphs message-win gcontext
				     *message-window-padding*
				     (+ (* i height)
					(xlib:font-ascent (screen-font screen)))
				     name)
	  when (xlib:window-equal (screen-current-window screen) win)
	  do (invert-rect screen message-win
			  0 (* i height)
			  (xlib:drawable-width message-win)
			  height))))

;;; Pointer control

(defun warp-pointer (screen x y)
  (let ((root (xlib:screen-root (screen-number screen))))
    (xlib:warp-pointer root x y)))

;;; Window managing

(defun delete-window (window)
  "Send a delete event to the window."
  (print "delete window")
  (xlib:send-event window
		   :client-message nil
		   :window window
		   :type :WM_PROTOCOLS
		   :format 32
		   :data (list +wm-delete-window+)))

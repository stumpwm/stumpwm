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
;; To start stumpwm, load this file and stumpwm-user.lisp and evaluate:
;; (stumpwm:stumpwm "" :display 0) This is assuming you want to connect to
;; display 0.
;;
;; Code:

(defpackage #:stumpwm
  (:use #:common-lisp)
  (:export stumpwm))
     
(require :cmucl-clx)

(in-package #:stumpwm)


;;; Hooks

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

(defvar *prefix-key* (xlib:keysym #\t)
  "The key to use as the prefix key")

(defvar *prefix-modifiers* '(:control)
  "The modifier list for the prefix key")

(defvar *key-binding-alist* (list (cons (xlib:keysym #\n) 'focus-next-window)
				  (cons (xlib:keysym #\p) 'focus-prev-window)
				  (cons (xlib:keysym #\w) 'echo-windows)
				  (cons (xlib:keysym #\k) 'delete-current-window)
				  (cons (xlib:keysym #\b) 'banish-pointer))
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

(defstruct frame
  number
  x
  y
  width
  height
  window)

(defstruct screen
  number
  frame-tree
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
  (xlib:get-property win :WM_NAME))

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
  (let* ((white (xlib:screen-white-pixel screen-number))
	 (black (xlib:screen-black-pixel screen-number))
	 (input-window (xlib:create-window :parent (xlib:screen-root screen-number)
					   :x 0 :y 0 :width 20 :height 20
					   :background white
					   :border white
					   :border-width 0
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
    ;; Listen for the window manager events on the root window
    (setf (xlib:window-event-mask (xlib:screen-root screen-number))
	  '(:substructure-redirect
	    :substructure-notify
	    :property-change))
    ;; Create our screen structure
    (make-screen :number screen-number
		 :frame-tree (list (make-frame 
				    :number 0
				    :x 0
				    :y 0
				    :width (xlib:screen-width screen-number)
				    :height (xlib:screen-height screen-number)))
		 :current-frame 0
		 :window-table (make-hash-table)
		 :key-window key-window
		 :message-window message-window
		 :input-window input-window
		 :frame-window frame-window)))

(defun init-atoms ()
  (setf +wm-delete-window+ (xlib:find-atom *display* 'WM_DELETE_WINDOW)))

;; (stumpwm "" :display 0)
(defun stumpwm (host &key display protocol)
  "Start the stump window manager."
  (setf *display* (xlib:open-display host :display display :protocol protocol))
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

(defun handle-events (display)
  "Reads any events from the queue and processes them."
					;  (loop
  (xlib:event-case (display :discard-p t :force-output-p t)
		   (:configure-request 
		    (window x y width height border-width value-mask)
		    (print "Configure request")
		    (handle-configure-request window x y width height border-width value-mask)
		    nil)
		   (:map-request 
		    (window)
		    (print "Map request")
		    (handle-map-request window)
		    nil)
		   (:map-notify
		    (window event-window)
		    (unless (eql event-window window)
		      (print "map notify")
		      (handle-map-notify window))
		    nil)
		   (:unmap-notify
		    (window event-window send-event-p)
		    (unless (or send-event-p
				(xlib:window-equal window event-window))
		      (print "Unmap notify")
		      ;; There are two kinds of unmap notify events:
		      ;; the straight up ones where event-window and
		      ;; window are the same, and substructure unmap
		      ;; events when the event-window is the parent
		      ;; of window. Since the parent of all stumpwm
		      ;; windows is the root window, use it to find
		      ;; the screen.
		      (handle-unmap-notify (find-screen event-window) window))
		    nil)
		   (:create-notify
		    (window x y width height border-width override-redirect-p)
		    (print "Create notify")
		    (handle-create-notify window x y width height border-width override-redirect-p)
		    nil)
		   (:destroy-notify
		    (event-window window send-event-p)
		    (unless (or send-event-p
				(xlib:window-equal event-window window))
		      ;; Ignore structure destroy notifies and only
		      ;; use substructure destroy notifiers. This way
		      ;; event-window is the root window.
		      (print "Destroy notify")
		      (handle-destroy-notify (find-screen event-window) window))
		    nil)
		   (:key-press
		    (root window state code)
		    (print "Key press event")
		    (print window)
		    (print state)
		    (print code)
		    (print "---")
		    (handle-key-press (find-screen root) window code state)
		    nil)))

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
      (setf x (- (/ width 2)
		 (/ (xlib:drawable-width win) 2))
	    y (- (/ height 2)
		 (/ (xlib:drawable-height win) 2))
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

(defun handle-configure-request (w x y width height border-width value-mask)
  (let ((screen (window-screen w)))
    (xlib:with-state (w)
		     (setf (xlib:drawable-x w) x
			   (xlib:drawable-y w) y
			   (xlib:drawable-height w) height
			   (xlib:drawable-width w) width
			   (xlib:drawable-border-width w) border-width)

		     ;; After honouring the request, maximize it
		     (when (member w (screen-mapped-windows screen))
		       (maximize-window w)))))

;; FIXME: I think some of this code should be put in the map hook
;; instead of hardcoded.
(defun handle-map-request (w)
  (let ((screen (window-screen w)))
    ;; Add the window to our mapped window list.
    (setf (screen-mapped-windows screen)
	  (adjoin w (screen-mapped-windows screen)))
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
  (member keysym (list (xlib:keysym :character-set-switch)
		       (xlib:keysym :left-shift)
		       (xlib:keysym :right-shift)
		       (xlib:keysym :left-control)
		       (xlib:keysym :right-control)
		       (xlib:keysym :caps-lock)
		       (xlib:keysym :shift-lock)
		       (xlib:keysym :left-meta)
		       (xlib:keysym :right-meta)
		       (xlib:keysym :left-alt)
		       (xlib:keysym :right-alt)
		       (xlib:keysym :left-super)
		       (xlib:keysym :right-super)
		       (xlib:keysym :left-hyper)
		       (xlib:keysym :right-hyper))))

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
  (if (xlib:window-equal window (screen-key-window screen))
      (progn
	(unless (is-modifier (xlib:keycode->keysym *display* code 0))
	  (print "Handling command")
	  ;; We've read our key, so we can release the keyboard.
	  (ungrab-pointer)
	  (xlib:ungrab-keyboard *display*)
	  (xlib:display-force-output *display*)
	  (handle-command-key screen code state)))
    ;; Otherwise, grab the keyboard and wait for the next key press event.
    (progn 
      (grab-pointer screen)
      (xlib:grab-keyboard (screen-key-window screen) :owner-p nil
			  :sync-keyboard-p nil :sync-pointer-p nil)
      (print "Awaiting command key"))))


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

(defun focus-window (window)
  "Give the window focus. This means the window will be visible,
maximized, and given focus."
  (let ((screen (window-screen window)))
    (setf (xlib:window-priority window) :above)
    (xlib:set-input-focus *display* window :pointer-root)
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
			      (or (window-equal win (screen-current-window s))
				  (window-equal win (screen-key-window s))))
			    *screen-list*)))
    ;; We MUST be able to figure out the current screen by this method
    (assert screen)
    ;; Return the current screen
    (car screen)))

;;; Echoing strings

(defun create-message-window-gcontext (screen font)
  "Create a graphic context suitable for printing characters."
  (xlib:create-gcontext :drawable (screen-message-window screen)
			:font font
			:foreground
			(xlib:screen-white-pixel (screen-number screen))
			:background
			(xlib:screen-black-pixel (screen-number screen))))

(defun max-width (font l)
  "Return the width of the longest string in L using FONT."
  (loop for i in l
	maximize (xlib:text-width font i)))

(defun setup-message-window (screen font l)
  (let ((height (* (length l)
		   (+ (xlib:font-ascent font)
		      (xlib:font-descent font))))
	(width (max-width font l))
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
  (let* ((font (xlib:open-font *display* *font-name*))
	 (height (+ (xlib:font-descent font)
		    (xlib:font-ascent font)))
	 (names (mapcar #'window-name l))
	 (gcontext (create-message-window-gcontext screen font))
	 (message-win (screen-message-window screen)))
    (setup-message-window screen font names)
    ;; Loop through each window and print its name. If we come across
    ;; the current window, then highlight it.
    (loop for win in l
	  for name in names
	  ;; We need this so we can track the row for each window
	  for i from 0 to (length l)
	  do (xlib:draw-image-glyphs message-win gcontext
				     *message-window-padding*
				     (+ (* i height)
					(xlib:font-ascent font))
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

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

(in-package :stumpwm)


;;; Main

(defun load-rc-file ()
  "Load the user's .stumpwmrc file or the system wide one if that
doesn't exist. Returns a values list: whether the file loaded (t if no
rc files exist), the error if it didn't, and the rc file that was
loaded."
  (let* ((user-rc (probe-file (merge-pathnames (user-homedir-pathname) #p".stumpwmrc")))
	 (etc-rc (probe-file #p"/etc/stumpwmrc"))
	 (rc (or user-rc etc-rc)))
    (if rc
	;; TODO: Should we compile the file before we load it?
	(handler-case (load rc)
		      (error (c) (values nil (format nil "~s" c) rc))
		      (:no-error (&rest args) (declare (ignore args)) (values t nil rc)))
      (values t nil nil))))
    
(defun init-atoms ()
  (setf +wm-delete-window+ (xlib:intern-atom *display* "WM_DELETE_WINDOW")
	+wm-take-focus+ (xlib:intern-atom *display* "WM_TAKE_FOCUS")
;; 	+wm-state+ (xlib:find-atom *display* "WM_STATE")
;; 	+wm-protocols+ (xlib:find-atom *display* "WM_PROTOCOLS")
;; 	+rp-command+ (xlib:intern-atom *display* "RP_COMMAND")
;; 	+rp-command-request+ (xlib:intern-atom *display* "RP_COMMAND_REQUEST")
;; 	+rp-command-result+ (xlib:intern-atom *display* "RP_COMMAND_RESULT")
	)
)

(defun error-handler (display error-key &rest key-vals &key asynchronous &allow-other-keys)
  "Handle X errors"
  (declare (ignore display asynchronous))
  (case error-key
    ('xlib:access-error
     (error "Another window manager is running."))
    (t
     (dformat "Error ~S ~S~%" error-key key-vals))))

(defun stumpwm-internal-loop ()
  "The internal loop that waits for events and handles them."
  ;; before entering the interactive debugger, ungrab the keyboard. If
  ;; we don't the whole X server could be locked.
  (labels ((ungrab (condition hook)
	     (declare (ignore condition hook))
	     (dformat "Error! Ungrabbing keyboard.~%")
	     ;;#+clisp (ext:show-stack 1 100 (sys::the-frame))
	     (ungrab-keyboard)
	     (xlib:display-finish-output *display*)))
    (let ((*debugger-hook* #'ungrab))
      (catch :quit
	(loop
	   (run-hook *internal-loop-hook*)
	   (handler-case 
	       (progn
		 (if (> *timeout* 0)
		     (progn
		       (let* ((time-before (get-universal-time))
			      (nevents (xlib:event-listen *display* *timeout*))
			      (time-left  (- *timeout* (- (get-universal-time) time-before))))
			 (if (<= time-left 0)
			     (progn
			       (unmap-all-frame-indicators)
			       (unmap-all-message-windows)
			       (setf *timeout* 0))
			     (setf *timeout* time-left))
			 (when nevents
			   (xlib:process-event *display* :handler #'handle-event))))
		     ;; Otherwise, simply wait for an event
		     (xlib:process-event *display* :handler #'handle-event :timeout nil))
		 ;; flush any pending output. You'd think process-event would, but
		 ;; it seems not.
		 (xlib:display-finish-output *display*))
	     (error (c)
	       (ecase *top-level-error-action*
		 (:message
		  (let ((s (format nil "~&Caught ~a at the top level. Please report this." c)))
		    (write-line s)
		    (echo-string (current-screen) s)))
		 (:break (invoke-debugger c))
		 (:abort
		  (throw :quit (format nil "~&Caught ~a at the top level. Please report this." c)))))))))))

(defun parse-display-string (display)
  "Parse an X11 DISPLAY string and return the host and display from it."
  (let* ((colon (position #\: display))
	 (host (subseq display 0 colon))
	 (rest (subseq display (1+ colon)))
	 (dot (position #\. rest))
	 (num (parse-integer (subseq rest 0 dot))))
    (values host num)))

;; Usage: (stumpwm)
(defun stumpwm (&optional (display-str (or (getenv "DISPLAY") ":0")) protocol)
  "Start the stump window manager."
  (multiple-value-bind (host display) (parse-display-string display-str)
    (setf *display* (xlib:open-display host :display display :protocol protocol))
    ;; In the event of an error, we always need to close the display
    (unwind-protect
	 (progn
	   ;; we need to do this first because init-screen grabs keys
	   (update-modifier-map)
	   ;; Initialize all the screens
	   (handler-case
	       (setf *screen-list* (loop for i in (xlib:display-roots *display*)
				      for n from 0
				      collect (init-screen i n host)))
	     (xlib:access-error (c)
	       (declare (ignore c))
	       (return-from stumpwm (write-line "Another window manager is running."))))
	   ;; Initialize the necessary atoms
	   (init-atoms)
	   (mapc 'process-existing-windows *screen-list*)
	   ;; We need to setup each screen with its current window. Go
	   ;; through them in reverse so the first screen's frame ends up
	   ;; with focus.
	   (dolist (s (reverse *screen-list*))
	     (let ((group (screen-current-group s)))
	       (focus-frame group (tile-group-current-frame group))))
	   ;; Load rc file
	   (multiple-value-bind (success err rc) (load-rc-file)
	     (echo-string (current-screen)
			  (if success
			      "Welcome to The Stump Window Manager!"
			      (format nil "Error loading ~A: ~A" rc err))))
	   (run-hook *start-hook*)
	   ;; Let's manage.
	   (stumpwm-internal-loop))
      (xlib:close-display *display*))))

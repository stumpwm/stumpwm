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
		      (error (c) (values nil (format nil "~A" c) rc))
		      (:no-error (&rest args) (values t nil rc)))
      (values t nil nil))))
    
(defun init-atoms ()
  (setf +wm-delete-window+ (xlib:find-atom *display* "WM_DELETE_WINDOW")
	+wm-take-focus+ (xlib:find-atom *display* "WM_TAKE_FOCUS")
	+wm-state+ (xlib:find-atom *display* "WM_STATE")
	+wm-protocols+ (xlib:find-atom *display* "WM_PROTOCOLS")))

(defun error-handler (display error-key &rest key-vals &key asynchronous &allow-other-keys)
  "Handle X errors"
  (declare (ignorable display))
  (declare (ignorable key-vals))
  (declare (ignorable asynchronous))
  (case error-key
    ('xlib:access-error
     (error "Another window manager is running."))
    (t
     (dformat "Error ~S ~S~%" error-key key-vals))))

(defun stumpwm-internal-loop ()
  "The internal loop that waits for events and handles them."
  (loop
   (if (> *timeout* 0)
       (progn
	 (let ((time-before (get-universal-time)))
	   (xlib:process-event *display* :handler #'handle-event :timeout *timeout*)
	   (let ((time-left (- *timeout* (- (get-universal-time) time-before))))
	     (if (<= time-left 0)
		 (progn
		   (unmap-all-frame-indicators)
		   (unmap-all-message-windows)
		   (setf *timeout* 0))
	       (setf *timeout* time-left)))))
     ;; Otherwise, simply wait for an event
     (xlib:process-event *display* :handler #'handle-event :timeout nil))))


(defun parse-display-string (display)
  "Parse an X11 DISPLAY string and return the host and display from it."
  (let* ((colon (position #\: display))
	 (host (subseq display 0 colon))
	 (rest (subseq display (1+ colon)))
	 (dot (position #\. rest))
	 (num (parse-integer (subseq rest 0 dot))))
    (values host num)))

;; Usage: (stumpwm)
(defun stumpwm (&optional (display-str nil) &key protocol)
  "Start the stump window manager."
  (multiple-value-bind (host display) (parse-display-string (or display-str
								(getenv "DISPLAY")
								":0"))
    (setf *display* (xlib:open-display host :display display :protocol protocol)))
  ;; In the event of an error, we always need to close the display
  (unwind-protect
      (progn
	;; Initialize all the screens
	(handler-case
	 (setf *screen-list* (mapcar #'init-screen (xlib:display-roots *display*)))
	 (xlib:access-error (c)
           (declare (ignorable c))
           (return-from stumpwm (princ "Another window manager is running."))))
	;; Initialize the necessary atoms
	(init-atoms)
	(mapc #'process-existing-windows *screen-list*)
	;; Give the first screen's frame focus
	(focus-frame (first *screen-list*) (screen-current-frame (first *screen-list*)))
	;; Setup the default key bindings. FIXME: should this be in the hook?
	(set-default-bindings)
        ;; Set the DISPLAY-environment-variable properly. This is
        ;; necessary if Stumpwm is running from a Lisp in another
        ;; X-display.
	;; SBCL doesn't have a setenv.
        #-sbcl(setf (getenv "DISPLAY") display-str)
	(echo-string (first *screen-list*) "Welcome to The Stump Window Manager!")
	;; Load rc file
	(multiple-value-bind (success err rc) (load-rc-file)
	  (unless success
	    (echo-string (first *screen-list*)
			 (format nil "Error loading ~A: ~A" rc err))))
	(run-hook *start-hook*)
	;; Let's manage.
	(stumpwm-internal-loop))
    (xlib:close-display *display*)))

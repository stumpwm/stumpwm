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

(defun init-atoms ()
  (setf +wm-delete-window+ (xlib:find-atom *display* 'WM_DELETE_WINDOW)))

(defun error-handler (display error-key &rest key-vals &key asynchronous &allow-other-keys)
  "Handle X errors"
  (declare (ignorable display))
  (declare (ignorable key-vals))
  (declare (ignorable asynchronous))
  (case error-key
    ('xlib:access-error
     (error "Another window manager is running."))
    (t
     (pprint (list 'error error-key key-vals)))))

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
		   (unmap-all-message-windows)
		   (setf *timeout* 0))
	       (setf *timeout* time-left)))))
     ;; Otherwise, simply wait for an event
     (xlib:process-event *display* :handler #'handle-event :timeout nil))))


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
	(mapc #'process-existing-windows *screen-list*)
	;; Give the first screen's frame focus
	(focus-frame (first *screen-list*) (screen-current-frame (first *screen-list*)))
	;; Setup our keys. FIXME: should this be in the hook?
	(set-default-bindings)
	(run-hook *start-hook*)
	;; Let's manage.
	(stumpwm-internal-loop))
    (xlib:close-display *display*)))

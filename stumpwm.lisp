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

(defun load-rc-file (&optional (catch-errors t))
  "Load the user's .stumpwmrc file or the system wide one if that
doesn't exist. Returns a values list: whether the file loaded (t if no
rc files exist), the error if it didn't, and the rc file that was
loaded. When CATCH-ERRORS is nil, errors are left to be handled further up. "
  (let* ((user-rc (probe-file (merge-pathnames (user-homedir-pathname) #p".stumpwmrc")))
	 (etc-rc (probe-file #p"/etc/stumpwmrc"))
	 (rc (or user-rc etc-rc)))
    (if rc
	;; TODO: Should we compile the file before we load it?
        (if catch-errors
            (handler-case (load rc)
              (error (c) (values nil (format nil "~a" c) rc))
              (:no-error (&rest args) (declare (ignore args)) (values t nil rc)))
            (progn
              (load rc)
              (values t nil rc)))
      (values t nil nil))))

(defun error-handler (display error-key &rest key-vals &key asynchronous &allow-other-keys)
  "Handle X errors"
  ;; ignore asynchronous window errors
  (if (and asynchronous
	   (find error-key '(xlib:window-error xlib:drawable-error xlib:match-error)))
      (dformat 4 "Ignoring error: ~s~%" error-key)
      ;; all other errors are thrown and caught at the top level where
      ;; stumpwm quits, basically.
      (if asynchronous
	  (apply 'cerror "Ignore" error-key :display display :error-key error-key key-vals)
	  (apply 'error error-key :display display :error-key error-key key-vals))))

;;; Timers

(defvar *timer-list* nil
  "List of active timers.")

(defstruct timer
  time repeat function args)

(defun run-with-timer (secs repeat function &rest args)
  "Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if repeat is non-nil.
SECS and REPEAT may be reals.
The action is to call FUNCTION with arguments ARGS."
  (check-type secs (real 0 *))
  (check-type repeat (or null (real 0 *)))
  (check-type function (or function symbol))
  (let ((timer (make-timer
                :repeat repeat
                :function function
                :args args)))
    (schedule-timer timer secs)
    (setf *timer-list* (sort-timers (cons timer *timer-list*)))
    timer))

(defun cancel-timer (timer)
  "Remove TIMER from the list of active timers."
  (check-type timer timer)
  (setf *timer-list* (remove timer *timer-list*)))

(defun schedule-timer (timer when)
  (setf (timer-time timer) (+ (get-internal-real-time) 
                              (* when internal-time-units-per-second))))

(defun sort-timers (timers)
  "Return a new list of timers sorted by time to time out."
  (sort (copy-list timers) 
        (lambda (a b)
          (< (timer-time a) (timer-time b)))))

(defun run-expired-timers (timers)
  "Return a new list of valid timers and run the timer functions
of those expired."
  (let ((now (get-internal-real-time)))
    (sort-timers (loop for i in timers
                    with keepers = nil do
                    (if (< (timer-time i) now)
                        (progn
                          (apply (timer-function i) (timer-args i))
                          (when (timer-repeat i)
                            (schedule-timer i (timer-repeat i))
                            (push i keepers)))
                        (push i keepers))
                    finally (return keepers)))))

(defun get-next-timeout (timers)
  "Return the number of seconds until the next timeout or nil if there are no timers."
  (when timers
    (max (/ (- (timer-time (car timers)) (get-internal-real-time))
                      internal-time-units-per-second)
         0)))

(defun stumpwm-internal-loop ()
  "The internal loop that waits for events and handles them."
  ;; before entering the interactive debugger, ungrab the keyboard. If
  ;; we don't the whole X server could be locked.
  (catch :quit
    (loop
       (run-hook *internal-loop-hook*)
       (handler-bind
           ;; 	     ((or xlib:window-error xlib:drawable-error) (lambda (c)
           ;; 	       ;; Just in case some synchronous window error gets here
           ;; 	       ;; (this should be impossible) catch it and ignore it.
           ;; 	       (dformat 4 "top level ignore synchronous ~a~%" c))
           ((error (lambda (c)
                     (run-hook *top-level-error-hook*)
                     (ecase *top-level-error-action*
                       (:message
                        (let ((s (format nil "~&Caught '~a' at the top level. Please report this." c)))
                          (write-line s)
                          (print-backtrace)
                          (message "~a" s)))
                       (:break (invoke-debugger c))
                       (:abort
                        (format t "~&Caught '~a' at the top level. Please report this." c)
                        (print-backtrace)
                        (throw :quit t))))))
         ;; Note: process-event appears to hang for an unknown
         ;; reason. This is why it is passed a timeout in hopes that
         ;; this will keep it from hanging.
         (let ((timeout (get-next-timeout *timer-list*)))
           (dformat 10 "timeout: ~a~%" timeout)
           (if timeout
               (let* ((nevents (xlib:event-listen *display* timeout)))
                 (setf *timer-list* (run-expired-timers *timer-list*))
                 (when nevents
                   (xlib:process-event *display* :handler #'handle-event)))
               ;; Otherwise, simply wait for an event
               (xlib:process-event *display* :handler #'handle-event))
           ;; flush any pending output. You'd think process-event would, but
           ;; it seems not.
           (xlib:display-finish-output *display*)
           ;;(dformat 10 "toplevel focus: ~a~%" (multiple-value-list (xlib:input-focus *display*)))
           )))))

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
    (setf *display* (xlib:open-display host :display display :protocol protocol)
	  (xlib:display-error-handler *display*) 'error-handler)
    (with-simple-restart (quit-stumpwm "Quit Stumpwm")
      ;; In the event of an error, we always need to close the display
      (unwind-protect
           (progn
             ;; we need to do this first because init-screen grabs keys
             (update-modifier-map)
             ;; Initialize all the screens
             (handler-case
                 (progn (setf *screen-list* (loop for i in (xlib:display-roots *display*)
                                               for n from 0
                                               collect (init-screen i n host)))
                        (xlib:display-finish-output *display*))
               (xlib:access-error ()
                 (return-from stumpwm (write-line "Another window manager is running."))))
             (mapc 'process-existing-windows *screen-list*)
             ;; We need to setup each screen with its current window. Go
             ;; through them in reverse so the first screen's frame ends up
             ;; with focus.
             (dolist (s (reverse *screen-list*))
               (let ((group (screen-current-group s)))
                 (when (group-windows group)
                   (frame-raise-window group (tile-group-current-frame group) (car (group-windows group))))
                 (focus-frame group (tile-group-current-frame group))))
             ;; Load rc file
             (let ((*package* (find-package *default-package*)))
               (multiple-value-bind (success err rc) (load-rc-file)
                 (if success
                     (and *startup-message* (message "~a" *startup-message*))
                     (message "Error loading ~A: ~A" rc err))))
             ;; Let's manage.
             (let ((*package* (find-package *default-package*)))
               (run-hook *start-hook*)
               (stumpwm-internal-loop)))
        (xlib:close-display *display*)))))

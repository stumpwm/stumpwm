;; Copyright (C) 2003-2008 Shawn Betts
;; Copyright (C) 2017 David Bjergaard
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
;; Provides the code for timers.
;;
;; Code:
(in-package :stumpwm)
(export '(cancel-timer
          timer-p
          idle-time
          run-with-timer))

;;; Timers

(defvar *toplevel-io* nil
  "Top-level I/O loop")

(defvar *timer-list* nil
  "List of active timers.")

(defvar *timer-list-lock* (sb-thread:make-mutex)
  "Lock that should be held whenever *TIMER-LIST* is modified.")

(defstruct timer
  time repeat function args)

(defun idle-time (screen)
  "Returns the time in seconds since idle according to the root window
of the `screen'."
  (/ (first (multiple-value-list
             (xlib:screen-saver-get-idle
              *display* (screen-root screen))))
     1000.0))

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
    (labels ((append-to-list ()
               (sb-thread:with-mutex (*timer-list-lock*)
                 (setf *timer-list* (merge 'list *timer-list* (list timer)
                                           #'< :key #'timer-time)))))
      (call-in-main-thread #'append-to-list)
      timer)))

(defun cancel-timer (timer)
  "Remove TIMER from the list of active timers."
  (check-type timer timer)
  (sb-thread:with-mutex (*timer-list-lock*)
    (setf *timer-list* (remove timer *timer-list*))))

(defun schedule-timer (timer when)
  (setf (timer-time timer) (+ (get-internal-real-time)
                              (* when internal-time-units-per-second))))

(defun sort-timers (timers)
  (let ((now (get-internal-real-time))
        (pending ())
        (remaining ()))
    (dolist (timer timers)
      (if (<= (timer-time timer) now)
          (progn (push timer pending)
                 (when (timer-repeat timer)
                   (schedule-timer timer (timer-repeat timer))
                   (push timer remaining)))
          (push timer remaining)))
    (values pending remaining)))


(defun execute-timers (timers)
  (map nil #'execute-timer timers))

(defun execute-timer (timer)
  (apply (timer-function timer) (timer-args timer)))

(defun run-expired-timers ()
  (let ((expired (sb-thread:with-mutex (*timer-list-lock*)
                   (multiple-value-bind (pending remaining)
                       (sort-timers *timer-list*)
                     (update-timer-list remaining)
                     pending))))
    ;; Call the timers after the lock has been released
    (execute-timers expired)))

(defun update-timer-list (timers)
  "Update the timer list, sorting the timers by which is closer expiry."
  (setf *timer-list*
        (sort timers #'< :key #'timer-time)))
(defun get-next-timeout (timers)
  "Return the number of seconds until the next timeout or nil if there are no timers."
  (when timers
    (max (/ (- (timer-time (car timers)) (get-internal-real-time))
            internal-time-units-per-second)
         0)))

(defclass stumpwm-timer-channel () ())

(defmethod io-channel-ioport (io-loop (channel stumpwm-timer-channel))
  (declare (ignore io-loop))
  nil)
(defmethod io-channel-events ((channel stumpwm-timer-channel))
  (sb-thread:with-mutex (*timer-list-lock*)
    (if *timer-list*
        `((:timeout ,(timer-time (car *timer-list*))))
        '(:loop))))
(defmethod io-channel-handle ((channel stumpwm-timer-channel) (event (eql :timeout)) &key)
  (run-expired-timers))
(defmethod io-channel-handle ((channel stumpwm-timer-channel) (event (eql :loop)) &key)
  (run-expired-timers))

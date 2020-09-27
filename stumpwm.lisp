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
;; Code:

(in-package :stumpwm)

(export '(*toplevel-io*
          stumpwm
          call-in-main-thread
          in-main-thread-p
          push-event))

(defvar *in-main-thread* nil
  "Dynamically bound to T during the execution of the main stumpwm function.")

;;; Main

(defun load-rc-file (&optional (catch-errors t))
  "Load the user's .stumpwmrc file or the system wide one if that
doesn't exist. Returns a values list: whether the file loaded (t if no
rc files exist), the error if it didn't, and the rc file that was
loaded. When CATCH-ERRORS is nil, errors are left to be handled
further up. "
  (let* ((user-rc
           (probe-file (merge-pathnames #p".stumpwmrc" (user-homedir-pathname))))
         (dir-rc
           (probe-file (merge-pathnames #p".stumpwm.d/init.lisp" (user-homedir-pathname))))
         (conf-rc
           (probe-file (uiop:xdg-config-home #p"stumpwm/config/")))
         (etc-rc (probe-file #p"/etc/stumpwmrc"))
         (rc (or user-rc dir-rc conf-rc etc-rc)))
    (if rc
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
  (cond
    ;; ignore asynchronous window errors
    ((and asynchronous
          (find error-key '(xlib:window-error xlib:drawable-error xlib:match-error)))
     (dformat 4 "Ignoring error: ~s~%" error-key))
    ((eq error-key 'xlib:access-error)
     (write-line "Another window manager is running.")
     (throw :top-level :quit))
     ;; all other asynchronous errors are printed.
     (asynchronous
      (message "Caught Asynchronous X Error: ~s ~s." error-key key-vals))
     (t
      (apply 'error error-key :display display :error-key error-key key-vals))))


(defgeneric handle-top-level-condition (c))

(defmethod handle-top-level-condition (c)
  ;; Do nothing by default; there's nothing wrong with signalling
  ;; arbitrary conditions
  )

(defmethod handle-top-level-condition ((c warning))
  (muffle-warning))

(defmethod handle-top-level-condition ((c serious-condition))
  (when (and (find-restart :remove-channel)
             (not (typep *current-io-channel*
                         '(or stumpwm-timer-channel
                           display-channel
                           request-channel))))
    (message "Removed channel ~S due to uncaught error '~A'." *current-io-channel* c)
    (invoke-restart :remove-channel))
  (ecase *top-level-error-action*
    (:message
     (let ((s (format nil "~&Caught '~a' at the top level. Please report this." c)))
       (write-line s)
       (print-backtrace)
       (message "^1*^B~a" s)))
    (:break (restart-case
                (invoke-debugger c)
              (:abort-debugging ()
                :report (lambda (stream) (format stream "abort debugging"))
                (throw :top-level (list c (backtrace-string))))))
    (:abort
     (throw :top-level (list c (backtrace-string))))))


(defclass request-channel ()
  ((in    :initarg :in
          :reader request-channel-in)
   (out   :initarg :out
          :reader request-channel-out)
   (queue :initform nil
          :accessor request-channel-queue)
   (lock  :initform (sb-thread:make-mutex)
          :reader request-channel-lock)))

(defvar *request-channel* nil)

(defmethod io-channel-ioport (io-loop (channel request-channel))
  (io-channel-ioport io-loop (request-channel-in channel)))

(defmethod io-channel-events ((channel request-channel))
  (list :read))

(defmethod io-channel-handle ((channel request-channel) (event (eql :read)) &key)
  ;; At this point, we know that there is at least one request written
  ;; on the pipe. We read all the data off the pipe and then evaluate
  ;; all the waiting jobs.
  (loop
    with in = (request-channel-in channel)
    do (read-byte in)
    while (listen in))
  (let ((events (sb-thread:with-mutex ((request-channel-lock channel))
                  (let ((queue-copy (request-channel-queue channel)))
                    (setf (request-channel-queue channel) nil)
                    queue-copy))))
    (dolist (event (reverse events))
      (funcall event))))

(defun in-main-thread-p ()
  *in-main-thread*)

(defun push-event (fn)
  (sb-thread:with-mutex ((request-channel-lock *request-channel*))
    (push fn (request-channel-queue *request-channel*)))
  (let ((out (request-channel-out *request-channel*)))
    ;; For now, just write a single byte since all we want is for the
    ;; main thread to process the queue. If we want to handle
    ;; different types of events, we'll have to change this so that
    ;; the message sent indicates the event type instead.
    (write-byte 0 out)
    (finish-output out)))

(defun call-in-main-thread (fn)
  (cond ((in-main-thread-p)
         (funcall fn))
        (t
         (push-event fn))))

(defclass display-channel ()
  ((display :initarg :display)))

(defmethod io-channel-ioport (io-loop (channel display-channel))
  (io-channel-ioport io-loop (slot-value channel 'display)))
(defmethod io-channel-events ((channel display-channel))
  (list :read :loop))
(flet ((dispatch-all (display)
         (block handle
           (loop
              (xlib:display-finish-output display)
              (let ((nevents (xlib:event-listen display 0)))
                (unless nevents (return-from handle))
                (xlib:with-event-queue (display)
                  (run-hook *event-processing-hook*)
                  ;; Note: process-event appears to hang for an unknown
                  ;; reason. This is why it is passed a timeout in hopes that
                  ;; this will keep it from hanging.
                  (xlib:process-event display :handler #'handle-event :timeout 0)))))))
  (defmethod io-channel-handle ((channel display-channel) (event (eql :read)) &key)
    (dispatch-all (slot-value channel 'display)))
  (defmethod io-channel-handle ((channel display-channel) (event (eql :loop)) &key)
    (dispatch-all (slot-value channel 'display))))

(defun stumpwm-internal-loop ()
  (loop
     (with-simple-restart (:new-io-loop "Recreate I/O loop")
       (let ((io (make-instance *default-io-loop*)))
         (io-loop-add io (make-instance 'stumpwm-timer-channel))
         (io-loop-add io (make-instance 'display-channel :display *display*))

         ;; If we have no implementation for the current CL, then
         ;; don't register the channel.
         (multiple-value-bind (in out)
             (open-pipe)
           (let ((channel (make-instance 'request-channel :in in :out out)))
             (io-loop-add io channel)
             (setq *request-channel* channel)))

         (setf *toplevel-io* io)
         (loop
            (handler-bind
                ((t (lambda (c)
                      (handle-top-level-condition c))))
              (io-loop io :description "StumpWM")))))))

(defun parse-display-string (display)
  "Parse an X11 DISPLAY string and return the host and display from it."
  (ppcre:register-groups-bind (protocol host ('parse-integer display screen))
                              ("^(?:(.*?)/)?(.*?)?:(\\d+)(?:\\.(\\d+))?" display :sharedp t)
    (values
     ;; clx doesn't like (vector character *)
     (coerce (or host "")
             '(simple-array character (*)))
     display screen
     (cond (protocol
             (intern1 protocol :keyword))
           ((or (string= host "")
                (string-equal host "unix"))
            :local)
           (t :internet)))))

(defun ensure-data-dir ()
  (ensure-directories-exist (data-dir) :mode #o611))

(defun data-dir ()
  (merge-pathnames ".stumpwm.d/" (user-homedir-pathname)))

(defun stumpwm-internal (display-str)
  (multiple-value-bind (host display screen protocol) (parse-display-string display-str)
    (declare (ignore screen))
    (setf *display* (xlib:open-display host :display display :protocol protocol)
          (xlib:display-error-handler *display*) 'error-handler)
    (with-simple-restart (quit-stumpwm "Quit Stumpwm")
      ;; In the event of an error, we always need to close the display
      (unwind-protect
           (progn
             (let ((*initializing* t))
               (ensure-data-dir)
               (open-log)
               ;; Start hashing the user's PATH so completion is quick
               ;; the first time they try to run a command.
               (sb-thread:make-thread #'rehash)
               
               ;; we need to do this first because init-screen grabs
               ;; keys
               (update-modifier-map)
               ;; Initialize all the screens
               (setf *screen-list* (loop for i in (xlib:display-roots *display*)
                                      for n from 0
                                      collect (init-screen i n host)))
               (xlib:display-finish-output *display*)
               ;; Load rc file
               (let ((*package* (find-package *default-package*)))
                 (multiple-value-bind (success err rc) (load-rc-file)
                   (if success
                       (and *startup-message* (message *startup-message* (print-key *escape-key*)))
                       (message "^B^1*Error loading ^b~A^B: ^n~A." rc err))))
               (when *last-unhandled-error*
                 (message-no-timeout "^B^1*StumpWM Crashed With An Unhandled Error!~%Copy the error to the clipboard with the 'copy-unhandled-error' command.~%^b~a^B^n~%~%~a."
                          (first *last-unhandled-error*) (second *last-unhandled-error*)))
               (mapc 'process-existing-windows *screen-list*)
               ;; We need to setup each screen with its current window. Go
               ;; through them in reverse so the first screen's frame ends up
               ;; with focus.
               (dolist (s (reverse *screen-list*))
                 ;; map the current group's windows
                 (mapc 'unhide-window (reverse (group-windows (screen-current-group s))))
                 ;; update groups
                 (dolist (g (reverse (screen-groups s)))
                   (dformat 3 "Group windows: ~S~%" (group-windows g))
                   (group-startup g))
                 ;; switch to the (old) current group.
                 (let ((netwm-id (first (xlib:get-property (screen-root s) :_NET_CURRENT_DESKTOP))))
                   (when (and netwm-id (< netwm-id (length (screen-groups s))))
                     (switch-to-group (elt (sort-groups s) netwm-id))))
                 (redraw-current-message (current-screen))))
             ;; Let's manage.
             (let ((*package* (find-package *default-package*)))
               (run-hook *start-hook*)
               (stumpwm-internal-loop)))
        (progn
          (xlib:close-display *display*)
          (close-log)))))
  ;; what should the top level loop do?
  :quit)

;; Usage: (stumpwm)
(defun stumpwm (&optional (display-str (or (getenv "DISPLAY") ":0")))
  "Start the stump window manager."
  (let ((*in-main-thread* t))
    (setf *data-dir*
          (make-pathname :directory (append (pathname-directory (user-homedir-pathname))
                                            (list ".stumpwm.d"))))
    (init-load-path *module-dir*)
    (loop
      (let ((ret (catch :top-level
                   (stumpwm-internal display-str))))
        (setf *last-unhandled-error* nil)
        (cond ((and (consp ret)
                    (typep (first ret) 'condition))
               (format t "~&Caught '~a' at the top level. Please report this.~%~a"
                       (first ret) (second ret))
               (setf *last-unhandled-error* ret))
              ;; we need to jump out of the event loop in order to hup
              ;; the process because otherwise we get errors.
              ((eq ret :hup-process)
               (run-hook *restart-hook*)
               (apply 'execv (first sb-ext:*posix-argv*) sb-ext:*posix-argv*))
              ((eq ret :restart)
               (run-hook *restart-hook*))
              (t
               (run-hook *quit-hook*)
               ;; the number is the unix return code
               (return-from stumpwm 0)))))))

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
;; message printing functions
;;
;; Code:

(in-package #:stumpwm)

(export '(echo-string
          err
          message))

(defun setup-message-window (screen width height)
  (let ((win (screen-message-window screen)))
    ;; Now that we know the dimensions, raise and resize it.
    (xlib:with-state (win)
      (setf (xlib:drawable-height win) height
            (xlib:drawable-width win) (+ width (* *message-window-padding* 2))
            (xlib:window-priority win) :above)
      (gravitate-xwin screen (current-head) win *message-window-gravity*))
    (xlib:map-window win)
    (incf (screen-ignore-msg-expose screen))
    ;; Have to flush this or the window might get cleared
    ;; after we've already started drawing it.
    (xlib:display-finish-output *display*)))

(defun unmap-message-window (screen)
  "Unmap the screen's message window, if it is mapped."
  (unless (eq (xlib:window-map-state (screen-message-window screen)) :unmapped)
    (xlib:unmap-window (screen-message-window screen))))

(defun unmap-all-message-windows ()
  (mapc #'unmap-message-window *screen-list*)
  (when (timer-p *message-window-timer*)
    (cancel-timer *message-window-timer*)
    (setf *message-window-timer* nil)))

(defun reset-message-window-timer ()
  "Set the message window timer to timeout in *timeout-wait* seconds."
  (unless *ignore-echo-timeout*
    (when (timer-p *message-window-timer*)
      (cancel-timer *message-window-timer*))
    (setf *message-window-timer* (run-with-timer *timeout-wait* nil
                                                 'unmap-all-message-windows))))

(defun show-frame-outline (group &optional (clear t))
  ;; Don't draw if this isn't a current group!
  (when (find group *screen-list* :key #'screen-current-group)
    (dformat 5 "show-frame-outline!~%")
    ;; *resize-hides-windows* uses the frame outlines for display,
    ;; so try not to interfere.
    (unless (eq *top-map* *resize-map*)
      (when clear
        (clear-frame-outlines group))
      (let ((frame (tile-group-current-frame group)))
        (unless (and (= 1 (length (tile-group-frame-tree group)))
                     (atom (first (tile-group-frame-tree group))))
          ;; draw the outline
          (unless (frame-window frame)
            (draw-frame-outline group frame t t)))))))

(defclass frame-indicator-window (stumpui:text-window
                                  stumpui:timed-window)
  ()
  (:default-initargs :event-mask '(:exposure)
                     :padding 0))

(defun show-frame-indicator (group &optional force)
  (show-frame-outline group)
  ;; FIXME: Arg, this test is already done in show-frame-outline
  (when (find group *screen-list* :key #'screen-current-group)
    (when (or force
              (and (not *suppress-frame-indicator*)
                   (or (> (length (tile-group-frame-tree group)) 1)
                       (not (atom (first (tile-group-frame-tree group)))))))
      (let ((window (screen-frame-indicator-window (current-screen)))
            (text (if (stringp *frame-indicator-text*)
                      *frame-indicator-text*
                      (prin1-to-string *frame-indicator-text*))))
        (stumpui:window-show window (tile-group-current-frame group)
                             :timeout *timeout-frame-indicator-wait*
                             :text text)))))

(defun push-last-message (screen strings highlights)
  ;; only push unique messages
  (unless *record-last-msg-override*
    (push strings (screen-last-msg screen))
    (push highlights (screen-last-msg-highlights screen))
    ;; crop for size
    (when (>= (length (screen-last-msg screen)) *max-last-message-size*)
      (setf (screen-last-msg screen) (butlast (screen-last-msg screen)))
      (setf (screen-last-msg-highlights screen) (butlast (screen-last-msg-highlights screen))))))

(defun redraw-current-message (screen)
  (let ((*record-last-msg-override* t)
        (*ignore-echo-timeout* t))
    (dformat 5 "Redrawing message window!~%")
    (apply 'echo-string-list screen (screen-current-msg screen) (screen-current-msg-highlights screen))))

(defun echo-nth-last-message (screen n)
  (let ((*record-last-msg-override* t))
    (apply 'echo-string-list screen (nth n (screen-last-msg screen)) (nth n (screen-last-msg-highlights screen)))))

(defun echo-string-list (screen strings &rest highlights)
  "Draw each string in l in the screen's message window. HIGHLIGHT is
  the nth entry to highlight."
  (when strings
    (unless *executing-stumpwm-command*
      (multiple-value-bind (width height)
          (rendered-size strings (screen-message-cc screen))
        (setup-message-window screen width height)
        (render-strings (screen-message-cc screen) *message-window-padding* 0 strings highlights))
      (setf (screen-current-msg screen)
            strings
            (screen-current-msg-highlights screen)
            highlights)
      ;; Set a timer to hide the message after a number of seconds
      (if *suppress-echo-timeout*
          ;; any left over timers need to be canceled.
          (when (timer-p *message-window-timer*)
            (cancel-timer *message-window-timer*)
            (setf *message-window-timer* nil))
          (reset-message-window-timer)))
    (push-last-message screen strings highlights)
    (xlib:display-finish-output *display*)
    (dformat 5 "Outputting a message:~%~{        ~a~%~}" strings)
    (apply 'run-hook-with-args *message-hook* strings)))

(defun echo-string (screen msg)
  "Display @var{string} in the message bar on @var{screen}. You almost always want to use @command{message}."
  (echo-string-list screen (split-string msg (string #\Newline))))

(defun message (fmt &rest args)
  "run FMT and ARGS through `format' and echo the result to the current screen."
  (echo-string (current-screen) (apply 'format nil fmt args)))


(defun err (fmt &rest args)
  "run FMT and ARGS through format and echo the result to the
current screen along with a backtrace. For careful study, the
message does not time out."
  (let ((*suppress-echo-timeout* t))
    (echo-string (current-screen)
                 (concat (apply 'format nil fmt args)
                         (backtrace-string)))))

(defun message-no-timeout (fmt &rest args)
  "Like message, but the window doesn't disappear after a few seconds."
  (let ((*suppress-echo-timeout* t))
    (apply 'message fmt args)))

;;; Commands

(defvar *lastmsg-nth* nil)

(defcommand lastmsg () ()
  "Display the last message. If the previous command was lastmsg, then
continue cycling back through the message history."
  (if (string= *last-command* "lastmsg")
      (progn
        (incf *lastmsg-nth*)
        (if (>= *lastmsg-nth* (length (screen-last-msg (current-screen))))
            (setf *lastmsg-nth* 0)))
      (setf *lastmsg-nth* 0))
  (if (screen-last-msg (current-screen))
      (echo-nth-last-message (current-screen) *lastmsg-nth*)
      (message "No last message.")))

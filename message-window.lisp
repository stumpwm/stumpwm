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

;;; Frame indicators

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

;;; Message window

(defclass message-window (stumpui:text-window
                          stumpui:timed-window)
  ()
  (:default-initargs :event-mask '(:exposure)))

(defmethod stumpui:window-gravity ((window message-window))
  *message-window-gravity*)

(defmethod (setf stumpui:window-gravity) (gravity (window message-window))
  (setf *message-window-gravity* gravity))

(defmethod stumpui:text-window-padding ((window message-window))
  *message-window-padding*)

(defmethod (setf stumpui:text-window-padding) (padding (window message-window))
  (setf *message-window-padding* padding))

(defun unmap-message-window (screen)
  "Unmap the screen's message window, if it is mapped."
  (stumpui:window-hide (screen-message-window screen)))

(defun unmap-all-message-windows ()
  (mapc #'unmap-message-window *screen-list*))

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
    (stumpui:window-redraw (screen-message-window screen))))

(defun echo-nth-last-message (screen n)
  (let ((*record-last-msg-override* t))
    (apply 'echo-string-list
           screen
           (nth n (screen-last-msg screen))
           (nth n (screen-last-msg-highlights screen)))))

(defun echo-string-list (screen strings &rest highlights)
  "Draw each string in l in the screen's message window. HIGHLIGHT is
  the nth entry to highlight."
  (unless *executing-stumpwm-command*
    (stumpui:window-show (screen-message-window screen)
                         (current-head)
                         :text strings
                         :highlights highlights
                         :timeout (unless *suppress-echo-timeout*
                                    *timeout-wait*)))
  (push-last-message screen strings highlights)
  (xlib:display-finish-output *display*)
  (dformat 5 "Outputting a message:~%~{        ~a~%~}" strings)
  (apply 'run-hook-with-args *message-hook* strings))

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

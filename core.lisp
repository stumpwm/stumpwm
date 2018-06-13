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
;; This file contains core functionality including functions on
;; windows, screens, and events.
;;
;; Code:

(in-package :stumpwm)

(export '(grab-pointer ungrab-pointer))

;;; keyboard helper functions
(defun key-to-keycode+state (key)
  (let ((code (xlib:keysym->keycodes *display* (key-keysym key))))
    (cond ((eq (xlib:keycode->keysym *display* code 0) (key-keysym key))
           (values code (x11-mods key)))
          ((eq (xlib:keycode->keysym *display* code 1) (key-keysym key))
           (values code (apply 'xlib:make-state-mask
                               (cons :shift (xlib:make-state-keys (x11-mods key))))))
          (t
           ;; just warn them and go ahead as scheduled
           (warn "Don't know how to encode ~s" key)
           (values code (x11-mods key))))))

(defun send-fake-key (win key)
  "Send a fake key press event to win."
  (let ((xwin (window-xwin win)))
    (multiple-value-bind (code state) (key-to-keycode+state key)
      (dolist (event '(:key-press :key-release))
        (xlib:send-event xwin
                         event
                         (xlib:make-event-mask event)
                         :display *display*
                         :root (screen-root (window-screen win))
                         ;; Apparently we need these in here, though they
                         ;; make no sense for a key event.
                         :x 0 :y 0 :root-x 0 :root-y 0
                         :window xwin
                         :event-window xwin
                         :code code
                         :state state)))))

(defun send-fake-click (win button)
  "Send a fake click (button press + button release) to win."
  (cond
    #+clx-ext-test
    ((xlib:query-extension *display* "XTEST")
     (xtest:fake-button-event *display* button t)
     (xtest:fake-button-event *display* button nil))
    (t
     (multiple-value-bind (x y) (xlib:query-pointer (window-xwin win))
       (multiple-value-bind (rx ry) (xlib:query-pointer (screen-root (window-screen win)))
         (xlib:send-event (window-xwin win) :button-press (xlib:make-event-mask :button-press)
                          :display *display*
                          :root (screen-root (window-screen win))
                          :window (window-xwin win) :event-window (window-xwin win)
                          :code button
                          :state 0
                          :x x :y y :root-x rx :root-y ry
                          :same-screen-p t)
         (xlib:send-event (window-xwin win) :button-release (xlib:make-event-mask :button-release)
                          :display *display*
                          :root (screen-root (window-screen win))
                          :window (window-xwin win) :event-window (window-xwin win)
                          :code button
                          :state #x100
                          :x x :y y :root-x rx :root-y ry
                          :same-screen-p t))))))


;;; Pointer helper functions

(defun grab-pointer (screen)
  "Grab the pointer and set the pointer shape."
  (incf *grab-pointer-count*)
  (let* ((cursor-font (xlib:open-font *display* *grab-pointer-font*))
         (cursor (xlib:create-glyph-cursor :source-font cursor-font
                                           :source-char *grab-pointer-character*
                                           :mask-font cursor-font
                                           :mask-char *grab-pointer-character-mask*
                                           :foreground *grab-pointer-foreground*
                                           :background *grab-pointer-background*)))
    (xlib:grab-pointer (screen-root screen) nil :owner-p nil
                       :cursor cursor)))

(defun ungrab-pointer ()
  "Remove the grab on the cursor and restore the cursor shape."
  (when (> *grab-pointer-count* 0) (decf *grab-pointer-count*))
  (when (eq *grab-pointer-count* 0)
    (xlib:ungrab-pointer *display*)
    (xlib:display-finish-output *display*)))

(defun grab-keyboard (xwin)
  (let ((ret (xlib:grab-keyboard xwin :owner-p nil
                                 :sync-keyboard-p nil :sync-pointer-p nil)))
    (dformat 5 "vvv Grab keyboard: ~s~%" ret)
    ret))

(defun ungrab-keyboard ()
  (let ((ret (xlib:ungrab-keyboard *display*)))
    (dformat 5 "^^^ Ungrab keyboard: ~s~%" ret)
    ret))

(defun warp-pointer (screen x y)
  "Move the pointer to the specified location."
  (let ((root (screen-root screen)))
    (xlib:warp-pointer root x y)))

(defun warp-pointer-relative (dx dy)
  "Move the pointer by DX and DY relative to the current location."
  (xlib:warp-pointer-relative *display* dx dy))

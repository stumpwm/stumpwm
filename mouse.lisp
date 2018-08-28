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
;; Functions relating to mouse policies.
;;
;; Code:
(in-package :stumpwm)

(defun temporarilly-disable-sloppy-focus ()
  "Disable the sloppy pointer for a brief period of time."
  (setf *sloppy-mouse-focus-ignored* t)
  (run-with-timer 0.1 nil #'reenable-sloppy-focus))

(defun reenable-sloppy-focus ()
  "Remove disable flag on sloppy focus policy."
  (setf *sloppy-mouse-focus-ignored* nil))

;; A few helpers for dealing with window and frame calculations
(defgeneric mouse-get-x (window)
  (:documentation "Generic get x position"))
(defgeneric mouse-get-y (window)
  (:documentation "Generic get y position"))
(defgeneric mouse-get-width (window)
  (:documentation "Generic get width"))
(defgeneric mouse-get-height (window)
  (:documentation "Generic get height position"))

(defmethod mouse-get-x ((window xlib:window))
  (xlib:drawable-x window))
(defmethod mouse-get-y ((window xlib:window))
  (xlib:drawable-y window))
(defmethod mouse-get-width ((window xlib:window))
  (xlib:drawable-width window))
(defmethod mouse-get-height ((window xlib:window))
  (xlib:drawable-height window))

(defmethod mouse-get-x ((frame stumpwm::frame))
  (frame-x frame))
(defmethod mouse-get-y ((frame stumpwm::frame))
  (frame-y frame))
(defmethod mouse-get-width ((frame stumpwm::frame))
  (frame-width frame))
(defmethod mouse-get-height ((frame stumpwm::frame))
  (frame-height frame))

(defmethod mouse-get-x ((window stumpwm::window))
  (mouse-get-x (window-parent window)))
(defmethod mouse-get-y ((window stumpwm::window))
  (mouse-get-y (window-parent window)))
(defmethod mouse-get-width ((window stumpwm::window))
  (mouse-get-width (window-parent window)))
(defmethod mouse-get-height ((window stumpwm::window))
  (mouse-get-height (window-parent window)))

(defun mouse-inside-frame-p (frame)
  "Determine if mouse already inside frame."
  (multiple-value-bind (mouse-x mouse-y)
      (xlib:global-pointer-position  *display*)
    (let* ((group (current-group))
           (min-x (mouse-get-x frame))
           (min-y (stumpwm::frame-display-y group frame))
           (max-x (+ min-x (mouse-get-width frame)))
           (max-y (+ min-y (stumpwm::frame-display-height group frame))))
      (and (<= min-x mouse-x max-x)
           (<= min-y mouse-y max-y)))))

(defun mouse-banish-frame (frame)
  "Banish mouse to corner of frame"
  (let* ((group (current-group))
         (x-offset 15)
         (y-offset 15)
         (win-x (mouse-get-x frame))
         (win-y (stumpwm::frame-display-y group frame))
         (w (mouse-get-width frame))
         (h (stumpwm::frame-display-height group frame))
         (x (- (+ win-x w)
               x-offset))
         (y (- (+ win-y h)
               y-offset)))
    (temporarilly-disable-sloppy-focus)
    (ratwarp x y)))

(defgeneric mouse-inside-window-p (window)
  (:documentation "Determine if mouse already inside window."))

(defmethod mouse-inside-window-p ((window stumpwm::float-window))
  (multiple-value-bind (mouse-x mouse-y)
      (xlib:global-pointer-position *display*)
    (let* ((offset 2)
           (x (mouse-get-x window))
           (w (mouse-get-width window))
           (min-x (- x *float-window-border* offset))
           (max-x (+ x w *float-window-border* offset))
           (y (mouse-get-y window))
           (h (mouse-get-height window))
           (min-y (- y *float-window-title-height* offset))
           (max-y (+ y h *float-window-border* offset)))
      (and (<= min-x mouse-x max-x)
           (<= min-y mouse-y max-y)))))

(defmethod mouse-inside-window-p ((window stumpwm::tile-window))
  (let ((frame (window-frame window)))
    (mouse-inside-frame-p frame)))

(defgeneric mouse-banish-window (window)
  (:documentation "Generic banish window"))

(defmethod mouse-banish-window ((window stumpwm::tile-window))
  (let ((frame (window-frame window)))
    (mouse-banish-frame frame)))

(defmethod mouse-banish-window ((window stumpwm::float-window))
  "Banish mouse to corner of a window"
  (let* ((x-offset 15)
         (y-offset 15)
         (win-x (mouse-get-x window))
         (win-y (mouse-get-y window))
         (w (mouse-get-width window))
         (h (mouse-get-height window))
         (x (- (+ win-x w)
               x-offset))
         (y (- (+ win-y h)
               y-offset)))
    (temporarilly-disable-sloppy-focus)
    (ratwarp x y)))

(defun mouse-handle-focus-frame (current-frame last-frame)
  "Move mouse when moving frames."
  (when (eq *mouse-follow-policy* :follow)
    (let ((current-window (stumpwm::frame-window current-frame)))
      (cond (current-window
             (unless (mouse-inside-window-p current-window)
               (mouse-banish-window current-window)))
            (t
             (unless (eq current-frame last-frame)
               (unless (mouse-inside-frame-p current-frame)
                 (mouse-banish-frame current-frame))))))))

(defun mouse-handle-split-frame (first-frame)
  "Reposition the mouse when a frame is created."
  (when (eq *mouse-follow-policy* :follow)
    (mouse-banish-frame first-frame)))

(defun mouse-handle-remove-split (current-frame)
  "Reposition the mouse when a frame is removed."
  (when (eq *mouse-follow-policy* :follow)
    (mouse-banish-frame current-frame)))

(defun mouse-handle-focus-window (current-window last-window)
  "Move mouse for floating windows."
  (when (eq *mouse-follow-policy* :follow)
    (unless (or (eq current-window last-window)
                (mouse-inside-window-p current-window))
      (mouse-banish-window current-window))))

(defun mouse-handle-focus-group (group)
  "Disable sloppy pointer when switching groups to prevent floating windows from
getting stuck and banish to last window or frame."
  (when (eq *mouse-follow-policy* :follow)
    (temporarilly-disable-sloppy-focus)
    (let ((window (current-window)))
      (if window
          (mouse-banish-window window)
          (mouse-banish-frame (tile-group-current-frame group))))))

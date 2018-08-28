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

(defun mouse-inside-frame-p (frame)
  "Determine if mouse already inside frame."
  (multiple-value-bind (mouse-x mouse-y)
      (xlib:global-pointer-position  *display*)
    (let* ((group (current-group))
           (min-x (frame-x frame))
           (min-y (frame-display-y group frame))
           (max-x (+ min-x (frame-width frame)))
           (max-y (+ min-y (frame-display-height group frame))))
      (and (<= min-x mouse-x max-x)
           (<= min-y mouse-y max-y)))))

(defun mouse-banish-frame (frame)
  "Banish mouse to corner of frame"
  (let* ((group (current-group))
         (x-offset 15)
         (y-offset 15)
         (win-x (frame-x frame))
         (win-y (frame-display-y group frame))
         (w (frame-width frame))
         (h (frame-display-height group frame))
         (x (- (+ win-x w)
               x-offset))
         (y (- (+ win-y h)
               y-offset)))
    (temporarilly-disable-sloppy-focus)
    (ratwarp x y)))

(defgeneric mouse-inside-window-p (window)
  (:documentation "Determine if mouse already inside window.")
  (:method ((window float-window))
    (multiple-value-bind (mouse-x mouse-y)
        (xlib:global-pointer-position *display*)
      (let* ((leniency-offset 2)
             (x (xlib:drawable-x (window-parent window)))
             (w (xlib:drawable-width (window-parent window)))
             (min-x (- x *float-window-border* leniency-offset))
             (max-x (+ x w *float-window-border* leniency-offset))
             (y (xlib:drawable-y (window-parent window)))
             (h (xlib:drawable-height (window-parent window)))
             (min-y (- y *float-window-title-height* leniency-offset))
             (max-y (+ y h *float-window-border* leniency-offset)))
        (and (<= min-x mouse-x max-x)
             (<= min-y mouse-y max-y)))))
  (:method ((window tile-window))
    (let ((frame (window-frame window)))
      (mouse-inside-frame-p frame))))

(defgeneric mouse-banish-window (window)
  (:documentation "Generic banish window")
  (:method ((window float-window))
    (let* ((x-offset 15)
           (y-offset 15)
           (win-x (xlib:drawable-x (window-parent window)))
           (win-y (xlib:drawable-y (window-parent window)))
           (w (xlib:drawable-width (window-parent window)))
           (h (xlib:drawable-height (window-parent window)))
           (x (- (+ win-x w)
                 x-offset))
           (y (- (+ win-y h)
                 y-offset)))
      (temporarilly-disable-sloppy-focus)
      (ratwarp x y)))
  (:method ((window tile-window))
    (let ((frame (window-frame window)))
      (mouse-banish-frame frame))))

(defun mouse-handle-focus-frame (current-frame last-frame)
  "Move mouse when moving frames."
  (when (eq *mouse-follow-policy* :follow)
    (let ((current-window (frame-window current-frame)))
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

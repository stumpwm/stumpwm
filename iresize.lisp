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
;;; A resize minor mode. Something a bit better should probably be
;;; written. But it's an interesting way of doing it.
;;
;; Code:

(in-package #:stumpwm)

(export '(*resize-increment*
          iresize
          setup-iresize))

(defvar *resize-increment* 10
  "Number of pixels to increment by when interactively resizing frames.")

(defun set-resize-increment (val)
  (setf *resize-increment* val))

(defun single-frame-p ()
  "Checks if there's only one frame."
  (let ((frame (tile-group-current-frame (current-group))))
    (atom (tile-group-frame-head (current-group)
                                 (frame-head (current-group)
                                             frame)))))

(defun abort-resize-p ()
  "Resize is only available if there's more than one frame."
  (when (single-frame-p)
    (message "There's only 1 frame!")
    t))

(defun setup-iresize ()
  "Start the interactive resize mode."
  (when *resize-hides-windows*
    (dolist (f (head-frames (current-group) (current-head)))
      (clear-frame f (current-group))))
  (draw-frame-outlines (current-group) (current-head)))

(defcommand resize-direction (d)
  ((:direction "Direction: "))
  "Resize frame to direction @var{d}"
  (case (princ d)
    ((:up) (resize 0 (- *resize-increment*)))
    ((:down) (resize 0 *resize-increment*))
    ((:left) (resize (- *resize-increment*) 0))
    ((:right) (resize *resize-increment* 0))))

(defun resize-unhide ()
  (clear-frame-outlines (current-group))
  (when *resize-hides-windows*
    (let ((group (current-group))
          (head (current-head)))
      (dolist (f (head-frames group head))
        (sync-frame-windows group f))
      (dolist (w (reverse (head-windows group head)))
        (setf (frame-window (window-frame w)) w)
        (raise-window w))
      (when (current-window)
        (focus-window (current-window))))))

(define-interactive-keymap (iresize tile-group) (:on-enter #'setup-iresize
                                                 :on-exit #'resize-unhide
                                                 :abort-if #'abort-resize-p)

  ((kbd "Up") "resize-direction up")
  ((kbd "C-p") "resize-direction up")
  ((kbd "p") "resize-direction up")
  ((kbd "k") "resize-direction up")

  ((kbd "Down") "resize-direction down")
  ((kbd "C-n") "resize-direction down")
  ((kbd "n") "resize-direction down")
  ((kbd "j") "resize-direction down")

  ((kbd "Left") "resize-direction left")
  ((kbd "C-b") "resize-direction left")
  ((kbd "b") "resize-direction left")
  ((kbd "h") "resize-direction left")

  ((kbd "Right") "resize-direction right")
  ((kbd "C-f") "resize-direction right")
  ((kbd "f") "resize-direction right")
  ((kbd "l") "resize-direction right"))

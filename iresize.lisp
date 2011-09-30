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
          abort-iresize
          exit-iresize))

(defvar *resize-backup* nil)

(defvar *resize-increment* 10
  "Number of pixels to increment by when interactively resizing frames.")

(defun set-resize-increment (val)
  (setf *resize-increment* val)
  (update-resize-map))

(defun update-resize-map ()
  (let ((m (or *resize-map* (setf *resize-map* (make-sparse-keymap)))))
    (let ((i *resize-increment*))
      (labels ((dk (m k c)
                 (define-key m k (format nil c i))))
        (dk m (kbd "Up") "resize 0 -~D")
        (dk m (kbd "C-p") "resize 0 -~D")
        (dk m (kbd "p") "resize 0 -~D")
        (dk m (kbd "k") "resize 0 -~D")

        (dk m (kbd "Down") "resize 0 ~D")
        (dk m (kbd "C-n") "resize 0 ~D")
        (dk m (kbd "n") "resize 0 ~D")
        (dk m (kbd "j") "resize 0 ~D")

        (dk m (kbd "Left") "resize -~D 0")
        (dk m (kbd "C-b") "resize -~D 0")
        (dk m (kbd "b") "resize -~D 0")
        (dk m (kbd "h") "resize -~D 0")

        (dk m (kbd "Right") "resize ~D 0")
        (dk m (kbd "C-f") "resize ~D 0")
        (dk m (kbd "f") "resize ~D 0")
        (dk m (kbd "l") "resize ~D 0")
        (define-key m (kbd "RET") "exit-iresize")
        (define-key m (kbd "C-g") "abort-iresize")
        (define-key m (kbd "ESC") "abort-iresize")))))

(update-resize-map)

(defcommand (iresize tile-group) () ()
  "Start the interactive resize mode. A new keymap specific to
resizing the current frame is loaded. Hit @key{C-g}, @key{RET}, or
@key{ESC} to exit."
  (let ((frame (tile-group-current-frame (current-group))))
    (if (atom (tile-group-frame-head (current-group) (frame-head (current-group) frame)))
        (message "There's only 1 frame!")
        (progn
          (when *resize-hides-windows*
            (dolist (f (head-frames (current-group) (current-head)))
              (clear-frame f (current-group))))
          (message "Resize Frame")
          (push-top-map *resize-map*)
          (draw-frame-outlines (current-group) (current-head)))
        ;;   (setf *resize-backup* (copy-frame-tree (current-group)))
        )))

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

(defcommand (abort-iresize tile-group) () ()
  "Exit from the interactive resize mode."
  (resize-unhide)
  (message "Abort resize")
  ;; TODO: actually revert the frames
  (pop-top-map))

(defcommand (exit-iresize tile-group) () ()
  "Exit from the interactive resize mode."
  (resize-unhide)
  (message "Resize Complete")
  (pop-top-map))

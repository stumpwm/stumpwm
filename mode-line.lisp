;; Copyright (C) 2006 Shawn Betts
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
;; a modeline that can enabled per screen and per frame (todo).
;;
;; Code:

(in-package "STUMPWM")

(defstruct mode-line
  window
  format
  gc)

(defvar *mode-line-screen-position* :top
  "Where should the mode line be displayed? :top or :bottom.")

(defvar *mode-line-frame-position* :top
  "Where should the mode line be displayed? :top or :bottom.")

(defvar *screen-mode-line-formatters* '((#\w fmt-screen-window-list))
  "An alist containing format character format function pairs for formatting screen mode-lines.")

(defvar *current-mode-line-formatters* nil
  "used in formatting modeline strings.")

(defvar *current-mode-line-formatter-args* nil
  "used in formatting modeline strings.")

(defun make-mode-line-window (parent screen)
  "Create a window suitable for a modeline."
  (xlib:create-window
   :parent parent
   :x 0 :y 0 :width 1 :height 1
   :background (get-bg-color-pixel screen)
   ;; You can click the modeline
   :event-mask (xlib:make-event-mask :button-press)
   ;; these windows are not controlled by the window manager
   :override-redirect :on))

(defgeneric mode-line-format-elt (elt))

(defmethod mode-line-format-elt ((elt string))
  (apply 'format-expand *current-mode-line-formatters* elt 
	 *current-mode-line-formatter-args*))

(defmethod mode-line-format-elt ((elt symbol))
  (format nil "~a" (symbol-value elt)))

(defmethod mode-line-format-elt ((elt list))
  (format nil "~a"
	  (ecase (first elt)
	    (:eval (eval (second elt))))))

(defun mode-line-format-string (ml)
  (apply 'concatenate 'string
	 (mapcar 'mode-line-format-elt
		 (mode-line-format ml))))

(defun make-mode-line-gc (window screen)
  (xlib:create-gcontext :drawable window
			:font (screen-font screen)
			:foreground (get-fg-color-pixel screen)
			:background (get-bg-color-pixel screen)))

(defun make-screen-mode-line (screen)
  (let ((w (make-mode-line-window (screen-root screen) screen)))
    (xlib:map-window w)
    (make-mode-line :window w
		    :format nil
		    :gc (make-mode-line-gc w screen))))

(defgeneric redraw-mode-line-for (ml thing)
  (:documentation "Resize and redraw the modeline for a screen, frame, ...other?"))

(defmethod redraw-mode-line-for (ml (obj screen))
  ;; first resize
  (setf (xlib:drawable-x (mode-line-window ml)) 0
	(xlib:drawable-y (mode-line-window ml)) 0
	(xlib:drawable-width (mode-line-window ml)) (screen-width obj)
	(xlib:drawable-height (mode-line-window ml)) (font-height (xlib:gcontext-font (mode-line-gc ml))))
  ;; then redraw
  (xlib:clear-area (mode-line-window ml))
  (xlib:draw-image-glyphs (mode-line-window ml) (mode-line-gc ml) 0 (xlib:font-ascent (xlib:gcontext-font (mode-line-gc ml)))
			  (let ((*current-mode-line-formatters* *screen-mode-line-formatters*)
				(*current-mode-line-formatter-args* (list obj)))
			    (mode-line-format-string ml))))

;; FIXME: the format string is juts a cheap hack for now. obviously
;; the formatter can handle more than just strings.
(define-stumpwm-command "mode-line" (screen (fmt :string "Format: "))
  (let ((create-p (null (screen-mode-line screen))))
    (when create-p
      (setf (screen-mode-line screen) (make-screen-mode-line screen)))
    ;; update the format string
    (setf (mode-line-format (screen-mode-line screen)) (list fmt))
    (redraw-mode-line-for (screen-mode-line screen) screen)
    ;; update the frames if need be
    (when create-p
      (offset-frames screen 0 (xlib:drawable-height (mode-line-window (screen-mode-line screen))))
      (grow-frames screen 0 (- (xlib:drawable-height (mode-line-window (screen-mode-line screen)))))
      (sync-all-frame-windows screen))))

;; Copyright (C) 2007 Jonathan Moore Liles
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
;; This simplified implementation of the the C color code is as follows:
;;
;; ^B bright
;; ^b dim
;; ^n normal (sgr0)
;;
;; ^00 black black
;; ^10 red black
;; ^01 black red
;; ^1* red clear
;;
;; and so on.
;;
;; I won't explain here the many reasons that C is better than ANSI, so just
;; take my word for it.

(in-package :stumpwm)

(export '(*colors* update-color-map))

;; Eight colors. You can redefine these to whatever you like (and
;; then call (update-color-map)).
(defvar *colors*
  '("black"
    "red"
    "green"
    "yellow"
    "blue"
    "magenta"
    "cyan"
    "white"))

;; Normal colors are dimmed and bright colors are intensified in order
;; to more closely resemble the VGA pallet.
(defun update-color-map (screen)
  (let ((scm (xlib:screen-default-colormap (screen-number screen))))
    (labels ((max-min (x y) (max 0 (min 1 (+ x y))))
	     (adj-color (color amt)
			(setf (xlib:color-red color) (max-min (xlib:color-red color) amt)
			      (xlib:color-green color) (max-min (xlib:color-green color) amt)
			      (xlib:color-blue color) (max-min (xlib:color-blue color) amt)))
	     (map-colors (amt)
			 (loop for c in *colors*
			       as color = (xlib:lookup-color scm c)
			       do (adj-color color amt)
			       collect (xlib:alloc-color scm color))))
      (setf (color-map-norm (screen-colors screen)) (map-colors -0.25)
	    (color-map-bright (screen-colors screen)) (map-colors 0.25)
	    (color-map-current (screen-colors screen)) (color-map-norm (screen-colors screen))))))

(defun get-bg-color (screen color)
  (setf (color-bg (screen-colors screen)) color)
  (if color
    (elt (color-map-norm (screen-colors screen)) color)
    (get-bg-color-pixel screen)))

(defun get-fg-color (screen color)
  (setf (color-fg (screen-colors screen)) color)
  (if color
    (elt (color-map-current (screen-colors screen)) color)
    (get-fg-color-pixel screen)))

(defun set-color (screen gc s i)
  (let* ((l (- (length s) i))
	 (r 2)
	 (f (subseq s i (1+ i)))
	 (b (if (< l 2) "*" (subseq s (1+ i) (+ i 2)))))
    (labels ((update-colors ()
			    (setf
			      (xlib:gcontext-foreground gc) (get-fg-color screen (color-fg (screen-colors screen)))
			      (xlib:gcontext-background gc) (get-bg-color screen (color-bg (screen-colors screen))))))
      (case (elt f 0)
	(#\n ; normal
	 (setf f "*" b "*" r 1
	       (color-map-current (screen-colors screen)) (color-map-norm (screen-colors screen)))
	 (get-fg-color screen nil)
	 (get-bg-color screen nil))
	(#\b ; bright off
	 (setf (color-map-current (screen-colors screen)) (color-map-norm (screen-colors screen)))
	 (update-colors)
	 (return-from set-color 1))
	(#\B ; bright on
	 (setf (color-map-current (screen-colors screen)) (color-map-bright (screen-colors screen)))
	 (update-colors)
	 (return-from set-color 1))
	(#\^ ; circumflex
	 (return-from set-color 1)))
      (handler-case 
	(let ((fg (if (equal f "*") (progn (get-fg-color screen nil) (get-fg-color-pixel screen)) (get-fg-color screen (parse-integer f))))
	      (bg (if (equal b "*") (progn (get-bg-color screen nil) (get-bg-color-pixel screen)) (get-bg-color screen (parse-integer b)))))
	  (setf (xlib:gcontext-foreground gc) fg
		(xlib:gcontext-background gc) bg))
	(error (c) (dformat 1 "Invalid color code: ~A" c)))) r))

(defun render-strings (screen strings highlights &optional (draw t))
  (let* ((height (+ (xlib:font-descent (screen-font screen))
		    (xlib:font-ascent (screen-font screen))))
	 (width 0)
	 (gcontext (screen-message-gc screen))
	 (message-win (screen-message-window screen)))
    (set-color screen gcontext "n" 0)
    (loop for s in strings
	  ;; We need this so we can track the row for each element
	  for i from 0 to (length strings)
	  do (let ((x 0) (off 0))
	       (loop
		 for st = 0 then (+ en (1+ off))
		 as en = (position #\^ s :start st)
		 do (progn
		      (let ((en (if (and en (eq #\^ (elt s (1+ en)))) (1+ en) en)))
			(when draw
			  (xlib:draw-image-glyphs message-win gcontext
						  (+ *message-window-padding* x)
						  (+ (* i height)
						     (xlib:font-ascent (screen-font screen)))
						  (subseq s st en)
						  :translate #'translate-id
						  :size 16))
			(setf x (+ x (xlib:text-width (screen-font screen) (subseq s st en) :translate #'translate-id))))
		      (when en
			(setf off (set-color screen gcontext s (1+ en))))
		      (setf width (max width x)))
		 while en))
	  when (find i highlights :test 'eql)
	  do (when draw (invert-rect screen message-win
				     0 (* i height)
				     (xlib:drawable-width message-win)
				     height)))
    width))


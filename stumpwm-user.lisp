;; Copyright (C) 2003 Shawn Betts
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
;; Window Manager commands that users can use to manipulate stumpwm
;; and write custos.
;;
;; Code:

(in-package #:stumpwm)

(defun set-key (keysym fn)
  "Bind keysym to the function FN."
  (push (cons keysym fn) *key-binding-alist*))

(defun focus-next-window (screen)
  (focus-forward screen (sort-windows screen)))

(defun focus-prev-window (screen)
  (focus-forward screen (reverse (sort-windows screen))))

;; In the future, this window will raise the window into the current
;; frame.
(defun focus-forward (screen window-list)
 "Set the focus to the next item in window-list from the focused window."
  ;; The window with focus is the "current" window, so find it in the
  ;; list and give that window focus
  (let* ((w (xlib:input-focus *display*))
	 (wins (member w window-list))
	 nw)
    ;; This is a catch in case something *BAD* happened. The focused
    ;; window *SHOULD* be in the mapped window list.
    (print w)
    (print window-list)
    (print wins)
    ;;(assert wins)
    (setf nw (if (null (cdr wins))
		 ;; If the last window in the list is focused, then
		 ;; focus the first one.
		 (car window-list)
	       ;; Otherwise, focus the next one in the list.
	       (cadr wins)))
    (focus-window nw)))

(defun delete-current-window (screen)
  "Send a delete event to the current window."
  (when (screen-current-window screen)
    (delete-window (screen-current-window screen))))

(defun banish-pointer (screen)
  "Move the pointer to the lower right corner of the screen"
  (warp-pointer screen
		(1- (screen-width screen))
		(1- (screen-height screen))))

(defun echo-windows (screen)
  "Print a list of the windows to the screen."
  (echo-window-list screen (sort-windows screen)))

(defun select-window (screen)
  "Read input from the user and go to the selected window."
    (let ((query (read-one-line screen))
	  match)
      (labels ((match (win)
		      (let* ((wname (window-name win))
			     (end (min (length wname) (length query))))
			(string-equal wname query :end1 end :end2 end))))
	(unless (null query)
	  (setf match (find-if #'match (screen-mapped-windows screen))))
	(when match
	  (focus-window match)))))

(defun select-window-number (screen num)
  (labels ((match (win)
		  (= (window-number screen win) num)))
    (setf match (find-if #'match (screen-mapped-windows screen)))
    (when match
      (focus-window match))))

(defun other-window (screen)
  (when (second (screen-mapped-windows screen))
    (focus-window (second (screen-mapped-windows screen)))))

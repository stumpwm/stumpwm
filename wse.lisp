;; Copyright 2011 Michael Raskin
;;
;; Maintainer: Michael Raskin
;;
;; This file is part of stumpwm.
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

;; Window Selection Expressions

(in-package :stumpwm)

(defmacro act-on-matching-windows 
  ((var &optional (range '(current-screen))) condition &rest code)
  "Run code on all windows matching condition; var is the shared lambda 
  variable"
  `(let
     ((range ,range))
     (loop for ,var in 
	   (cond
	     ((typep range 'screen) (screen-windows range))
	     ((typep range 'group) (group-windows range))
	     ((typep range 'frame) (frame-windows (current-group) range))
	     ((typep range 'list) range)
	     ((eq range :screen) (screen-windows (current-screen)))
	     ((eq range :group) 
	      (group-windows (current-group)))
	     ((eq range :frame)
	      (frame-windows (current-group)
			     (tile-group-current-frame 
			       (current-group))))
	     (t (error "Unknown kind of window set")))
	   when ,condition
	   collect (progn ,@code))))

(defun pull-w (w) (move-windows-to-group (list w) (current-group)))
(defun titled-p (w title) (equal (window-title w) title))
(defun title-re-p (w tre) (cl-ppcre:Scan tre (window-title w)))
(defun classed-p (w class) (equal (window-class w) class))
(defun class-re-p (w cre) (cl-ppcre:Scan cre (window-class w)))
(defun typed-p (w type) (equal (window-type w) type))
(defun type-re-p (w tre) (cl-ppcre:Scan tre (window-type w)))
(defun roled-p (w role) (equal (window-role w) role))
(defun role-re-p (w rre) (cl-ppcre:Scan rre (window-role w)))
(defun resed-p (w res) (equal (window-res w) res))
(defun res-re-p (w rre) (cl-ppcre:Scan rre (window-res w)))
(defun classed-p (w class) (equal (window-class w) class))
(defun class-re-p (w cre) (cl-ppcre:Scan cre (window-class w)))
(defun grouped-p (w name) (equal name (group-name (window-group w))))
(defun in-current-group-p (w) (equal (window-group w) (current-group)))
(defun in-frame-p (w f) (eq (window-frame w) f))
(defun in-current-frame-p (w)
  (equal (window-frame w) (tile-group-current-frame 
			    (current-group (current-screen)))))

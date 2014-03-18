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

(export '(move-windows-to-group act-on-matching-windows))

(defun move-windows-to-group (windows &optional (arggroup nil))
  "Move all windows from the list to the group"
  (let*
    ((group
       (if (stringp arggroup)
	 (or
	   (find-group (current-screen) arggroup)
	   (add-group (current-screen) arggroup))
	 (or arggroup (current-group)))))
    (mapcar (lambda (w) (move-window-to-group w group)) windows)))

(defgeneric
  list-windows (range)
  (:documentation "List all the windows in a set."))

(defmethod list-windows ((range t))
  (error "Unknown kind of window set"))

(defmethod list-windows ((range (eql :screen)))
  (list-windows (current-screen)))

(defmethod list-windows ((range (eql :group)))
  (list-windows (current-group)))

(defmethod list-windows ((range (eql :frame)))
  (list-windows (tile-group-current-frame (current-group))))

(defmethod list-windows ((range screen))
  (screen-windows range))

(defmethod list-windows ((range group))
  (group-windows range))

(defmethod list-windows ((range frame))
  (frame-windows (current-group) range))

(defmethod list-windows ((range list)) range)

(defmacro act-on-matching-windows
  ((var &optional (range '(current-screen))) condition &rest code)
  "Run code on all windows matching condition; var is the shared lambda
  variable. Range can be any screen/group/frame or :screen/:group/:frame
  for the current instance. Condition is just the code to evaluate."
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

(defun pull-w (w &optional g)
  "Pull the window w: to the current group or to the specified group g."
  (move-windows-to-group (list w) (or g (current-group))))
(defun titled-p (w title)
  "Check whether window title of the window w is equal to the string
  title."
  (equal (window-title w) title))
(defun title-re-p (w tre)
  "Check whether the window title of the window w matches the regular
  expression tre."
  (cl-ppcre:scan tre (window-title w)))
(defun classed-p (w class)
  "Check whether the window class of the window w is equal to the string
  class."
  (equal (window-class w) class))
(defun class-re-p (w cre)
  "Check whether the window class of the window w matches the regular
  expression cre."
  (cl-ppcre:scan cre (window-class w)))
(defun typed-p (w type)
  "Check whether the window type of the window w is equal to the string
  type."
  (equal (window-type w) type))
(defun type-re-p (w tre)
  "Check whether the window type of the window w matches the regular
  expression tre."
  (cl-ppcre:scan tre (window-type w)))
(defun roled-p (w role)
  "Check whether the window role of the window w is equal to the string
  role."
  (equal (window-role w) role))
(defun role-re-p (w rre)
  "Check whether the window role of the window w matches the regular
  expression rre."
  (cl-ppcre:scan rre (window-role w)))
(defun resed-p (w res)
  "Check whether the window resource of the window w is equal to the
  string res."
  (equal (window-res w) res))
(defun res-re-p (w rre)
  "Check whether the window resource of the window w matches the regular
  expression rre."
  (cl-ppcre:scan rre (window-res w)))
(defun grouped-p (w &optional name)
  "Check whether the window w belongs to the group name or the current
  group if name is not specified."
  (if name
    (equal name (group-name (window-group w)))
    (equal (window-group w) (current-group))))
(defun in-frame-p (w &optional f)
  "Check whether the window w belongs to the frame f or to the current
  frame if the frame is not specified."
  (eq (window-frame w)
      (or f (tile-group-current-frame
	      (current-group (current-screen))))))

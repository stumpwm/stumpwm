;;;; Copyright (C) 2016 Joram Schrijver
;;;;
;;;;  This file is part of stumpwm.
;;;;
;;;; stumpwm is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; stumpwm is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, see
;;;; <http://www.gnu.org/licenses/>.

(in-package :stumpui)

;;; Window protocol -----------------------------------------------------------

(defgeneric window-p (object)
  (:method (object) nil))

(defgeneric window-screen (window))
(defgeneric window-xwin (window))

(defgeneric window-event-mask (window)
  (:method-combination append))

(defgeneric window-required-size (window))

(defgeneric window-gravity (window))
(defgeneric (setf window-gravity) (gravity window))

(defgeneric window-show (window frame &key &allow-other-keys))
(defgeneric window-hide (window))
(defgeneric window-redraw (window))

;;; Text window protocol ------------------------------------------------------

(defgeneric text-window-p (object)
  (:method (object) nil))

(defgeneric text-window-ccontext (text-window))

(defgeneric text-window-text (text-window))
(defgeneric (setf text-window-text) (string/list text-window))

(defgeneric text-window-highlights (text-window))
(defgeneric (setf text-window-highlights) (list text-window))

;;; Style

(defgeneric text-window-padding (text-window))
(defgeneric (setf text-window-padding) (integer text-window))

(defgeneric (setf text-window-foreground-color) (color text-window))
(defgeneric (setf text-window-background-color) (color text-window))
(defgeneric (setf text-window-border-color) (color text-window))
(defgeneric (setf text-window-border-width) (width text-window))

;;; Window timeout protocol ---------------------------------------------------

(defgeneric window-supports-timeout-p (object)
  (:method (object) nil))

(defgeneric window-schedule-timeout (window seconds))
(defgeneric window-cancel-timeout (window))

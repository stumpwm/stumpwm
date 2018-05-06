;; Copyright (C) 2018 Shawn Betts
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
;; Interface declarations/implementations for interactive menus. See
;; menu-definitions.lisp for implementations of the generic methods
;; declared here.
;;
;; Code:

;;; interactive menu

(in-package #:stumpwm)

(defvar *menu-map* nil
  "The keymap used by the interactive menu.")

(defvar *single-menu-map* nil
  "The keymap used by single selection menus in addtion to *menu-map*")

(defclass menu ()
  ((table :initarg :table
          :accessor menu-table
          :documentation "The list that is displayed in the menu")
   (selected :initarg :selected
             :initform 0
             :accessor menu-selected)
   (prompt :initarg :prompt
           :accessor menu-prompt)
   (view-start :initarg :view-start
               :accessor menu-view-start
               :initform 0)
   (view-end :initarg :view-end
             :accessor menu-view-end
             :initform 0)
   ;; whatever is supplied supplements *menu-map*, it does not replace it.
   ;; probably not the best way to supply this argument:
   (keymap :initarg :additional-keymap
           :reader menu-keymap
           :initform nil
           :documentation "Keymap used for navigating the menu." ))
  (:documentation "Base class for holding the state of a menu"))

(defmethod initialize-instance :after ((m menu) &key initargs)
  (declare (ignore initargs))
  (with-slots (keymap) m
    (setf keymap (if keymap
                     (list keymap *menu-map*)
                     (list *menu-map*)))))

(defclass single-menu (menu)
  ((unfiltered-table :initarg :filtered-table
                   :initform nil
                   :accessor single-menu-unfiltered-table
                   :documentation "Holds the values that have been filtered based on
current-input and filter-pred")
   (filter-pred :initarg :filter-pred
                :initform (error "You must specify a filter predicate")
                :accessor single-menu-filter-pred)
   (current-input :initarg current-input
                  :initform (make-array 10 :element-type 'character
                                        :adjustable t :fill-pointer 0)
                  :accessor single-menu-current-input))
  (:documentation "Class used when selecting a single item in a menu. Allowss searching through the list."))

(defmethod initialize-instance :after ((m single-menu) &key initargs)
  (declare (ignore initargs))
  (with-slots (unfiltered-table table keymap) m
    (unless unfiltered-table
      (setf unfiltered-table table))
    (setf keymap (if keymap
                     (push *single-menu-map* keymap)
                     (list keymap)))))

(defgeneric menu-up (menu)
  (:documentation "Move menu cursor up"))

(defgeneric menu-down (menu)
  (:documentation "Move menu cursor down"))

(defgeneric menu-scroll-up (menu)
  (:documentation "Scroll the menu up"))

(defgeneric menu-scroll-down (menu)
  (:documentation "Scroll the menu down"))

(defgeneric menu-page-up (menu)
  (:documentation "Move a whole page down in the menu"))

(defgeneric menu-page-down (menu)
  (:documentation "Move a whole page up in the menu"))

(defgeneric menu-finish (menu)
  (:documentation "What to do when exiting the menu with results.
Must signal :menu-quit with the result."))

(defgeneric menu-abort (menu)
  (:documentation "What to do when exiting the menu without results.
Must signal :menu-quit with the result."))

;; here for single-menu
(defgeneric menu-backspace (menu)
  (:documentation "What happens when backspace is pressed in a menu"))

(defgeneric menu-promt-line (menu)
  (:documentation "Returns the prompt-line that should be displayed. If no
line is to be displayed, then return nil"))

(defgeneric typing-action (menu key-seq)
  (:documentation "Performs an action based on key-seq when key-seq is not in the
appropriate menu-map."))

(defgeneric get-menu-items (menu)
  (:documentation "Returns the items in the menu that should be displayed."))

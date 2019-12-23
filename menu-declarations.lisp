;; Copyright (C) 2018 Stuart Dilts
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

;; export all of the commands/variables that someone may want to change
;; the key bindings of, and those needed for constructing menus
(export '(*menu-map*
          *single-menu-map*
          *batch-menu-map*
          menu-entry
          menu-entry-display
          menu-entry-apply
          menu
          menu-abort
          menu-up
          menu-down
          menu-scroll-down
          menu-scroll-up
          menu-page-down
          menu-page-up
          menu-finish
          menu-backspace))

(defvar *menu-map* nil
  "The keymap used by the interactive menu.")

(defvar *single-menu-map* nil
  "The keymap used by single selection menus in addition to *menu-map*")

(defvar *batch-menu-map* nil
  "The keymap used by batch-menu menus in addition to *menu-map*")

(defclass menu-entry ()
  ((label :initarg :label
          :reader menu-entry-label)
   (icon :initarg :icon
         :initform #\Space
         :reader menu-entry-icon
         :documentation "An additional decorator for the entry")
   (data :initarg :data
         :reader menu-entry-data
         :documentation "Any additional object that is associated with the menu-entry"))
  (:documentation "Defines a menu entry"))

(defgeneric menu-entry-display (menu-entry)
  (:documentation "Generates a string suitable for displaying in a menu"))

(defgeneric menu-entry-apply (menu-entry function)
  (:documentation "Apply FUNCTION to the data portion of the menu entry."))

(defclass menu ()
  ((table :initarg :table
          :accessor menu-table
          :documentation "The list that is displayed in the menu")
   (selected :initarg :selected
             :initform 0
             :accessor menu-selected
             :documentation "The index of the selected item")
   (prompt :initarg :prompt
           :initform "Search?"
           :reader menu-prompt)
   (view-start :initarg :view-start
               :accessor menu-view-start
               :initform 0)
   (view-end :initarg :view-end
             :accessor menu-view-end
             :initform 0)
   (keymap :accessor menu-keymap
           :initform nil
           :documentation "Keymap used for navigating the menu." ))
  (:documentation "Base class for holding the state of a menu"))

(defmethod initialize-instance :after ((m menu) &key additional-keymap)
  (with-accessors ((keymap menu-keymap))
      m
    (setf keymap (if additional-keymap
                     (list additional-keymap *menu-map*)
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
                  :accessor single-menu-current-input
                  :documentation "The input field for the menu."))
  (:documentation "Class used when selecting a single item in a menu. Allows searching through the list."))

(defmethod initialize-instance :after ((menu single-menu) &key initargs)
  (declare (ignore initargs))
  (with-accessors ((unfiltered-table single-menu-unfiltered-table)
                   (table menu-table)
                   (keymap menu-keymap))
      menu
    (unless unfiltered-table
      (setf unfiltered-table table))
    (push *single-menu-map* keymap)))

(defclass batch-menu (menu)
  ((allowed-markers :initarg :allowed-markers
                    :reader batch-menu-allowed-markers
                    :initform nil
                    :documentation "The characters that a user is allowed to mark entries with.
If nil, then all chars are allowed"))
  (:documentation "Class used for marking items in a menu"))

(defmethod initialize-instance :after ((m batch-menu) &key initargs)
  (declare (ignore initargs))
  (with-accessors ((table menu-table)
                   (keymap menu-keymap))
      m
    ;; process the table to hold selection values:
    ;; tables is a list of pairs, with the first val the mark, the second the entry
    (labels ((process-entry (entry)
               (cons nil entry)))
      (setf table (mapcar #'process-entry table)))
    (push *batch-menu-map* keymap)))

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
  (:documentation "What occurs when backspace is pressed in a menu"))


(defgeneric menu-prompt-line (menu)
  (:documentation "Returns the prompt-line that should be displayed. If no
line is to be displayed, then return nil"))

(defgeneric typing-action (menu key-seq)
  (:documentation "Performs an action based on key-seq when key-seq is not in the
appropriate menu-map."))

(defgeneric get-menu-items (menu)
  (:documentation "Returns the items in the menu that should be displayed."))

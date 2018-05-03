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
   (keymap :initarg :additional-keymap
           :reader menu-keymap
           :initform nil
           :documentation "Keymap used for navigating the menu." ))
  (:documentation "Base class for holding the state of a menu"))

(defmethod initialize-instance :after ((m menu) &rest initargs)
  (declare (ignore initargs))
  (with-slots (keymap) m
    (setf keymap (if keymap
                     (list keymap *menu-map*)
                     (list *menu-map*)))))

(defclass single-menu (menu)
  ((unfiltered-table :initarg unfiltered-table
                     :accessor single-menu-unfiltered-table)
   (filter-pred :initarg filter-pred
                :accessor single-menu-filter-pred)
   (current-input :initarg current-input
                  :initform (make-array 10 :element-type 'character
                                        :adjustable t :fill-pointer 0)
                  :accessor single-menu-current-input))
  (:documentation "State used when selecting a single item in a menu"))

(defmethod initialize-instance :after ((m single-menu) &rest initargs)
  (declare (ignore initargs))
  (with-slots (unfiltered-table table keymap) m
    (unless unfiltered-table
      (setf unfiltered-table table))
    (setf keymap (append keymap *single-menu-map*))))

;; should this really be a generic function?
;; (defgeneric menu-scrolling-required (menu)
;;   (:documentation "Checks if the selected value is at the start or end of the menu"))

(defgeneric menu-height (menu)
  (:documentation "Returns the height of the  menu"))

(defgeneric menu-prompt-visible (menu))

;; should this really be a generic function?
;; (defgeneric bound-check-menu (menu)
;;   (:documentation "Check if cursor has gone out of bounds and fix it."))

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

;; only here because menu-backspace might be defined in external code somewhere:
;; (defgeneric menu-backspace (menu)
;;   (:documentation "What happens when backspace is pressed in a menu"))

(defgeneric menu-promt-line (menu)
  (:documentation "Returns the prompt-line that should be displayed. If no
line is to be displayed, then return nil"))

(defgeneric typing-action (menu key-seq)
  (:documentation "Performs an action based on key-seq when key-seq is not in the
appropriate menu-map."))

(defgeneric get-menu-items (menu)
  (:documentation "Returns the items in the menu that should be displayed as a list"))
;; (defun menu-element-name (element)
;;   (if (listp element)
;;       (first element)
;;       element))

(defun menu-item-matches-regexp (item-string item-object user-input)
  "The default filter predicate for SELECT-FROM-MENU. When using this
predicate, an item is visible when it matches all of the regular
expressions in USER-INPUT (multiple regexps are separated by one or
more spaces; ARGUMENT-POP is used to split the string)."
  (declare (ignore item-object))
  (match-all-regexps user-input item-string))

(defun run-menu (screen menu)
  (declare (type menu menu))
                 ;(screen screen)))
  (bound-check-menu menu)
  (catch :menu-quit
    (unwind-protect
         (with-focus (screen-key-window screen)
           (loop
              (let* ((sel (menu-selected menu))
                     (start (menu-view-start menu))
                     (end (menu-view-end menu))
                     (len (length (menu-table menu)))
                     (prompt-line (menu-prompt-line menu))
                     (strings (get-menu-items menu))
                     (highlight (- sel start)))
                (unless (zerop start)
                  (setf strings (cons "..." strings))
                  (incf highlight))
                (unless (= len end)
                  (setf strings (nconc strings '("..."))))
                (when prompt-line
                  (push prompt-line strings)
                  (incf highlight))
                (run-hook-with-args *menu-selection-hook* menu)
                (echo-string-list screen strings highlight))
              (multiple-value-bind (action key-seq) (read-from-keymap (menu-keymap keymap))
                (if action
                    (progn (funcall action menu)
                           (bound-check-menu menu))
                    (typing-action menu (first key-seq))))))
      (unmap-all-message-windows))))


(defun select-from-menu (screen table &optional (prompt "Search:")
                                        (initial-selection 0)
                                        extra-keymap
                                        (filter-pred #'menu-item-matches-regexp))
  "Prompt the user to select from a menu on SCREEN. TABLE can be
a list of values or an alist. If it's an alist, the CAR of each
element is displayed in the menu. What is displayed as menu items
must be strings.
EXTRA-KEYMAP can be a keymap whose bindings will take precedence
over the default bindings.
FILTER-PRED should be a a function returning T when a certain menu
item should be visible to the user.  It should accept arguments
ITEM-STRING (the string shown to the user), ITEM-OBJECT (the object
corresponding to the menu item), and USER-INPUT (the current user
input). The default is MENU-ITEM-MATCHES-REGEXP.
Returns the selected element in TABLE or nil if aborted. "
  (check-type screen screen)
  (check-type table list)
  (check-type prompt (or null string))
  (check-type initial-selection integer)

  (when table
    (let ((menu (make-instance 'single-menu
                               :keymap extra-keymap
                               :unfiltered-table table
                               :table table
                               :filter-pred filter-pred
                               :prompt prompt
                               :view-start 0
                               :view-end 0
                               :selected initial-selection)))
      (run-menu screen menu))))

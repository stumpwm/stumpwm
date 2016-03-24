;; Copyright (C) 2008 Shawn Betts
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
;; Implementation of an interactive menu.
;;
;; Code:

;;; interactive menu

(in-package #:stumpwm)

(export '(select-from-menu
          menu-state-selected
          menu-state-table))

(defvar *menu-map* nil
  "The keymap used by the interactive menu.")

(when (null *menu-map*)
  (setf *menu-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "C-p") 'menu-up)
          (define-key m (kbd "Up") 'menu-up)
          (define-key m (kbd "S-Up") 'menu-scroll-up)
          (define-key m (kbd "SunPageUp") 'menu-page-up)

          (define-key m (kbd "C-n") 'menu-down)
          (define-key m (kbd "Down") 'menu-down)
          (define-key m (kbd "S-Down") 'menu-scroll-down)
          (define-key m (kbd "SunPageDown") 'menu-page-down)

          (define-key m (kbd "DEL") 'menu-backspace)

          (define-key m (kbd "C-g") 'menu-abort)
          (define-key m (kbd "ESC") 'menu-abort)
          (define-key m (kbd "RET") 'menu-finish)
          m)))

(defstruct menu-state
  unfiltered-table table filter-pred prompt selected view-start view-end
  (current-input (make-array 10 :element-type 'character :adjustable t :fill-pointer 0)))

(defun menu-scrolling-required (menu)
  (and *menu-maximum-height*
       (> (length (menu-state-table menu))
          *menu-maximum-height*)))

(defun menu-height (menu)
  (let ((len (length (menu-state-table menu))))
    (min (or *menu-maximum-height* len) len)))

(defun menu-prompt-visible (menu)
  (or (menu-state-prompt menu)
      (> (length (menu-state-current-input menu)) 0)))

(defun bound-check-menu (menu)
  "Adjust the menu view and selected item based
on current view and new selection."
  (let ((len (length (menu-state-table menu))))
    ;; Wrap around
    (setf (menu-state-selected menu)
          (cond ((< (menu-state-selected menu) 0) (1- len))
                ((>= (menu-state-selected menu) len) 0)
                (t (menu-state-selected menu))))
    (setf (values (menu-state-view-start menu)
                  (menu-state-view-end menu))
          (if (zerop len)
              (values 0 0)
              (let* ((menu-height (menu-height menu))
                     (sel (menu-state-selected menu))
                     (start (- sel 1))
                     (end (+ sel menu-height -1)))
                (multiple-value-bind
                      (start end) (cond ((< start 0) (values 0 menu-height))
                                        ((> end len) (values (- len menu-height) len))
                                        (t (values start end)))
                  (assert (<= 0 start (- len menu-height)) (start))
                  (assert (<= menu-height end len) (end))
                  (values start end)))))))

(defun menu-up (menu)
  (setf (fill-pointer (menu-state-current-input menu)) 0)
  (decf (menu-state-selected menu))
  (bound-check-menu menu))

(defun menu-down (menu)
  (setf (fill-pointer (menu-state-current-input menu)) 0)
  (incf (menu-state-selected menu))
  (bound-check-menu menu))

(defun menu-scroll-up (menu)
  (setf (fill-pointer (menu-state-current-input menu)) 0)
  (decf (menu-state-selected menu) *menu-scrolling-step*)
  (bound-check-menu menu))

(defun menu-scroll-down (menu)
  (setf (fill-pointer (menu-state-current-input menu)) 0)
  (incf (menu-state-selected menu) *menu-scrolling-step*)
  (bound-check-menu menu))

(defun menu-page-up (menu)
  (when *menu-maximum-height* ;;No scrolling = no page up/down
    (setf (fill-pointer (menu-state-current-input menu)) 0)
    (decf (menu-state-selected menu) *menu-maximum-height*)
    (let ((*menu-scrolling-step* *menu-maximum-height*))
      (bound-check-menu menu))))

(defun menu-page-down (menu)
  (when *menu-maximum-height*
    (setf (fill-pointer (menu-state-current-input menu)) 0)
    (incf (menu-state-selected menu) *menu-maximum-height*)
    (let ((*menu-scrolling-step* *menu-maximum-height*))
      (bound-check-menu menu))))


(defun menu-finish (menu)
  (throw :menu-quit (nth (menu-state-selected menu) (menu-state-table menu))))

(defun menu-abort (menu)
  (declare (ignore menu))
  (throw :menu-quit nil))

(defun get-input-char (key)
  "If @var{key} is a character suitable for menu completion (e.g. not
backspace or F9), return it otherwise return nil"
  (let ((char (xlib:keysym->character *display* (key-keysym key))))
    (if (or (key-mods-p key) (null char)
            (not (characterp char)))
        nil
        char)))

(defun menu-element-name (element)
  (if (listp element)
      (first element)
      element))

(defun menu-backspace (menu)
  (when (> (fill-pointer (menu-state-current-input menu)) 0)
    (vector-pop (menu-state-current-input menu))
    (check-menu-complete menu nil)))

(defun check-menu-complete (menu key-seq)
  "If the user entered a key not mapped in @var{*menu-map}, check if
  he's trying to type an entry's name. Match is case insensitive. If
  @var{key-seq} is nil, some other function has manipulated the
  current-input and is requesting a re-computation of the match."
  (let ((input-char (and key-seq (get-input-char key-seq))))
    (when input-char
      (vector-push-extend input-char (menu-state-current-input menu)))
    (handler-case
        (when (or input-char (not key-seq))
          (labels ((match-p (table-item)
                     (funcall (menu-state-filter-pred menu)
                              (car table-item)
                              (second table-item)
                              (menu-state-current-input menu))))
            (setf (menu-state-table menu) (remove-if-not #'match-p (menu-state-unfiltered-table menu))
                  (menu-state-selected menu) 0)
            (bound-check-menu menu)))
      (cl-ppcre:ppcre-syntax-error ()))))

(defun menu-item-matches-regexp (item-string item-object user-input)
  "The default filter predicate for SELECT-FROM-MENU. When using this
predicate, an item is visible when it matches all of the regular
expressions in USER-INPUT (multiple regexps are separated by one or
more spaces; ARGUMENT-POP is used to split the string)."
  (match-all-regexps user-input item-string))

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
    (let* ((*record-last-msg-override* t)
           (*suppress-echo-timeout* t)
           (menu (make-menu-state
                  :unfiltered-table table
                  :table table
                  :filter-pred filter-pred
                  :prompt prompt
                  :view-start 0
                  :view-end 0
                  :selected initial-selection))
           (keymap (if extra-keymap
                       (list extra-keymap *menu-map*)
                       (list *menu-map*))))
      (bound-check-menu menu)
      (catch :menu-quit
        (unwind-protect
             (with-focus (screen-key-window screen)
               (loop
                  (let* ((sel (menu-state-selected menu))
                         (start (menu-state-view-start menu))
                         (end (menu-state-view-end menu))
                         (len (length (menu-state-table menu)))
                         (prompt-line (when (menu-prompt-visible menu)
                                        (format nil "~@[~A ~]~A"
                                                prompt (menu-state-current-input menu))))
                         (strings (mapcar #'menu-element-name
                                          (subseq (menu-state-table menu)
                                                  start end)))
                         (highlight (- sel start)))
                    (unless (zerop start)
                      (setf strings (cons "..." strings))
                      (incf highlight))
                    (unless (= len end)
                      (setf strings (nconc strings '("..."))))
                    (when prompt-line
                      (push prompt-line strings)
                      (incf highlight))
                    (echo-string-list screen strings highlight))
                  (multiple-value-bind (action key-seq) (read-from-keymap keymap)
                    (if action
                        (progn (funcall action menu)
                               (bound-check-menu menu))
                        (check-menu-complete menu (first key-seq))))))
          (unmap-all-message-windows))))))

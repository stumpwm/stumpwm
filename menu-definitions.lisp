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
;; Implementation of an interactive menu. This file contains the definitions of the menu
;; class as defined in menu-declarations.lisp.
;;
;; Code:

;;; interactive menu

(in-package #:stumpwm)

(export '(entries-from-nested-list
          select-from-menu
          select-from-batch-menu
          command-menu))

(defun entries-from-nested-list (lst)
  (mapcar (lambda (x)
            (make-instance 'menu-entry
                           :label (car x)
                           :data (cadr x)))
          lst))

(defmethod menu-entry-display ((entry menu-entry))
  (concat (string (menu-entry-icon entry)) " " (menu-entry-label entry)))

(defmethod menu-entry-apply ((entry menu-entry) function)
  (if (slot-boundp entry 'data)
      (values (apply function (menu-entry-data entry)) t)
      (values nil nil)))


(defmethod print-object ((obj menu-entry) out)
  (print-unreadable-object (obj out :type t)
    (format out ":LABEL ~s :ICON ~s :DATA ~a"
            (menu-entry-label obj) (menu-entry-icon obj)
            (when (slot-boundp obj 'data)
              (menu-entry-data obj)))))


(defun menu-scrolling-required (menu)
  (and *menu-maximum-height*
       (> (length (menu-table menu))
          *menu-maximum-height*)))

(defun menu-height (menu)
  (let ((len (length (menu-table menu))))
    (min (or *menu-maximum-height* len) len)))

(defun bound-check-menu (menu)
  "Adjust the menu view and selected item based
on current view and new selection."
  (let ((len (length (menu-table menu))))
    ;; Wrap around
    (setf (menu-selected menu)
          (cond ((< (menu-selected menu) 0) (1- len))
                ((>= (menu-selected menu) len) 0)
                (t (menu-selected menu))))
    (setf (values (menu-view-start menu)
                  (menu-view-end menu))
          (if (zerop len)
              (values 0 0)
              (let* ((menu-height (menu-height menu))
                     (sel (menu-selected menu))
                     (start (- sel 1))
                     (end (+ sel menu-height -1)))
                (multiple-value-bind
                      (start end) (cond ((< start 0) (values 0 menu-height))
                                        ((> end len) (values (- len menu-height) len))
                                        (t (values start end)))
                  (assert (<= 0 start (- len menu-height)) (start))
                  (assert (<= menu-height end len) (end))
                  (values start end)))))))

(defmethod menu-up ((menu menu))
  (decf (menu-selected menu))
  (bound-check-menu menu))

;; before or after? probably doesn't matter
(defmethod menu-up :before ((menu single-menu))
  "clear the search string if the cursor is moved"
  (setf (fill-pointer (single-menu-current-input menu)) 0))

(defmethod menu-down ((menu menu))
  (incf (menu-selected menu))
  (bound-check-menu menu))

(defmethod menu-down :before ((menu single-menu))
  "clear the search string if the cursor is moved"
  (setf (fill-pointer (single-menu-current-input menu)) 0))

(defmethod menu-scroll-up ((menu menu))
  (decf (menu-selected menu) *menu-scrolling-step*)
  (bound-check-menu menu))

(defmethod menu-scroll-up :before ((menu single-menu))
  "clear the search string if the cursor is moved"
  (setf (fill-pointer (single-menu-current-input menu)) 0))

(defmethod menu-scroll-down ((menu menu))
  (incf (menu-selected menu) *menu-scrolling-step*)
  (bound-check-menu menu))

(defmethod menu-scroll-down :before ((menu single-menu))
  "clear the search string if the cursor is moved"
  (setf (fill-pointer (single-menu-current-input menu)) 0))

(defmethod menu-page-up ((menu menu))
  (when *menu-maximum-height* ;;No scrolling = no page up/down
    (decf (menu-selected menu) *menu-maximum-height*)
    (let ((*menu-scrolling-step* *menu-maximum-height*))
      (bound-check-menu menu))))

(defmethod menu-page-up :before ((menu single-menu))
  (when *menu-maximum-height* ;;No scrolling = no page up/down
    (setf (fill-pointer (single-menu-current-input menu)) 0)))

(defmethod menu-page-down ((menu menu))
  (when *menu-maximum-height*
    (incf (menu-selected menu) *menu-maximum-height*)
    (let ((*menu-scrolling-step* *menu-maximum-height*))
      (bound-check-menu menu))))

(defmethod menu-page-down :before ((menu single-menu))
  (when *menu-maximum-height*
    (setf (fill-pointer (single-menu-current-input menu)) 0)))

(defmethod menu-finish ((menu menu))
  (throw :menu-quit (nth (menu-selected menu) (menu-table menu))))

(defmethod menu-finish ((menu batch-menu))
  "Value returned with the signal is an alist, where the cdr of the value
returned by assoc is a list items that were marked with that character.
Example when entry1 and entry2 are marked with 'a', and entry3 is not marked:
    ((a entry1 entry2) (NIL entry3))"
  (with-slots (allowed-markers table) menu
    (let ((alist (list)))
      (dolist (entry table)
        (let ((mark (car entry))
              (value (cdr entry)))
          (if-let ((cell (assoc mark alist)))
            (push value (cdr cell))
            (setf alist (acons mark (list value) alist)))))
      (throw :menu-quit alist))))

(defmethod menu-abort (menu)
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

;; menu-backspace might be in someone's custom *menu-map*:
;; leave this here just to be safe:
(defmethod menu-backspace ((menu menu))
  (declare (ignore menu))
  "By default, do nothing")

(defun batch-menu-unmark-selected (menu)
  (with-accessors ((table menu-table)
                   (selected menu-selected))
      menu
    (setf (car (nth selected table)) nil)))

(defmethod menu-backspace ((menu batch-menu))
  "If the cursor is not at the top, move cursor up. Regardless,
unmark the entry at the selected point."
  (when (> (menu-selected menu) 0)
    (menu-up menu))
  (batch-menu-unmark-selected menu))

(defmethod menu-backspace ((menu single-menu))
  (when (> (fill-pointer (single-menu-current-input menu)) 0)
    (vector-pop (single-menu-current-input menu))
    (typing-action menu nil)))

(defmethod menu-prompt-line ((menu menu))
  "If there is a prompt, show it:"
  (menu-prompt menu))

(defun menu-prompt-visible (menu)
    (or (menu-prompt menu)
        (> (length (single-menu-current-input menu)) 0)))

(defmethod menu-prompt-line ((menu single-menu))
  "When a prompt is shown, also show the search string."
  (when (menu-prompt-visible menu)
      (format nil "~@[~A ~]~A"
              (menu-prompt menu) (single-menu-current-input menu))))

(defmethod typing-action ((menu menu) key-seq)
  "Default action is to do nothing"
  (declare (ignore key-seq)))

(defmethod typing-action ((menu single-menu) key-seq)
  "If the user entered a key not mapped in @var{*menu-map}, check if
  he's trying to type an entry's name. Match is case insensitive. If
  @var{key-seq} is nil, some other function has manipulated the
  current-input and is requesting a re-computation of the match."
  (let ((input-char (and key-seq (get-input-char key-seq))))
    (when input-char
      (vector-push-extend input-char (single-menu-current-input menu)))
    (handler-case
        (when (or input-char (not key-seq))
          (labels ((match-p (table-item)
                     (funcall (single-menu-filter-pred menu)
                              (car table-item)
                              (second table-item)
                              (single-menu-current-input menu))))
            (setf (menu-table menu) (remove-if-not #'match-p (single-menu-unfiltered-table menu))
                  (menu-selected menu) 0)
            (bound-check-menu menu)))
      (cl-ppcre:ppcre-syntax-error ()))))

(defmethod typing-action ((menu batch-menu) key-seq)
  "Mark the selected item with the character that was typed. If the character
is not allowed, as specified by allowed-markers, item is not marked"
  (let ((input-char (and key-seq (get-input-char key-seq))))
    (with-slots (selected table allowed-markers) menu
      (when (and input-char (or (not allowed-markers) (member input-char allowed-markers)))
        (setf (car (nth selected table)) input-char)
        (menu-down menu)))))

;; used for the default menus: they don't use menu-entrys
(defun menu-element-name (element)
  (if (listp element)
      (first element)
      element))

(defmethod get-menu-items ((menu menu))
  (mapcar #'menu-element-name
          (subseq (menu-table menu)
                  (menu-view-start menu) (menu-view-end menu))))

(defmethod get-menu-items ((menu batch-menu))
  (with-slots (table view-start view-end) menu
    (mapcar (lambda (entry)
              (if (car entry)
                  ;; if there is a mark, show it, else show a space
                  (concat (string (car entry)) (menu-entry-display (cdr entry)))
                  (concat " " (menu-entry-display (cdr entry)))))
            (subseq table
                    view-start view-end))))

(defun menu-item-matches-regexp (item-string item-object user-input)
  "The default filter predicate for SELECT-FROM-MENU. When using this
predicate, an item is visible when it matches all of the regular
expressions in USER-INPUT (multiple regexps are separated by one or
more spaces; ARGUMENT-POP is used to split the string)."
  (declare (ignore item-object))
  (match-all-regexps user-input item-string))

(defun run-menu (screen menu)
  "Runs the menu. Implement all of the methods in the menu, then pass an instance to this function"
  (declare (type menu menu))
  ;; align the menu, make the pages
  (bound-check-menu menu)
  (catch :menu-quit
    (unwind-protect
         (with-focus (screen-key-window screen)
           (let ((*suppress-echo-timeout* t))
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
                (multiple-value-bind (action key-seq) (read-from-keymap (menu-keymap menu))
                  (if action
                      (progn (funcall action menu)
                             (bound-check-menu menu))
                      (typing-action menu (first key-seq)))))))
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
                               :table table
                               :selected initial-selection
                               :prompt prompt
                               :view-start 0
                               :view-end 0
                               :additional-keymap extra-keymap
                               :FILTER-PRED filter-pred)))
      (run-menu screen menu))))

(defun select-from-batch-menu (screen table &key (prompt "Select:")
                                              allowed-markers
                                              (initial-selection 0)
                                              extra-keymap)
  "Prompt the user with a menu that allows them to mark each item
with a character. They can exit the menu by pressing enter, or
whatever key is mapped to 'menu-finish' in *menu-map*. Value returned
is an alist, where the cdr of each entry is a list of
items that were marked with that character. Note that the lisp printer cannot
distinguish between '(a . (b c d)) and '(a b c d).

Example when \"foo\" and \"bar\" are marked with '#\d', and \"baz\" is not marked:
    ((#\d \"foo\" \"bar\") (NIL \"baz\"))
ALLOWED-MARKERS is a list of characters. If this parameter is specified, no
    other markers are allowed.
EXTRA-KEYMAP can be a keymap whose bindings will take precedence
    over the default bindings."
  (check-type screen screen)
  (check-type table list)
  (check-type prompt (or null string))
  (check-type allowed-markers list)
  (when table
    (let ((menu (make-instance 'batch-menu
                               :table table
                               :prompt prompt
                               :allowed-markers allowed-markers
                               :selected initial-selection
                               :additional-keymap extra-keymap)))
      (run-menu screen menu))))

(defun command-menu (screen items command-list &key (prompt "Select:")
                                                   (initial-selection 0)
                                                   extra-keymap)
  "Use batch-menu to make selections and run commands specified in command-list.

SCREEN: The screen to display the menu on.

ITEMS: The items to be shown in the list. This is expected to be a list of @code{menu-item}s.

COMMAND-LIST: A list of entries defining the commands associated with each mark.
              Only marks that are defined are allowed in the menu. The format
              for these entries is (mark-character function calling-options).

              Available calling-options:
                 :single   (Default) Each value is passed separately to the supplied function.
                 :all      all values selected with this mark are passed to the function in a list.

              Example:
                 '((#\d 'delete-window) (#\m 'move-multiple-windows :all))"
  (let ((results
         (select-from-batch-menu screen items
                                 :prompt prompt
                                 ;; use the first value of every entry
                                 ;; except when it is nill:
                                 :allowed-markers (mapcan (lambda (x)
                                                            (if (first x)
				                                (list (first x))))
                                                          command-list)
                                 :initial-selection initial-selection
                                 :extra-keymap extra-keymap)))
    (dolist (command-entry command-list)
      (let ((selections (assoc (first command-entry) results))
            (func (second command-entry))
            ;; change this to cddr if we ever have more than one option?
            (options (caddr command-entry)))
        (when selections
          (cond
            ((eql :all options)
             (funcall func (mapcar 'menu-entry-data (cdr selections))))
            ;; default option: check if it is nil:
            ((or (eql options :all) (eql options nil))
             (dolist (data (cdr selections))
               (funcall func (menu-entry-data data))))
            (t (error (format nil "keyword ~s not a valid command option for selection-menu."
                              options)))))))))

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

          (define-key m (kbd "C-g") 'menu-abort)
          (define-key m (kbd "ESC") 'menu-abort)
          (define-key m (kbd "RET") 'menu-finish)
          m)))

(when (null *single-menu-map*)
  (setf *single-menu-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "DEL") 'menu-backspace)
          m)))

(when (null *batch-menu-map*)
  (setf *batch-menu-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd  "DEL") 'menu-backspace)
          (define-key m (kbd "n") 'menu-down)
          (define-key m (kbd  "p") 'menu-up)
          (define-key m (kbd "space") 'menu-down)
          (define-key m (kbd "u") (lambda (menu)
                                    (batch-menu-unmark-selected menu)
                                    (menu-down menu)))
          (define-key m (kbd "x") 'menu-finish)
          m)))

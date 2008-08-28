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
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; Implementation of an interactive menu.
;;
;; Code:

;;; interactive menu

(in-package #:stumpwm)

(export '())

(defvar *menu-map* nil
  "The keymap used by the interactive menu.")

(when (null *menu-map*)
  (setf *menu-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "C-p") 'menu-up)
          (define-key m (kbd "Up") 'menu-up)
          (define-key m (kbd "k") 'menu-up)

          (define-key m (kbd "C-n") 'menu-down)
          (define-key m (kbd "Down") 'menu-down)
          (define-key m (kbd "j") 'menu-down)
          (define-key m (kbd "C-g") 'menu-abort)
          (define-key m (kbd "ESC") 'menu-abort)
          (define-key m (kbd "RET") 'menu-finish)
          m)))

(defvar *current-menu-input* nil)

(defstruct menu-state
  table prompt selected)

(defun bound-check-menu (menu)
  (setf (menu-state-selected menu)
        (cond ((< (menu-state-selected menu) 0)
               (1- (length (menu-state-table menu))))
              ((>= (menu-state-selected menu) (length (menu-state-table menu)))
               0)
              (t (menu-state-selected menu)))))

(defun menu-up (menu)
  (setf *current-menu-input* "")
  (decf (menu-state-selected menu))
  (bound-check-menu menu))

(defun menu-down (menu)
  (setf *current-menu-input* "")
  (incf (menu-state-selected menu))
  (bound-check-menu menu))

(defun menu-finish (menu)
  (throw :menu-quit (nth (menu-state-selected menu) (menu-state-table menu))))

(defun menu-abort (menu)
  (declare (ignore menu))
  (throw :menu-quit nil))

(defun get-input-char (key)
  "returns t if key is a character suitable for menu
completion (e.g. not backspace or F9)"
  (let ((char (xlib:keysym->character *display* (key-keysym key))))
    (if (or (key-mods-p key) (null char)
            (not (characterp char)))
        nil
        char)))

(defun menu-element-name (element)
  (if (listp element)
      (first element)
      element))

(defun check-menu-complete (menu key-seq)
  "If the use entered a key not mapped in @var{*menu-map}, check if
  he's trying to type an entry's name"
  (let ((input-char (get-input-char key-seq)))
    (when input-char
      (setf *current-menu-input*
	    (concatenate 'string
			 *current-menu-input*
			 (string input-char)))
      (do* ((cur-pos 0 (1+ cur-pos))
	    (rest-elem (menu-state-table menu)
		       (cdr rest-elem))
	    (cur-elem (car rest-elem) (car rest-elem))
	    (cur-elem-name (menu-element-name cur-elem) (menu-element-name cur-elem))
	    (current-input-length (length *current-menu-input*))
	    (match-regex (ppcre:create-scanner *current-menu-input* :case-insensitive-mode t)))
	   ((not cur-elem))
	(when (and (>= (length cur-elem-name) current-input-length)
		   (ppcre:scan match-regex cur-elem-name))
	  (setf (menu-state-selected menu) cur-pos)
	  (return))))))

(defun select-from-menu (screen table &optional prompt (initial-selection 0))
  "Prompt the user to select from a menu on SCREEN. TABLE can be
a list of values or an alist. If it's an alist, the CAR of each
element is displayed in the menu. What is displayed as menu items
must be strings. Returns the selected element in TABLE or nil if aborted.

See *menu-map* for menu bindings."
  (check-type screen screen)
  (check-type table list)
  (check-type prompt (or null string))
  (check-type initial-selection integer)
  (let* ((menu (make-menu-state
                :table table
                :prompt prompt
                :selected initial-selection))
         (menu-options (mapcar #'menu-element-name
                               table))
         (menu-text (if prompt
                        (cons prompt menu-options)
                        menu-options))
         (*record-last-msg-override* t)
         (*suppress-echo-timeout* t)
	 (*current-menu-input* ""))
    (bound-check-menu menu)
    (catch :menu-quit
      (unwind-protect
           (with-focus (screen-key-window screen)
             (loop
                (echo-string-list screen menu-text
                                  (+ (menu-state-selected menu) (if prompt 1 0)))
                (multiple-value-bind (action key-seq) (read-from-keymap *menu-map*)
		  (if action
		      (funcall action menu)
		      (check-menu-complete menu (first key-seq))))))
        (unmap-all-message-windows)))))

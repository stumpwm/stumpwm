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
  (decf (menu-state-selected menu))
  (bound-check-menu menu))

(defun menu-down (menu)
  (incf (menu-state-selected menu))
  (bound-check-menu menu))

(defun menu-finish (menu)
  (throw :menu-quit (nth (menu-state-selected menu) (menu-state-table menu))))

(defun menu-abort (menu)
  (declare (ignore menu))
  (throw :menu-quit nil))

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
         (menu-options (mapcar (lambda (elt)
                                 (if (listp elt)
                                     (first elt)
                                     elt))
                               table))
         (menu-text (if prompt
                        (cons prompt menu-options)
                        menu-options))
         (*record-last-msg-override* t)
         (*suppress-echo-timeout* t))
    (bound-check-menu menu)
    (catch :menu-quit
      (unwind-protect
           (with-focus (screen-key-window screen)
             (loop
                (echo-string-list screen menu-text
                                  (+ (menu-state-selected menu) (if prompt 1 0)))
                (let ((action (read-from-keymap *menu-map*)))
                  (when action
                    (funcall action menu)))))
        (unmap-all-message-windows)))))

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

(export '())

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
  table prompt selected view-start view-end
  (current-input (make-array 10 :element-type 'character :adjustable t :fill-pointer 0)))

(defun bound-check-menu (menu)
  "Adjust the menu view and selected item based
on current view and new selection."
  (let ((len (length (menu-state-table menu))))
    (setf (menu-state-selected menu)
          (cond ((< (menu-state-selected menu) 0) (1- len))
                ((>= (menu-state-selected menu) len) 0)
                (t (menu-state-selected menu))))
    (when (and *menu-maximum-height*
               (> len *menu-maximum-height*))  ; scrolling required
      (let ((sel (menu-state-selected menu)))
        (setf (values (menu-state-view-start menu)
                      (menu-state-view-end menu))
              (cond ((< sel *menu-maximum-height*)
                     (values 0 *menu-maximum-height*))
                    ((> sel (- len *menu-maximum-height*))
                     (values (- len *menu-maximum-height*) len))
                    ((< sel (menu-state-view-start menu))
                     (values (- sel *menu-scrolling-step*)
                             (- (+ sel *menu-maximum-height*)
                                *menu-scrolling-step*)))
                    ((>= sel (menu-state-view-end menu))
                     (values (+ (- sel *menu-maximum-height*)
                                *menu-scrolling-step*)
                             (+ sel *menu-scrolling-step*)))
                    (t
                     (values (menu-state-view-start menu)
                             (menu-state-view-end menu)))))))))

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
  he's trying to type an entry's name. Match is case insensitive as
  long as the user types lower-case characters. If @var{key-seq} is
  nil, some other function has manipulated the current-input and is
  requesting a re-computation of the match."
  (let ((input-char (and key-seq (get-input-char key-seq))))
    (when input-char
      (vector-push-extend input-char (menu-state-current-input menu)))
    (when (or input-char (not key-seq))
      (do* ((cur-pos 0 (1+ cur-pos))
	    (rest-elem (menu-state-table menu)
		       (cdr rest-elem))
	    (cur-elem (car rest-elem) (car rest-elem))
	    (cur-elem-name (menu-element-name cur-elem) (menu-element-name cur-elem))
	    (current-input-length (length (menu-state-current-input menu)))
	    (match-regex (ppcre:create-scanner (menu-state-current-input menu)
					       :case-insensitive-mode
					       (string= (string-downcase (menu-state-current-input menu))
							(menu-state-current-input menu)))))
	   ((not cur-elem))
	(when (and (>= (length cur-elem-name) current-input-length)
		   (ppcre:scan match-regex cur-elem-name))
	  (setf (menu-state-selected menu) cur-pos)
          (bound-check-menu menu)
	  (return))))))

(defun select-from-menu (screen table &optional prompt
                                                (initial-selection 0))
  "Prompt the user to select from a menu on SCREEN. TABLE can be
a list of values or an alist. If it's an alist, the CAR of each
element is displayed in the menu. What is displayed as menu items
must be strings. Returns the selected element in TABLE or nil if aborted.

See *menu-map* for menu bindings."
  (check-type screen screen)
  (check-type table list)
  (check-type prompt (or null string))
  (check-type initial-selection integer)
  (let* ((menu-options (mapcar #'menu-element-name table))
         (menu-require-scrolling (and *menu-maximum-height*
                                       (> (length menu-options)
                                          *menu-maximum-height*)))
         (menu (make-menu-state
                :table table
                :prompt prompt
                :view-start (if menu-require-scrolling initial-selection 0)
                :view-end (if menu-require-scrolling
                              (+ initial-selection *menu-maximum-height*)
                              (length menu-options))
                :selected initial-selection))
         (*record-last-msg-override* t)
         (*suppress-echo-timeout* t))
    (bound-check-menu menu)
    (catch :menu-quit
      (unwind-protect
           (with-focus (screen-key-window screen)
             (loop
                (let ((strings (subseq menu-options
                                       (menu-state-view-start menu)
                                       (menu-state-view-end menu)))
                      (highlight (- (menu-state-selected menu)
                                    (menu-state-view-start menu))))
                  (unless (= 0 (menu-state-view-start menu))
                    (setf strings (cons "..." strings))
                    (incf highlight))
                  (unless (= (length menu-options) (menu-state-view-end menu))
                    (setf strings (nconc strings '("..."))))
                  (unless (= (fill-pointer (menu-state-current-input menu)) 0)
                    (setf strings
                          (cons (format nil "Search: ~a"
                                        (menu-state-current-input menu))
                                strings))
                    (incf highlight))
                  (when prompt
                    (setf strings (cons prompt strings))
                    (incf highlight))
                  (echo-string-list screen strings highlight))
                (multiple-value-bind (action key-seq) (read-from-keymap (list *menu-map*))
                  (if action
                      (funcall action menu)
                      (check-menu-complete menu (first key-seq))))))
        (unmap-all-message-windows)))))

;; Copyright (C) 2018 Ram Krishnan
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
;; Provides a simple way to remap keybindings in applications running
;; under StumpWM
;;
;; Code:

(in-package #:stumpwm)

(export '(define-remapped-keys))

(defvar *remap-keys-window-class-list* nil)

(defun find-remap-keys-window-class (class)
  (first
   (member-if (lambda (pattern)
                (string-match class pattern))
              *remap-keys-window-class-list*
              :key 'car)))

(defun make-remap-keys (kmap)
  (labels ((as-list (x) (if (consp x) x (list x)))
           (validated-kbd (key)
             (or (kbd key)
                 (throw 'error
                   (format nil "Invalid keyspec: ~S" key)))))
    (mapcar (lambda (kspec)
              (let ((src-key (car kspec))
                    (target-keyseq (as-list (cdr kspec))))
                (cons src-key
                      (mapcar #'validated-kbd target-keyseq))))
            kmap)))

(defun remap-keys-grab-keys (win)
  (let* ((window-class (window-class win))
         (keymap (cdr (find-remap-keys-window-class window-class)))
         (src-keys (mapcar 'car keymap)))
    (dolist (key src-keys)
      (xwin-grab-key (window-xwin win) (kbd key)))))

(defun remap-keys-focus-window-hook (new-focus cur-focus)
  (declare (ignorable cur-focus))
  (when new-focus
    (remap-keys-grab-keys new-focus)))

(defun remap-keys-event-handler (code state)
  (let* ((raw-key (code-state->key code state))
         (window (current-window))
         (window-class (window-class window))
         (keymap (cdr (find-remap-keys-window-class window-class)))
         (keys (cdr (assoc (print-key raw-key) keymap :test 'equal))))
    (when keys
      (dolist (key keys)
        (send-fake-key window key))
      t)))

(defun define-remapped-keys (specs)
  "Define the keys to be remapped and their mappings. The SPECS
argument needs to be of the following structure:

  (regexp . ((\"key-to-remap\" . <new-keycodes>) ...))

EXAMPLE:
  (define-remapped-keys
    '((\"Firefox\"
       (\"C-n\"   . \"Down\")
       (\"C-p\"   . \"Up\")
       (\"C-k\"   . (\"C-S-End\" \"C-x\")))))

  The above form remaps Ctrl-n to Down arrow, and Ctrl-p to Up arrow
  keys.  The Ctrl-k key is remapped to the sequence of keys
  Ctrl-Shift-End followed by Ctrl-x."
  (setq *remap-keys-window-class-list*
        (mapcar (lambda (spec)
                  (let ((pattern (car spec))
                        (kmap (cdr spec)))
                    (cons pattern (make-remap-keys kmap))))
                specs))
  (when *remap-keys-window-class-list*
    (add-hook *focus-window-hook* 'remap-keys-focus-window-hook))
  (setq *custom-key-event-handler* (when *remap-keys-window-class-list*
                                     'remap-keys-event-handler)))

(defcommand send-raw-key () ()
  "Prompts for a key and forwards it to the CURRENT-WINDOW."
  (message "Press a key to send")
  (let* ((k (read-key))
         (code (car k))
         (state (cdr k))
         (screen (current-screen)))
    (unmap-message-window screen)
    (when (screen-current-window screen)
      (let* ((win (screen-current-window screen))
             (xwin (window-xwin win)))
        (dolist (event '(:key-press :key-release))
          (xlib:send-event xwin
                           event
                           (xlib:make-event-mask event)
                           :display *display*
                           :root (screen-root screen)
                           ;; Apparently we need these in here, though they
                           ;; make no sense for a key event.
                           :x 0 :y 0 :root-x 0 :root-y 0
                           :window xwin
                           :event-window xwin
                           :code code
                           :state state))))))

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

(export '(define-remapped-keys *remapped-keys-enabled-p*))

(defvar *remap-keys-window-match-list* nil)

(defvar *remapped-keys-enabled-p* t
  "Bool to toggle remapped-keys on/off. Defaults to t ")

(defun find-remap-keys-by-window (window)
  (first
   (member-if (lambda (pattern)
                (cond
                  ((stringp pattern)
                   (string-match (window-class window) pattern))

                  ((or (symbolp pattern) (functionp pattern))
                   (funcall pattern window))))
              *remap-keys-window-match-list*
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
  (let* ((keymap (cdr (find-remap-keys-by-window win)))
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
         (keymap (when window
                   (cdr (find-remap-keys-by-window window))))
         (keys (cdr (assoc (print-key raw-key) keymap :test 'equal))))
    (when keys
      (dolist (key keys)
        (send-fake-key window (if *remapped-keys-enabled-p*
                                  key
                                  raw-key)))
      t)))

(defun define-remapped-keys (specs)
  "Define the keys to be remapped and their mappings. The SPECS
argument needs to be of the following structure:

  (regexp-or-function . ((\"key-to-remap\" . <new-keycodes>) ...))

EXAMPLE:
  (define-remapped-keys
    '((\"Firefox\"
       (\"C-n\"   . \"Down\")
       (\"C-p\"   . \"Up\")
       (\"C-k\"   . (\"C-S-End\" \"C-x\")))))

  The above form remaps Ctrl-n to Down arrow, and Ctrl-p to Up arrow
  keys.  The Ctrl-k key is remapped to the sequence of keys
  Ctrl-Shift-End followed by Ctrl-x."
  (setq *custom-key-event-handler* nil
        *remap-keys-window-match-list*
        (mapcar (lambda (spec)
                  (let ((pattern (car spec))
                        (kmap (cdr spec)))
                    (cons pattern (make-remap-keys kmap))))
                specs))
  (when *remap-keys-window-match-list*
    (add-hook *focus-window-hook* 'remap-keys-focus-window-hook)
    (setq *custom-key-event-handler* 'remap-keys-event-handler)))

(defcommand send-raw-key () ()
  "Prompts for a key and forwards it to the CURRENT-WINDOW."
  (message "Press a key to send")
  (let* ((screen (current-screen))
         (win (screen-current-window screen))
         (k (with-focus (screen-key-window screen)
              (read-key)))
         (code (car k))
         (state (cdr k)))
    (unmap-message-window screen)
    (when win
      (let ((xwin (window-xwin win)))
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

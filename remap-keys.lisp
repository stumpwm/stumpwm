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

(defcommand send-remapped-key () ()
  "If the WINDOW-CLASS of the current window matches a previously
defined REMAP-KEYS rule (see REMAP-KEYS:DEFINE-REMAPPED-KEYS), this
command looks up the most recently triggered key sequence in that rule
and forwards the new key-sequence to the target window."
  (let* ((raw-key (first *current-key-seq*))
         (window (current-window))
         (window-class (window-class window))
         (keymap (cdr (find-remap-keys-window-class window-class)))
         (keys (cdr (assoc (print-key raw-key) keymap :test 'equal))))
    (dformat 1 "~s ~s ~s ~s~%"
             window
             window-class
             (print-key raw-key)
             (when keys
               (mapcar 'print-key keys)))
    (if keys
        (dolist (key keys)
          (send-fake-key window key))
        (send-meta-key (current-screen) raw-key))))

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

(defun define-remapped-keys (specs)
  "Define the keys to be remapped and their mappings. The SPECS
argument needs to be of the following structure:

  (regexp . ((\"key-to-remap\" . <new-keycodes>) ...))

EXAMPLE:
  (remap-keys:define-remapped-keys
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
  (let ((keys (mapcar 'car
                      (mapcan 'cdr *remap-keys-window-class-list*))))
    (dolist (k keys)
      (define-key *top-map*
          (kbd k)
        "send-remapped-key"))))

(defcommand send-raw-key () ()
  "Prompts for a key and forwards it to the CURRENT-WINDOW."
  (message "Press a key to send")
  (let* ((k (read-key))
         (code (car k))
         (state (cdr k))
         (screen (current-screen)))
    (unmap-message-window screen)
    (when (screen-current-window screen)
      (let ((win (screen-current-window screen)))
        (xlib:send-event (window-xwin win)
                         :key-press (xlib:make-event-mask :key-press)
                         :display *display*
                         :root (screen-root
                                (window-screen win))
                         ;; Apparently we need these in here, though they
                         ;; make no sense for a key event.
                         :x 0 :y 0 :root-x 0 :root-y 0
                         :window (window-xwin win)
                         :event-window (window-xwin win)
                         :code code
                         :state state)))))

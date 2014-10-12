;; Copyright (C) 2003-2008 Shawn Betts
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
;; Handle the X selection.
;;
;; Code:

(in-package #:stumpwm)

(export '(get-x-selection
          set-x-selection))

(defun export-selection (selection)
  (let* ((screen (current-screen))
         (selwin (screen-focus-window (current-screen)))
         (root (screen-root screen)))
    (xlib:set-selection-owner *display* selection selwin)
    (unless (xlib:window-equal (xlib:selection-owner *display* selection) selwin)
      (error "Can't set selection owner"))
    ;; also set the cut buffer for completeness. Note that this always sets cut
    ;; buffer 0.
    (xlib:change-property root :cut-buffer0 (getf *x-selection* selection)
                               :string 8 :transform #'xlib:char->card8
                               :mode :replace)))

(defun set-x-selection (text &optional (selection :primary))
  "Set the X11 selection string to @var{string}."
  (setf (getf *x-selection* selection) text)
  (export-selection selection))

(defun send-selection (requestor property selection target time)
  (dformat 1 "send-selection ~s ~s ~s ~s ~s~%" requestor property selection target time)
  (case target
    ;; they're requesting what targets are available
    (:targets
     (xlib:change-property requestor property (list :targets :string :utf8_string) target 8 :mode :replace))
    ;; send them a string
    (:string
     (xlib:change-property requestor property (getf *x-selection* selection)
                           :string 8 :mode :replace :transform #'xlib:char->card8))
    (:utf8_string
     (xlib:change-property requestor property (string-to-utf8 (getf *x-selection* selection)) target 8 :mode :replace))
    ;; we don't know how to handle anything else
    (t
     (setf property nil)))
  (xlib:send-event requestor :selection-notify nil
                   :display *display*
                   :window requestor
                   :selection selection
                   :property property
                   :target target
                   :time time)
  (xlib:display-finish-output *display*))

(defun get-x-selection (&optional timeout (selection :primary))
  "Return the x selection no matter what client own it."
  (labels ((wait-for-selection (&rest event-slots &key display event-key &allow-other-keys)
             (declare (ignore display))
             (when (eq event-key :selection-notify)
               (destructuring-bind (&key window property &allow-other-keys) event-slots
                 (if property
                     (utf8-to-string (xlib:get-property window property :type :utf8_string :result-type 'vector :delete-p t))
                     "")))))
    (or (getf *x-selection* selection)
        (progn
          (xlib:convert-selection selection :utf8_string (screen-input-window (current-screen)) :stumpwm-selection)
          ;; Note: this may spend longer than timeout in this loop but it will eventually return.
          (let ((time (get-internal-real-time)))
            (loop for ret = (xlib:process-event *display* :handler #'wait-for-selection :timeout timeout :discard-p nil)
                  when (or ret
                           (> (/ (- time (get-internal-real-time)) internal-time-units-per-second)
                              timeout))
                  ;; make sure we return a string
                  return (or ret "")))))))

;;; Commands

(defcommand putsel (string) ((:rest "Text: "))
  "Stuff the string @var{string} into the X selection."
  (set-x-selection string))

;; FIXME: this function is basically useless atm.
(defcommand getsel () ()
  "Echo the X selection."
  (message "~a" (get-x-selection)))

(defcommand copy-last-message () ()
  "Copy the last message displayed into the X selection"
  (when (screen-last-msg (current-screen))
    (set-x-selection (uncolorify (format nil "~{~a~^~%~}" (car (screen-last-msg (current-screen))))))))

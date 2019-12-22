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

;; Conditions provide a mechanism for improving the outcome of exceptional
;; situations.

(in-package :stumpwm)

;;; These are the conditions that are parents all of StumpWM's conditions. Note
;;; that conditions must be defined before they are used, so higher-order
;;; conditions come first in this file.

(define-condition stumpwm-condition (condition)
  ((message :initarg :message :reader warning-message))
  (:documentation "Any stumpmwm specific condition should inherit from this.")
  (:report (lambda (condition stream)
             (format stream "~A" (warning-message condition)))))

(define-condition stumpwm-error (stumpwm-condition error)
  ()
  (:documentation "Any stumpwm specific error should inherit this."))

(define-condition stumpwm-warning (warning stumpwm-condition)
  ()
  (:documentation "Adds a message slot to warning. Any stumpwm specific warning
  should inherit from this."))

(define-condition command-docstring-warning (style-warning)
  ;; Don't define an accessor to prevent collision with the generic command
  ((command :initarg :command))
  (:report
   (lambda (condition stream)
     (format stream "The command ~A doesn't have a docstring" (slot-value condition 'command))))
  (:documentation "Prevent accidentally not using docstrings (bad style!)."))

(defun report-kbd-parse-error (c stream)
  "The message for a failed attempt to parse a key."
  (format stream "Failed to parse key string: ~s" (slot-value c 'string)))

(define-condition kbd-parse-error (stumpwm-error)
  ((string :initarg :string))
  (:report report-kbd-parse-error)
  (:documentation "Raised when a kbd string failed to parse."))

(define-condition not-implemented (stumpwm-error)
  ()
  (:documentation "A function has been called that is not implemented yet."))

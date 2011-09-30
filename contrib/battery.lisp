;;; Battery charge formatters for the mode-line
;;;
;;; Copyright 2008 Vitaly Mayatskikh
;;;
;;; Maintainer: Julian Stecklina
;;;
;;; This module is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This module is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;

;;; USAGE:
;;;
;;; Put:
;;;
;;;     (load "/path/to/battery.lisp")
;;;
;;; In your ~/.stumpwmrc
;;;
;;; Then you can use "%b" in your mode line format.
;;;
;;; NOTES:
;;;
;;; This is specific to Linux.

(in-package :stumpwm)

(export '(*battery-name*))

(dolist (a '((#\b fmt-bat-charge)))
  (pushnew a *screen-mode-line-formatters* :test 'equal))

(defvar *bat-state* nil)
(defvar *bat-remain* 0)
(defvar *bat-remain-time* nil)
(defvar *bat-prev-time* 0)

(defvar *battery-name* "BAT0")

(defun read-battery-file (battery fname)
  (let ((fields (make-hash-table :test #'equal)))
    (with-open-file (s (concatenate 'string "/proc/acpi/battery/" battery "/" fname) 
		       :if-does-not-exist nil)
      (if s
          (do ((line (read-line s nil nil) (read-line s nil nil)))
              ((null line) fields)
            (let ((split (cl-ppcre:split ":\\s*" line)))
              (setf (gethash (string-trim '(#\Space) (car split)) fields)
                    (string-trim '(#\Space) (cadr split)))))
          ""))))

(defun current-battery-charge ()
  "Calculate remaining battery charge. Don't make calculation more than once in 15 seconds."
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    (when (or (= 0 *bat-prev-time*) (>= (- now *bat-prev-time*) 15))
      (setf *bat-prev-time* now)
      (let ((battery-state (read-battery-file *battery-name* "state"))
            (battery-info (read-battery-file *battery-name* "info")))
        (if (string= "no" (gethash "present" battery-state))
            (setf *bat-state* nil)
            (let ((charge-state (gethash "charging state" battery-state))
                  (remain (parse-integer (gethash "remaining capacity" battery-state)
                                         :junk-allowed t))
                  (rate (/ (or (parse-integer (gethash "present rate" battery-state)
                                              :junk-allowed t) 0) 60))
                  (full (parse-integer (gethash "last full capacity" battery-info)
                                       :junk-allowed t)))
              (setf *bat-remain* (round (/ (* 100 remain) full))
                    *bat-state* charge-state
                    *bat-remain-time* nil)

              (when (> rate 0)
                (let* ((online (round (/ (if (string= "charging" *bat-state*)
                                             (- full remain) remain)
                                         rate))))
                  (setf *bat-remain-time* (multiple-value-bind (h m)
                                              (truncate online 60)
                                            (list h m)))))))))))

(defun fmt-bat-charge (ml)
  "Returns a string representing the remaining battery charge (for laptop users.)"
  (declare (ignore ml))
  (current-battery-charge)
  (if *bat-state*
      (format nil "BAT: ^[~A~D%^]~A"
              (bar-zone-color *bat-remain* 50 30 10 t)
              *bat-remain*
              (if *bat-remain-time*
                  (format nil " (~2,'0d:~2,'0d) ~A"  (car *bat-remain-time*) (cadr *bat-remain-time*) *bat-state*) "")) "no battery"))


;; Alternative display:
;;
;;    TT: RRR% (HH:MM)  [or "NO BAT" if present = no]
;;
;;   TT    = AC/DC (AC if charging state = charged/charging,
;;                  DC if charging state = discharging)
;;
;;   RRR   = remain/full
;;
;;   HH:MM = time until charged/discharged (present when state is charging
;;                                          or discharging)
;;
;; (defun fmt-bat-charge (ml)
;;   "Returns a string representing the remaining battery charge (for laptop users.)"
;;   (declare (ignore ml))
;;   (current-battery-charge)
;;   (if (not *bat-state*)
;;       "NO BAT"
;;       (format nil "~A:~D%~A"
;;               (if (or (string= *bat-state* "charging")
;;                       (string= *bat-state* "charged"))
;;                   "AC" "DC")
;;            *bat-remain*
;;            (if (and (string/= *bat-state* "charged") *bat-remain-time*)
;;                (format nil (if (and (= (car *bat-remain-time*) 0)
;;                                        (< (cadr *bat-remain-time*) 30))
;;                                   " (^[^B^1*~2,'0d:~2,'0d^])" " (~2,'0d:~2,'0d)")
;;                           (car *bat-remain-time*)
;;                           (cadr *bat-remain-time*))
;;                   ""))))

;;; Battery charge formatters for the mode-line
;;;
;;; Copyright 2008 Vitaly Mayatskikh
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
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA
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

(dolist (a '((#\b fmt-bat-charge)))
  (push a *screen-mode-line-formatters*))

(defvar *bat-state* nil)
(defvar *bat-remain* 0)
(defvar *bat-remain-time* nil)
(defvar *bat-prev-time* 0)

(defun current-battery-charge ()
  "Calculate remaining battery charge. Don't make calculation more than once in 15 seconds."
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    (when (or (= 0 *bat-prev-time*) (>= (- now *bat-prev-time*) 15))
      (setf *bat-prev-time* now)
      (if (string= "no" (string-trim '(#\Newline) (run-shell-command "grep present /proc/acpi/battery/BAT0/state | grep -o -e '\\w*$'" t)))
	  (setq *bat-state* nil)
	  (let ((remain (parse-integer (run-shell-command "grep remaining\\ capacity /proc/acpi/battery/BAT0/state | sed -n 's/[a-zA-Z\\ \\:]*//gp'" t)))
		(rate (/ (parse-integer (run-shell-command "grep present\\ rate /proc/acpi/battery/BAT0/state | sed -n 's/[a-zA-Z\\ \\:]*//gp'" t)) 60))
		(full (parse-integer (run-shell-command "grep last\\ full\\ capacity /proc/acpi/battery/BAT0/info | sed -n 's/[a-zA-Z\\:\\ ]//gp'" t))))

	    (setq *bat-remain* (round (/ (* 100 remain) full))
		  *bat-state* (string-trim '(#\Newline) (run-shell-command "grep charging\\ state /proc/acpi/battery/BAT0/state | grep -o -e '\\w*$'" t))
		  *bat-remain-time* nil)
	    
	    (when (> rate 0)
	      (let* ((online (round (/ (if (string= "charging" *bat-state*) (- full remain) remain) rate))))
		(setq *bat-remain-time* (multiple-value-bind (h m) 
					    (truncate online 60)
					  (list h m))))))))))

(defun fmt-bat-charge (ml)
  "Returns a string representing the remaining battery charge (for laptop users.)"
  (declare (ignore ml))
  (current-battery-charge)
  (if *bat-state*
      (format nil "BAT: ~D%~A" 
	      *bat-remain*
	      (if *bat-remain-time*
		  (format nil " (~2,'0d:~2,'0d) ~A"  (car *bat-remain-time*) (cadr *bat-remain-time*) *bat-state*) "")) "no battery"))

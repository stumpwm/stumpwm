;;; CPU formatters for the mode-line
;;;
;;; Copyright 2007 Anonymous Coward, Jonathan Moore Liles.
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
;;;     (load "/path/to/cpu.lisp")
;;;
;;; In your ~/.stumpwmrc
;;;
;;; Then you can use "%c %t" in your mode line format.
;;;
;;; NOTES:
;;;
;;; This is specific to Linux.

(in-package :stumpwm)

(export '(*acpi-thermal-zone*))

;; Install formatters.
(dolist (a '((#\c fmt-cpu-usage)
             (#\C fmt-cpu-usage-bar)
             (#\f fmt-cpu-freq)
             (#\t fmt-cpu-temp)))
  (pushnew a *screen-mode-line-formatters* :test 'equal))

;; Defaults arguments for fmt-cpu-usage-bar
(defvar *cpu-usage-bar-width* 10)
(defvar *cpu-usage-bar-full* #\#)
(defvar *cpu-usage-bar-empty* #\:)


(defvar *prev-user-cpu* 0)
(defvar *prev-sys-cpu* 0)
(defvar *prev-idle-cpu* 0)
(defvar *prev-iowait* 0)
(defvar *prev-result* '(0 0 0))
(defvar *prev-time* 0)

;; More or less yanked from the wiki.
(defun current-cpu-usage ()
  "Return the average CPU usage since the last call.  First value is percent
of CPU in use.  Second value is percent of CPU in use by system processes.
Third value is percent of time since last call spent waiting for IO (or 0 if
not available). Don't make calculation more than once a second."
  (let ((cpu-result 0)
        (sys-result 0)
        (io-result nil)
        (now (/ (get-internal-real-time) internal-time-units-per-second)))
    (when (>= (- now *prev-time*) 1)
      (setf *prev-time* now)
      (with-open-file (in #P"/proc/stat" :direction :input)
        (read in)
        (let* ((norm-user (read in))
               (nice-user (read in))
               (user (+ norm-user nice-user))
               (sys (read in))
               (idle (read in))
               (iowait (or (ignore-errors (read in)) 0))
               (step-denom (- (+ user sys idle iowait)
                              (+ *prev-user-cpu* *prev-sys-cpu* *prev-idle-cpu* *prev-iowait*))))
          (setf cpu-result (/ (- (+ user sys)
                                 (+ *prev-user-cpu* *prev-sys-cpu*))
                              step-denom)
                sys-result (/ (- sys *prev-sys-cpu*)
                              step-denom)
                io-result (/ (- iowait *prev-iowait*)
                             step-denom)
                *prev-user-cpu* user
                *prev-sys-cpu* sys
                *prev-idle-cpu* idle
                *prev-iowait* iowait
                *prev-result* (list cpu-result sys-result io-result))))))
  (apply 'values *prev-result*))

(defun fmt-cpu-usage (ml)
  "Returns a string representing current the percent of average CPU
  utilization."
  (declare (ignore ml))
  (let ((cpu (truncate (* 100 (current-cpu-usage)))))
    (format nil "CPU: ^[~A~3D%^] " (bar-zone-color cpu) cpu)))

(defun fmt-cpu-usage-bar (ml &optional (width *cpu-usage-bar-width*) (full *cpu-usage-bar-full*) (empty *cpu-usage-bar-empty*))
  "Returns a coloured bar-graph representing the current percent of average CPU
utilization."
  (declare (ignore ml))
  (let ((cpu (truncate (* 100 (current-cpu-usage)))))
    (bar cpu width full empty)))

(defun get-proc-file-field (fname field)
  (with-open-file (s fname :if-does-not-exist nil) ;
    (if s
        (do ((line (read-line s nil nil) (read-line s nil nil)))
            ((null line) nil)
          (let ((split (cl-ppcre:split "\\s*:\\s*" line)))
            (when (string= (car split) field) (return (cadr split)))))
        "")))

(defun fmt-cpu-freq (ml)
  "Returns a string representing the current CPU frequency (especially useful for laptop users.)"
  (declare (ignore ml))
  (let ((mhz (parse-integer (get-proc-file-field "/proc/cpuinfo" "cpu MHz")
                            :junk-allowed t)))
    (if (>= mhz 1000)
	(format nil "~,2FGHz" (/ mhz 1000))
	(format nil "~DMHz" mhz))))

(defvar *acpi-thermal-zone*
  (let ((proc-dir (list-directory #P"/proc/acpi/thermal_zone/"))
        (sys-dir (sort
                  (remove-if-not
                   (lambda (x)
                     (when (cl-ppcre:scan "^.*/thermal_zone\\d+/" (namestring x))
                       x))
                   (list-directory #P"/sys/class/thermal/"))
                  #'string< :key #'namestring)))
    (cond
      (proc-dir
       (cons :procfs
             (make-pathname :directory (pathname-directory (first proc-dir))
                            :name "temperature")))
      (sys-dir
       (cons :sysfs
             (make-pathname :directory (pathname-directory (first sys-dir))
                            :name "temp"))))))

(defun fmt-cpu-temp (ml)
  "Returns a string representing the current CPU temperature."
  (declare (ignore ml))
  (format nil "~a°C"
          (case (car *acpi-thermal-zone*)
            (:procfs (parse-integer
                      (get-proc-file-field (cdr *acpi-thermal-zone*) "temperature")
                      :junk-allowed t))
            (:sysfs   (with-open-file (f (cdr *acpi-thermal-zone*))
                        (/ (read f) 1000))))))

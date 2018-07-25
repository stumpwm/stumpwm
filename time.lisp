;; Copyright (C) 2003-2008 Ivy Foster
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
;; This file contains code relating to the display of time.
;;
;; When setting `*time-format-string-default*' to look like you want, the
;; options are exactly the same as those in the output of date --help (with date
;; 6.12), with the exception of a few unimplemented functions (see the comments
;; in *time-format-string-alist*, below). `*time-modeline-string*' is also
;; customizable; it defaults to the same value as *time-format-string-default*.
;;

;; TODO:
;;
;; - Implement all options from date.
;; - Simplify code (fewer helper functions somehow?)

;; Code:

(in-package :stumpwm)

(export '(*time-format-string-default*
          *time-modeline-string*
          time-format
          echo-date
          time
          refresh-time-zone))

(defvar *time-format-string-default* "%a %b %e %Y %k:%M:%S"
  "The default value for `echo-date', (e.g, Thu Mar  3 2005 23:05:25).")

(defvar *time-modeline-string* "%a %b %e %k:%M:%S"
  "The default time value to pass to the modeline.")

(defvar *time-month-names*
  #("January" "February" "March" "April" "May" "June" "July" "August"
    "September" "October" "November" "December"))

(defvar *time-day-names*
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

;; `date --help` with date_6.12
(defvar *time-format-string-alist*
  '((#\a time-dow-shortname)
    (#\A time-dow-name)
    (#\b time-month-shortname)
    (#\B time-month-name)
    (#\c time-date-and-time)
    (#\C time-century)
    (#\d time-day-of-month)
    (#\D time-date)
    (#\e time-day-of-month-zero)
    (#\F time-date-full)
    ;; (#\g)   last two digits of year of ISO week number (see %G)
    ;; (#\G)   year of ISO week  number (see %V); normally useful only with %V
    (#\h time-month-shortname)
    (#\H time-hour-zero)
    (#\I time-hour-12hr-zero)
    ;; (#\j)   day of year (001..366)
    (#\k time-hour)
    (#\l time-hour-12hr)
    (#\m time-month)
    (#\M time-minute)
    (#\n time-newline)
    ;; (#\N)   nanoseconds (000000000..999999999)
    (#\p time-am-pm)
    (#\P time-am-pm-caps)
    (#\r time-12hr-time)
    (#\R time-24hr-and-minute)
    (#\s time-unix-era)
    (#\S time-second)
    (#\t time-tab)
    (#\T time-24hr-time)
    (#\u time-day-of-week)
    ;; (#\U)   week number of  year, with Sunday as first  day of week (00..53)
    ;; (#\V)   ISO  week number,  with  Monday as  first  day of  week (01..53)
    (#\w time-day-of-week-sun-start)
    ;; (#\W)   week number of  year, with Monday as first  day of week (00..53)
    ;; (#\x)   locale's date representation (e.g., 12/31/99)
    ;; (#\X)   locale's time representation (e.g., 23:13:48)
    (#\y time-year-short)
    (#\Y time-year)
    (#\z time-tz)
    ;; (#\:z)  +hh:mm numeric timezone (e.g., -04:00)
    ;; (#\::z) +hh:mm:ss numeric time zone (e.g., -04:00:00)
    ;; (#\:::z) numeric time zone with  : to necessary precision (e.g., -04, +05:30)
    ;; %Z   alphabetic time zone abbreviation (e.g., EDT)
    ))

(defcommand echo-date () ()
  "Display the date and time."
  (message "~a" (time-format *time-format-string-default*)))

(defcommand-alias time echo-date)

(defcommand refresh-time-zone () ()
  "Refresh the time zone information from the system.

If you change the system time zone while StumpWM is running you can
run this command to make StumpWM notice the change."
  (sb-alien:alien-funcall
    (sb-alien:extern-alien "tzset" (function sb-alien:void))))


;;; ------------------------------------------------------------------
;;; Helper functions
;;; ------------------------------------------------------------------

(defun get-decoded-system-time ()
  (decode-universal-time (+ (encode-universal-time 0 0 0 1 1 1970 0)
                            (sb-posix:time))))

(defun time-plist (&optional time)
  (multiple-value-bind (sec min hour dom mon year dow dstp tz)
      (or time (get-decoded-system-time))
    (list :second sec :minute min :hour hour :dom dom :month mon
          :year year :dow dow :dlsavings-p dstp :tz tz)))

(defun time-second ()
  (format nil "~2,'0D" (getf (time-plist) :second)))

(defun time-minute ()
  (format nil "~2,'0D" (getf (time-plist) :minute)))

(defun time-hour ()
  (format nil "~2,D" (getf (time-plist) :hour)))

(defun time-hour-zero ()
  (format nil "~2,'0D" (getf (time-plist) :hour)))

(defun time-hour-12hr ()
  (let ((hour (rem (getf (time-plist) :hour) 12)))
    (format nil "~2,D"
            (if (zerop hour) 12 hour))))

(defun time-hour-12hr-zero ()
  (let ((hour (rem (getf (time-plist) :hour) 12)))
    (format nil "~2,'0D"
            (if (zerop hour) 12 hour))))

(defun time-day-of-month-zero ()
  (format nil "~2,'0D" (getf (time-plist) :dom)))

(defun time-day-of-month ()
  (format nil "~2,' D" (getf (time-plist) :dom)))

(defun time-month ()
  (format nil "~2,'0D" (getf (time-plist) :month)))

(defun time-month-name ()
  (aref *time-month-names* (1- (getf (time-plist) :month))))

(defun time-month-shortname ()
  (subseq (time-month-name) 0 3))

(defun time-year ()
  (write-to-string (getf (time-plist) :year)))

(defun time-century ()
  (subseq (time-year) 0 2))

(defun time-year-short ()
  (subseq (time-year) 2))

(defun time-day-of-week ()
  (write-to-string (1+ (getf (time-plist) :dow))))

(defun time-day-of-week-sun-start ()
  (let ((dow (getf (time-plist) :dow)))
    (write-to-string (if (= dow 6) 0 (1+ dow)))))

(defun time-dow-name ()
  (aref *time-day-names* (getf (time-plist) :dow)))

(defun time-dow-shortname ()
  (subseq (time-dow-name) 0 3))

(defun time-newline ()
  (format nil "~a" #\newline))

(defun time-tab ()
  (format nil "~T"))

(defun time-am-pm ()
  (if (>= (getf (time-plist) :hour) 12)
      "pm" "am"))

(defun time-am-pm-caps ()
  (if (>= (getf (time-plist) :hour) 12)
      "PM" "AM"))

(defun time-tz ()
  (let ((tz (getf (time-plist) :tz))
        (dlsave (if (getf (time-plist) :dlsavings-p) 1 0)))
    (multiple-value-bind (hour-local decimal-local)
      (truncate (+ (* (float tz) -1)
                   (if dlsave 1 0)))
      (format nil "~A~2,'0D~2,'0D"
              (if (> hour-local 0) '+ '-)
              (abs hour-local)
              (truncate (if (/= decimal-local 0)
                            (* 60 decimal-local) 0))))))

(defun time-unix-era ()
  (format nil "~D" (sb-posix:time)))

(defun time-date-and-time ()
  (time-format "%a %h %d %H:%M:%S %Y"))

(defun time-date ()
  (time-format "%m/%d/%y"))

(defun time-date-full ()
  (time-format "%Y-%m-%d"))

(defun time-12hr-time ()
  (time-format "%I:%M:%S %P"))

(defun time-24hr-and-minute ()
  (time-format "%H:%M"))

(defun time-24hr-time ()
  (time-format "%H:%M:%S"))

(defun time-format (str)
  (format-expand *time-format-string-alist* str))

;;; End of file

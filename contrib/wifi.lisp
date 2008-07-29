;;; Wifi formatter for the mode-line
;;;
;;; Copyright 2008 John Li
;;;
;;; Maintainer: John Li
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
;;; Place the following in your ~/.stumpwmrc file:
;;;
;;;     (load-module "wifi")
;;;
;;; Then you can use "%I" in your mode line format (both "w" and "W"
;;; were taken. Think _I_EEE 802.11 :-)).
;;;
;;; Notes: This gets information through iwconfig, so it only works on
;;; Linux. On Debian-ish systems, iwconfig is in the wireless-tools
;;; package.

(defpackage :stumpwm.contrib.wifi
  (:use :common-lisp :stumpwm )
  (:export #:*iwconfig-path*))
(in-package :stumpwm.contrib.wifi)

(defvar *iwconfig-path* "/sbin/iwconfig"
  "Location if iwconfig, defaults to /sbin/iwconfig.")

(defmacro defun-cached (name interval arglist &body body)
  "Creates a function that does simple caching. The body must be
written in a functional style - the value returned is set as the
prev-val."
  (let ((prev-time (gensym "PREV-TIME"))
        (prev-val (gensym "PREV-VAL"))
        (now (gensym "NOW"))
        (docstring (when (stringp (car body))
                     (pop body))))
    `(let ((,prev-time 0)
           (,prev-val nil))
       (defun ,name ,arglist
         ;; if no docstring, return nothing (not even nil)
         ,@(when docstring (list docstring))
         (let ((,now (get-internal-real-time)))
           (when (>= (- ,now ,prev-time)
                     (* ,interval internal-time-units-per-second))
             (setf ,prev-time ,now)
             (setf ,prev-val (locally ,@body)))
           ,prev-val)))))

(defun-cached fmt-wifi 5 (ml)
  "Formatter for wifi status. Displays the ESSID of the access point
you're connected to as well as the signal strength. When no valid data
is found, just displays nil."
  (declare (ignore ml))
  (let ((essid (run-shell-command
                (format nil "~A 2>/dev/null | grep ESSID | cut -d '\"' -f 2 | tr -d '\\n'"
                        *iwconfig-path*)
                t)))
    (if (string= essid "")
        "nil"
        (let* ((qual (run-shell-command
                      (format nil "~A 2>/dev/null | grep 'Link Quality' | awk '{print $2}' | cut -b 9- | tr -d '\\n'"
                              *iwconfig-path*)
                      t))
               (num-pos (position #\/ qual))
               (perc (if num-pos
                         (round (* 100 (/ (parse-integer
                                           (subseq qual 0 num-pos))
                                          (parse-integer
                                           (subseq qual (1+ num-pos))))))
                         0)))
          (format nil "~A ^[~A~D%^]"
                  essid (bar-zone-color perc 40 30 15 t) perc)))))

;;; Add mode-line formatter

(add-screen-mode-line-formatter #\I #'fmt-wifi)

;;; EOF

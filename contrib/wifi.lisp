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
;;; along with this software; see the file COPYING.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;

;;; WARNING:
;;;
;;; This triggers a yet to be discovered bug in SBCL, which causes
;;; stumpwm to freeze.

;;; USAGE:
;;;
;;; Place the following in your ~/.stumpwmrc file:
;;;
;;;     (load-module "wifi")
;;;
;;; Then you can use "%I" in your mode line format (both "w" and "W"
;;; were taken. Think _I_EEE 802.11 :-)).
;;;
;;; Notes: This gets information through sysfs, so it only works on
;;; Linux with a mounted sysfs.

(defpackage :stumpwm.contrib.wifi
  (:use :common-lisp :stumpwm )
  (:export #:*iw-path*
           #:*wireless-device*
           #:*wireless-cache-interval*))
(in-package :stumpwm.contrib.wifi)

(defvar *iw-path* "/sbin/iw"
  "Location if iw, defaults to /sbin/iw.")

(defvar *wireless-device* nil
  "Set to the name of the wireless device you want to monitor. If set
  to NIL, try to guess.")

(defvar *wireless-cache-interval* 5
  "The number of seconds that can pass before refreshing.")

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

(defun guess-wireless-device ()
  (multiple-value-bind (match? sub)
      (cl-ppcre:scan-to-strings
       "Interface (.*)"
       (run-shell-command (format nil "~A dev" *iw-path*) t))
    (if match?
        (aref sub 0)
        (error "No wireless device found."))))

(defun read-wifi-info (device)
  (let ((info (run-shell-command (format nil "~A dev ~A link" *iw-path* device) t)))
    (list
     :ssid
     (multiple-value-bind (match? sub)
         (cl-ppcre:scan-to-strings "SSID: (.*)" info)
       (if match? (aref sub 0) nil))
     :freq
     (multiple-value-bind (match? sub)
         (cl-ppcre:scan-to-strings "freq: (.*)" info)
       (if match? (parse-integer (aref sub 0)) nil))
     :signal
     (multiple-value-bind (match? sub)
         (cl-ppcre:scan-to-strings "signal: (.*) dBm" info)
       (if match? (parse-integer (aref sub 0)) nil))
     :tx-bitrate
     (multiple-value-bind (match? sub)
         (cl-ppcre:scan-to-strings "tx bitrate: (.*)" info)
       (if match? (aref sub 0) nil)))))


(defun color-signal-strength (signal-strength)
  (bar-zone-color
   (if (numberp signal-strength)
       signal-strength
       0)
   -50 -70 -80 t))

(defun-cached fmt-wifi *wireless-cache-interval* (ml)
  "Formatter for wifi status. Displays the ESSID of the access point
you're connected to as well as the signal strength. When no valid data
is found, just displays nil."
  (declare (ignore ml))
  (handler-case
      (Let* ((device (or *wireless-device* (guess-wireless-device))))
        (destructuring-bind (&key ssid freq signal tx-bitrate)
            (read-wifi-info device)
            (format nil "~A ^[~A~D dBm^]" ssid (color-signal-strength signal) signal)))
    ;; CLISP has annoying newlines in their error messages... Just
    ;; print a string showing our confusion.
    (t (c) (format nil "~A" c))))

(defun-cached fmt-wifi-short *wireless-cache-interval* (ml)
  "Formatter for wifi status. Displays the signal strength. When no
valid data is found, just displays nil."
  (declare (ignore ml))
  (handler-case
      (let* ((device (or *wireless-device* (guess-wireless-device))))
        (destructuring-bind (&key ssid freq signal tx-bitrate)
            (read-wifi-info device)
            (format nil "^[~A~D dBm^]" (color-signal-strength signal) signal)))
    ;; CLISP has annoying newlines in their error messages... Just
    ;; print a string showing our confusion.
    (t (c) (format nil "~A" c))))

;;; Add mode-line formatter

(add-screen-mode-line-formatter #\I #'fmt-wifi)

(add-screen-mode-line-formatter #\i #'fmt-wifi-short)

;;; EOF

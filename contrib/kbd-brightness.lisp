;;; Adjust keyboard brightness.
;;;
;;; Copyright 2013 Russell Sim <russell.sim@gmail.com>
;;;
;;; Maintainer: Russell Sim
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
;;;
;;; REQUIRES:
;;; dbus -- https://github.com/lucashpandolfo/dbus
;;;
;;; USAGE:
;;;
;;; Put:
;;;
;;;     (load-module "kdb-brightness")
;;;     (define-key *top-map* (kbd "XF86KbdBrightnessUp") "kbd-brightness 25")
;;;     (define-key *top-map* (kbd "XF86KbdBrightnessDown") "kbd-brightness -25")
;;;
;;; In your ~/.stumpwmrc
;;;
;;;
;;; If you would like the current brightness value to be displayed
;;; then add the following to your ~/.stumpwmrc
;;;
;;;     (setf stumpwm.contrib.kbd-brightness:*keyboard-brightness-display* t)

(defpackage #:stumpwm.contrib.kbd-brightness
  (:use #:cl)
  (:import-from #:stumpwm
                #:defcommand)
  (:import-from #:dbus
                #:system-server-addresses
                #:with-open-bus
                #:with-introspected-object)
  (:export
   #:get-brightness
   #:set-brightness
   #:max-brightness
   #:*keyboard-brightness-display*))

(in-package #:stumpwm.contrib.kbd-brightness)

(defvar *keyboard-brightness-display* nil)

(defmacro with-kbd-backlight ((backlight) &body body)
  `(with-open-bus (bus (system-server-addresses))
    (with-introspected-object (,backlight bus "/org/freedesktop/UPower/KbdBacklight" "org.freedesktop.UPower")
      ,@body)))

(defun get-brightness (backlight)
  (funcall backlight "org.freedesktop.UPower.KbdBacklight" "GetBrightness"))

(defun max-brightness (backlight)
  (funcall backlight "org.freedesktop.UPower.KbdBacklight" "GetMaxBrightness"))

(defun set-brightness (backlight value)
  (funcall backlight "org.freedesktop.UPower.KbdBacklight" "SetBrightness" value))

(defcommand kbd-brightness (inc) ((:number "Adjust brightness: "))
  "adjust the keyboard brightness."
  (with-kbd-backlight (backlight)
    (let* ((current (get-brightness #'backlight))
           (max (max-brightness #'backlight))
           (min 0)
           (new (let ((possible-value (+ current inc)))
                  (cond
                    ((not (plusp possible-value)) min)
                    ((> possible-value max) max)
                    (t possible-value)))))
      (set-brightness #'backlight new)
      (when *keyboard-brightness-display*
        (format nil "Keyboard Brightness: ~d%" (round (* (/ new max) 100)))))))

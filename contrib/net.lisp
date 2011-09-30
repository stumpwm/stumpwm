;;; Network activity formatter for the mode-line
;;;
;;; Copyright 2009 Vitaly Mayatskikh
;;;
;;; Maintainer:
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
;;;     (load "/path/to/net.lisp")
;;;
;;; In your ~/.stumpwmrc
;;;
;;; Then you can use "%l" in your mode line format.
;;;
;;; NOTES:
;;;
;;; This is specific to Linux.

(defpackage :stumpwm.contrib.net
  (:use :common-lisp :stumpwm :cl-ppcre)
  (:export #:*net-device*))

(in-package :stumpwm.contrib.net)

;; Install formatters.
(dolist (a '((#\l fmt-net-usage)))
  (pushnew a *screen-mode-line-formatters* :test 'equal))

(defvar *net-device* nil) ; nil means auto. or specify explicitly, i.e. "wlan0"
(defvar *net-last-rx* 0)
(defvar *net-last-tx* 0)
(defvar *net-last-time* nil)
(defvar *net-rx* nil)
(defvar *net-tx* nil)
(defvar *net-time* nil)

;; stuff for parsing /proc/net/route
(defconstant +iface+ 0)
(defconstant +destination+ 1)
(defconstant +gateway+ 2)
(defconstant +flags+ 3)
(defconstant +mask+ 7)
(defconstant +ipv4-zero+ "00000000")

(defun now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defvar *last-route-rescan-time* (now))
(defvar *last-route-device* nil)

(defun find-default ()
  "Tries to found device with default route. NIL if none."
  (with-open-file (file #P"/proc/net/route" :if-does-not-exist nil)
    (when file
      (read-line file nil) ; skip desc
      (loop :as line = (read-line file nil)
	 :when (null line) :return nil
	 :do
	 (let ((split (cl-ppcre:split "\\s+" line)))
	   (when (and (string= (nth +destination+ split) +ipv4-zero+)
		      (string= (nth +mask+ split) +ipv4-zero+)
		      (logand (parse-integer (nth +flags+ split) :junk-allowed t) 2))
	     (return (nth +iface+ split))))))))

(defun net-device ()
  "Returns statically assigned device name or tries to find it be default gw.
For the second case rescans route table every minute."
  (if *net-device*
      *net-device*
      (if (and *last-route-device*
	       (< (- (now) *last-route-rescan-time*) 60))
	  *last-route-device*
	  (let ((new-device (or (find-default) "lo")))
	    (when (string/= new-device *last-route-device*)
		(setq *net-last-tx* 0
		      *net-last-rx* 0
		      *net-last-time* nil
		      *net-rx* nil
		      *net-tx* nil
		      *net-time* nil))
	    (setq *last-route-rescan-time* (now)
		  *last-route-device* new-device)))))

(defun net-sys-stat-read (device stat-file)
  (with-open-file (file (concatenate 'string "/sys/class/net/"
				     device
				     "/statistics/"
				     stat-file) :if-does-not-exist nil)
    (if file
	(parse-integer (read-line-from-sysfs file) :junk-allowed t)
	(progn (setq *net-device* nil
		     *last-route-device* nil)
	       0))))

(defun net-usage ()
  "Returns a list of 2 values: rx and tx bytes/second."
  (let ((now (now))
	(rx-s 0.0)
	(tx-s 0.0)
	(t-s 0.1) ; don't want division by zero
	(rx (net-sys-stat-read (net-device) "rx_bytes"))
	(tx (net-sys-stat-read (net-device) "tx_bytes")))

    (when (and *net-last-time* (> (- now *net-last-time*) 0.0))
      (let ((drx (/ (- rx *net-last-rx*)
		    (- now *net-last-time*)))
	    (dtx (/ (- tx *net-last-tx*)
		    (- now *net-last-time*))))

	(push drx *net-rx*)
	(push dtx *net-tx*)
	(push now *net-time*)

	(when (> (length *net-time*) 1)
	  (dotimes (i (1- (length *net-time*)))
	    (let ((dt (- (nth (1+ i) *net-time*)
			 (nth i *net-time*)))
		  (rx (nth i *net-rx*))
		  (tx (nth i *net-tx*)))
	      (incf rx-s (* rx dt))
	      (incf tx-s (* tx dt))
	      (incf t-s dt)))
	  ;; cut off old values
	  (when (> (length *net-time*) 5)
	    (pop *net-rx*)
	    (pop *net-tx*)
	    (pop *net-time*)))))

      (setq *net-last-rx* rx
	    *net-last-tx* tx
	    *net-last-time* now)

      (list (round (/ rx-s t-s))
	    (round (/ tx-s t-s)))))

(defun fmt-net-usage (ml)
  "Returns a string representing the current network activity."
  (declare (ignore ml))
  (let ((net (net-usage))
	dn up)
    (defun kbmb (x y)
      (if (>= (/ x 1e6) y)
	  (list (/ x 1e6) "m")
	  (list (/ x 1e3) "k")))
    (setq dn (kbmb (car net) 0.1)
	  up (kbmb (cadr net) 0.1))
    (format nil "~A: ~5,2F~A/~5,2F~A " (net-device)
	    (car dn) (cadr dn) (car up) (cadr up))))

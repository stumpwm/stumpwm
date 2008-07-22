;;; Portable battery information for StumpWM's mode-line.
;;; 
;;; Written by Julian Stecklina with inspiration from John Li and
;;; Rupert Swarbrick.
;;; 
;;; Copyright (c) 2008 Julian Stecklina
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; 
;;;
;;; To load this module, place
;;;
;;;     (load-contrib "battery-portable")
;;;
;;; in your .stumpwmrc. Battery information is then available via %B
;;; in your mode-line config.

(defpackage :stumpwm.contrib.battery-portable
  (:use :common-lisp :stumpwm :cl-ppcre)
  (:export #:*refresh-time*
	   #:*prefer-sysfs*
	   ))
(in-package :stumpwm.contrib.battery-linux)

;;; Configuration

(defvar *refresh-time* 5
  "Time in seconds between updates of battery information.")

(defvar *prefer-sysfs* t
  "Prefer sysfs over procfs for information gathering. This has effect
  only on Linux.")

;;; Globals

(defparameter *battery-methods* nil
  "Available methods to query battery information.")

;;; Method base class

(defclass battery-method ()
  ()
  (:documentation "Base class for battery information retrieval"))

(defgeneric all-batteries (method)
  (:documentation "Returns all recognized batteries."))

(defgeneric preference-value (method)
  (:method ((m battery-method)) 0)
  (:documentation "Returns an integer. Larger values indicate a method
  with higher preference. Default is 0."))

(defun preferred-battery-method ()
  (make-instance
   (first (sort *battery-methods* #'> :key #'preference-value))))

;;; Battery class

(defclass battery ()
  ()
  (:documentation "Base class for battery information."))

(defgeneric state-of (battery)
  (:documentation "Returns either :UNKNOWN, :CHARGED, :CHARGING,
  or :DISCHARGING with the obvious meanings. If the state is
  not :UNKNOWN, returns the battery fill percentage. If the state
  is :CHARGING or :DISCHARGING, this function returns a third value
  indicating the corresponding time in seconds."))

;;; Linux sysfs implementation

(defclass sysfs-method (battery-method)
  ()
  (:documentation "Collect battery information through Linux'
  class-based sysfs interface."))

(pushnew 'sysfs-method *battery-methods*)

(defmethod preference-value ((m sysfs-method))
  (if *prefer-sysfs* 100 -100))


(defclass sysfs-battery (battery)
  ((path :initarg :path :initform (error ":path missing")
	 :reader path-of)))

(defun sysfs-field (path name)
  (with-open-file (file (merge-pathnames (make-pathname :name name)
					 path))
    (read-line-from-sysfs file)))

(defun sysfs-int-field (path name)
  (parse-integer (sysfs-field path name) :junk-allowed t))

(defmethod all-batteries ((m sysfs-method))
  (remove nil
	  (mapcar (lambda (path)
		    (handler-case
			(when (string= "Battery"
				       (sysfs-field path "type"))
			  (make-instance 'sysfs-battery
					 :path path))
		      (file-error () nil)))
		  (directory 
		   (make-pathname :directory '(:absolute "sys" "class" "power_supply" :wild))))))

(defmethod state-of ((battery sysfs-battery))
  (handler-case
      (let ((path (path-of battery)))
	(if (string= (sysfs-field path "present") "0")
	    :unknown
	    (let* ((state (sysfs-field path "status"))
		   (consumption (sysfs-int-field path "current_now"))
		   (curr (sysfs-int-field path "energy_now"))
		   (full (sysfs-int-field path "energy_full"))
		   (percent (* 100 (/ curr full))))
	      (cond
		((string= state "Full") (values :charged percent))
		((string= state "Discharging")
		 (values :discharging percent
			 (if (zerop consumption)
			     0
			     (* 3600 (/ curr consumption)))))
		((string= state "Charging")
		 (values :charging percent
			 (if (zerop consumption)
			     0
			     (* 3600 (/ (- full curr) consumption)))))
		(t :unknown)))))
    (t () :unknown)))

;;; Interface to the outside world.

(defun fmt-time (stream arg colonp atp)
  (declare (ignore colonp atp))
  (multiple-value-bind (hours rest)
      (truncate arg 3600) 
  (format stream "~D:~2,'0D" hours (floor rest 60))))

(defun battery-info-string ()
  "Compiles a string suitable for StumpWM's mode-line."
  (with-output-to-string (fmt)
    (let ((batteries (all-batteries (or (preferred-battery-method)
					(return-from battery-info-string
					  "(not implemented)")))))
      (if (endp batteries)
	  (format fmt "(no battery)")
	  (loop 
	     for bat in batteries
	     do (multiple-value-bind (state perc time)
		    (state-of bat)
		  (ecase state
		    (:unknown (format fmt "(no info)"))
		    (:charged (format fmt "~~ ~D%" (round perc)))
		    ((:charging :discharging) 
		     (format fmt "~/stumpwm.contrib.battery-portable::fmt-time/~A ^[~A~D%^]"
			     time
			     (if (eq state :charging) #\+ #\-)
			     (bar-zone-color perc 20 50 90
					     (eq state :discharging))
			     (round perc))))))))))

;;; The actual mode-line format function. A bit ugly...
(let ((next 0)
      (last-value ""))
  (defun fmt-bat (ml)
    (declare (ignore ml))
    ;; Return the last info again, if we are called too quickly.
    (let ((now (get-universal-time)))
      (when (< now next)
	(return-from fmt-bat last-value))
      (setf next (+ now *refresh-time*)))
    ;; Generate info string.
    (setf last-value (battery-info-string))))

;;; Put this at the end to avoid evaluating it when the core above
;;; throws an error.
(pushnew '(#\B fmt-bat) *screen-mode-line-formatters* :test 'equal)

;;; EOF

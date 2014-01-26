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
;;;     (load-module "battery-portable")
;;;
;;; in your .stumpwmrc. Battery information is then available via %B
;;; in your mode-line config.
;;;
;;; If you would like to use D-Bus to interact with UPower, add
;;;
;;;     (setf stumpwm.contrib.battery-portable:*prefer-dbus* t)
;;;     (setf stumpwm.contrib.battery-portable:*prefer-sysfs* nil)
;;;
;;; below the above line. This requires the dbus package to be installed,
;;; either from death/dbus, lucashpandolfo/dbus or joseph-gay/dbus on github,
;;; I use quicklisp (and local-projects) to accomplish this, and I add
;;;
;;;     (ql:quickload :dbus)
;;;
;;; above the previous settings.
;;;
;;; If you have an older kernel and the above doesn't work, add
;;;
;;;     (setf stumpwm.contrib.battery-portable:*prefer-sysfs* nil)
;;;
;;; below the above line.

(defpackage :stumpwm.contrib.battery-portable
  (:use :common-lisp :stumpwm :cl-ppcre)
  (:export #:*refresh-time*
           #:*prefer-sysfs*
           #:*prefer-dbus*
           ))
(in-package :stumpwm.contrib.battery-portable)

;;; CLISP doesn't include :linux in *features* even if it runs on
;;; Linux. :-/

;;; Configuration

(defvar *refresh-time* 5
  "Time in seconds between updates of battery information.")

(defvar *prefer-sysfs* t
  "Prefer sysfs over procfs for information gathering. This has effect
  only on Linux.")

(defvar *prefer-dbus* NIL
  "Prefer DBus over sysfs and procfs for information gathering. This should
  effect any system that has DBus and UPower installed.
  This requires the :dbus package.")

;;; Method base class

(defclass battery-method ()
  ()
  (:documentation "Base class for battery information retrieval"))

(defgeneric all-batteries (method)
  (:documentation "Returns all recognized batteries."))

(defun preferred-battery-method ()
  #- (or linux openbsd)
  nil
  #+ linux
  (cond
    (*prefer-sysfs* (make-instance 'sysfs-method))
    (*prefer-dbus* (if (find-package :dbus)
                       (make-instance 'dbus-method)
                       nil))
    (t (make-instance 'procfs-method)))
  #+ openbsd
  (make-instance 'usr-sbin-apm-method))

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

;;; Linux procfs implementation

#+ linux
(progn
  (defclass procfs-method (battery-method)
    ()
    (:documentation "Collect battery information through Linux' procfs interface."))

  (defclass procfs-battery (battery)
    ((path :initarg :path :initform (error ":path missing")
           :reader path-of)
     (info-hash :initform (make-hash-table :test 'equal)
                :reader info-hash-of)))

  (defmethod update-info ((battery procfs-battery))
    (clrhash (info-hash-of battery))
    (loop
       for filename in '("state" "info")
       do (with-open-file (file (merge-pathnames (make-pathname :name filename)
                                                 (path-of battery)))
            (loop
               for line = (read-line file nil nil)
               while line
               do (multiple-value-bind (match? matches)
                      (scan-to-strings "^([^:]+):\\s*([^\\s]+)(\\s.*)?$" line)
                    (if (not match?)
                        (format t "Unrecognized line: ~S~%" line)
                        (setf (gethash (aref matches 0) (info-hash-of battery))
                              (aref matches 1))))))))

  (define-condition info-value-not-present (error)
    ())

  (defmethod info-value ((battery procfs-battery) key)
    (multiple-value-bind (val found?)
        (gethash key (info-hash-of battery))
        (if found?
            val
            (error 'info-value-not-present))))

  (defmethod info-value-int ((battery procfs-battery) key)
    (values (parse-integer (info-value battery key))))

  (defmethod all-batteries ((method procfs-method))
    (mapcar (lambda (p)
              (make-instance 'procfs-battery :path p))
            (list-directory "/proc/acpi/battery/")))

  (defmethod state-of ((battery procfs-battery))
    (handler-case
        (progn
          (update-info battery)
          (if (string/= (info-value battery "present") "yes")
              :unknown
              (let* ((state (info-value battery "charging state")))
                (flet ((percent ()
                         (/ (info-value-int battery "remaining capacity")
                            (info-value-int battery "last full capacity"))))

                (cond
                  ((string= state "charged") (values :charged (percent)))
                  ((string= state "discharging")
                   (values :discharging (percent)
                           (* 3600 (/ (info-value-int battery "remaining capacity")
                                      (info-value-int battery "present rate")))))
                  ((string= state "charging")
                   (values :charging (percent)
                           (* 3600 (/ (- (info-value-int battery "last full capacity")
                                         (info-value-int battery "remaining capacity"))
                                      (info-value-int battery "present rate")))))
                  (t :unknown))))))
      (t () :unknown))))

;;; Linux sysfs implementation

#+ linux
(progn

  (defclass sysfs-method (battery-method)
    ()
    (:documentation "Collect battery information through Linux'
  class-based sysfs interface."))

  (defclass sysfs-battery (battery)
    ((path :initarg :path :initform (error ":path missing")
           :reader path-of)))

  (defun sysfs-field-exists? (path name)
    (probe-file (merge-pathnames (make-pathname :name name)
                                 path)))

  (defun sysfs-field (path name)
    (with-open-file (file (merge-pathnames (make-pathname :name name)
                                           path))
      (read-line-from-sysfs file)))

  (defun sysfs-int-field (path name)
    (parse-integer (sysfs-field path name) :junk-allowed t))

  (defun sysfs-int-field-or-nil (path name)
    (if (sysfs-field-exists? path name)
        (sysfs-int-field path name)
        nil))

  (defmethod all-batteries ((m sysfs-method))
    (remove nil
            (mapcar (lambda (path)
                      (handler-case
                          (when (string= "Battery"
                                         (sysfs-field path "type"))
                            (make-instance 'sysfs-battery
                                           :path path))
                        (file-error () nil)))
                    (list-directory "/sys/class/power_supply/"))))

  (defmethod state-of ((battery sysfs-battery))
    (handler-case
        (let ((path (path-of battery)))
          (if (string= (sysfs-field path "present") "0")
              :unknown
              (let* ((state (sysfs-field path "status"))
                     (consumption (or (sysfs-int-field-or-nil path "power_now")
                                      (sysfs-int-field-or-nil path "current_now")
                                      (return-from state-of :unknown)))
                     (curr (or (sysfs-int-field-or-nil path "energy_now")
                               ;; energy_* seems not to be there on
                               ;; some boxes. Strange...
                               (sysfs-int-field-or-nil path "charge_now")
                               (return-from state-of :unknown)))
                     (full (or (sysfs-int-field-or-nil path "energy_full")
                               (sysfs-int-field-or-nil path "charge_full")
                               (return-from state-of :unknown)))
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
      (t () :unknown))))

;;; DBus implementation
#+ linux
(progn

  (defclass dbus-method (battery-method)
    ()
    (:documentation "Collect battery information through DBus interface."))

  (defclass dbus-battery (battery)
    ((object :initarg :object :initform
	     (error ":object missing for dbus battery")
	     :reader object-of)))

  (defun list-dbus-batteries (path destination)
    "List the batteries that UPower knows about, ignoring line power objects.
An example PATH is /org/freedesktop/UPower and an example DESTINATION is org.freedesktop.UPower"
    (dbus::with-open-bus
	(bus (dbus::system-server-addresses))
      (remove nil
	      (mapcar (lambda (path)
			(if (cl-ppcre:scan ".*battery.*" path)
			    path))
		      (dbus::with-introspected-object (bat0 bus path destination)
			(bat0 destination "EnumerateDevices"))))))

  (defmethod all-batteries ((method dbus-method))
    (remove nil
	    (mapcar (lambda (path)
		      (make-instance 'dbus-battery
				     :object
				     (let ((dbus_conn
					    (dbus:open-connection
					     (make-instance 'iolib.multiplex:event-base)
					     (dbus::system-server-addresses))))
				       (dbus::authenticate
					(dbus:supported-authentication-mechanisms dbus_conn) dbus_conn)
				       (dbus:hello dbus_conn)
				       (dbus::make-object-from-introspection
					dbus_conn
					path "org.freedesktop.UPower"))))
		    (list-dbus-batteries "/org/freedesktop/UPower" "org.freedesktop.UPower"))))

  (defmethod state-of ((battery dbus-battery))
    (let ((state (dbus::object-invoke (object-of battery)
				      "org.freedesktop.DBus.Properties" "Get"
				      "org.freedesktop.UPower.Device" "State"))
	  (percent (dbus::object-invoke (object-of battery)
					"org.freedesktop.DBus.Properties" "Get"
					"org.freedesktop.UPower.Device" "Percentage"))
	  (time-to-empty (dbus::object-invoke (object-of battery)
					      "org.freedesktop.DBus.Properties" "Get"
					      "org.freedesktop.UPower.Device" "TimeToEmpty"))
	  (time-to-full (dbus::object-invoke (object-of battery)
					     "org.freedesktop.DBus.Properties" "Get"
					     "org.freedesktop.UPower.Device" "TimeToFull")))
      (cond ((equalp state 0) :unknown)
	    ((equalp state 1) (values :charging percent time-to-full))
	    ((equalp state 2) (values :discharging percent time-to-empty))
	    ((equalp state 3) :unknown)
	    ((equalp state 4) (values :charged percent))
	    ((equalp state 5) (values :charging percent time-to-full)) ; pending-charge
	    ((equalp state 6) (values :discharging percent time-to-empty))))) ;pending-discharge

)

;;; OpenBSD /usr/sbin/apm implementation

#+ openbsd
(progn
  (defclass usr-sbin-apm-method (battery-method) ()
    (:documentation "Collect battery information through OpenBSD' /usr/sbin/apm program."))

  (defclass usr-sbin-apm-battery (battery) ())

  (defun read-usr-sbin-apm-info ()
    (with-input-from-string (apm (run-shell-command "/usr/sbin/apm -ablm" t))
      (let* ((state (ignore-errors (parse-integer (read-line apm))))
             (percent (ignore-errors (parse-integer (read-line apm))))
             (minutes (ignore-errors (parse-integer (read-line apm))))
             (ac (ignore-errors (parse-integer (read-line apm)))))
        (unless (and (or (null state) (eql state 4))
                     (or (null ac) (eql ac 255)))
          (values (case state
                    (0 :high)
                    (1 :low)
                    (2 :critical)
                    (3 :charging)
                    (4 :absent)
                    (t :unknown))
                  percent
                  minutes
                  (case ac
                    (0 :disconnected)
                    (1 :connected)
                    (2 :backup)
                    (t :unknown)))))))

  (defmethod all-batteries ((method usr-sbin-apm-method))
    (unless (null (read-usr-sbin-apm-info))
      (list (make-instance 'usr-sbin-apm-battery))))

  (defmethod state-of ((battery usr-sbin-apm-battery))
    (multiple-value-bind (state percent minutes ac)
        (read-usr-sbin-apm-info)
      (let ((percent (or percent 0))
            (seconds (when minutes (* minutes 60))))
        (case ac
          ((:disconnected :backup)
           (values :discharging percent seconds))
          (:connected
           (cond
             ((or (eql state :absent)
                  (eql state :unknown))
              (values :unknown))
             ((eql percent 100)
              (values :charged percent))
             (t
              (values :charging percent seconds))))
          (t
           (values :unknown)))))))

;;; Interface to the outside world.

(defun fmt-time (stream arg colonp atp)
  (declare (ignore colonp atp))
  (when (numberp arg)
    (multiple-value-bind (hours rest)
        (truncate arg 3600)
      (format stream "~D:~2,'0D" hours (floor rest 60)))))

;;; To avoid generating an infinite number of connections or objects, we should generate it once
;;; and then reuse the battery objects
(let* ((pref-method (preferred-battery-method))
       (batteries
	(if pref-method
	    (all-batteries pref-method)
	    "(not implemented)")))
  (defun battery-info-string ()
    "Compiles a string suitable for StumpWM's mode-line."
    (with-output-to-string (fmt)
      (if (and (stringp batteries)
	       (string-equal batteries "(not implemented)"))
	  (return-from battery-info-string "(not implemented)"))
; If the preferred battery method changes, regenerate the batteries list
      (if (not (eq (class-of (preferred-battery-method)) (class-of (preferred-battery-method))))
	  (progn
	    (setf pref-method (preferred-battery-method))
	    (setf batteries
		  (if pref-method
		      (all-batteries pref-method)
		      (return-from battery-info-string "(not implemented)")))))
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
			     (bar-zone-color perc 90 50 20 t)
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

(add-screen-mode-line-formatter #\B #'fmt-bat)

;;; EOF

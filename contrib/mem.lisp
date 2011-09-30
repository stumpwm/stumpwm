;;; MEM formatters for the mode-line
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
;;;     (load "/path/to/mem.lisp")
;;;
;;; In your ~/.stumpwmrc
;;;
;;; Then you can use "%M" and/or "%N in your mode line format.
;;;
;;; NOTES:
;;;
;;; This is specific to Linux.

(defpackage :stumpwm.contrib.mem
  (:use :common-lisp :stumpwm :cl-ppcre))

(in-package :stumpwm.contrib.mem)

;; Install formatters.
(dolist (a '((#\M fmt-mem-usage)
             (#\N fmt-mem-usage-bar)))
  (pushnew a *screen-mode-line-formatters* :test 'equal))

;; Defaults arguments for fmt-mem-usage-bar
(defvar *mem-usage-bar-width* 10)
(defvar *mem-usage-bar-full* #\#)
(defvar *mem-usage-bar-empty* #\:)

(defun get-proc-fd-field (s field)
  (if s
      (do ((line (read-line s nil nil) (read-line s nil nil)))
	  ((null line) nil)
	(let ((split (cl-ppcre:split "\\s*:\\s*" line)))
	  (when (string= (car split) field) (return (cadr split)))))
      ""))

(defun mem-usage ()
  "Returns a list containing 3 values:
total amount of memory, allocated memory, allocated/total ratio"
  (let ((allocated 0))
    (multiple-value-bind (mem-total mem-free buffers cached)
	(with-open-file (file #P"/proc/meminfo" :if-does-not-exist nil)
	  (values
	   (read-from-string (get-proc-fd-field file "MemTotal"))
	   (read-from-string (get-proc-fd-field file "MemFree"))
	   (read-from-string (get-proc-fd-field file "Buffers"))
	   (read-from-string (get-proc-fd-field file "Cached"))))
      (setq allocated (- mem-total (+ mem-free buffers cached)))
      (list mem-total allocated (/ allocated mem-total)))))

(defun fmt-mem-usage (ml)
  "Returns a string representing the current percent of used memory."
  (declare (ignore ml))
  (let* ((mem (mem-usage))
	 (|%| (truncate (* 100 (nth 2 mem))))
	 (allocated (truncate (/ (nth 1 mem) 1000))))
    (format nil "MEM: ~4D mb ^[~A~3D%^] " allocated (bar-zone-color |%|) |%|)))

(defun fmt-mem-usage-bar (ml &optional (width *mem-usage-bar-width*) (full *mem-usage-bar-full*) (empty *mem-usage-bar-empty*))
  "Returns a coloured bar-graph representing the current allocation of memory."
  (declare (ignore ml))
  (let ((cpu (truncate (* 100 (nth 2 (mem-usage))))))
    (stumpwm::bar cpu width full empty)))

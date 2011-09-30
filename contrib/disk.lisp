;;; Disk usage monitoring for stumpwm's modeline
;;;
;;; Copyright 2007 Morgan Veyret.
;;;
;;; Maintainer: Morgan Veyret
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
;;;     (load-module "disk")
;;;
;;; ...into your ~/.stumpwmrc
;;;
;;; Then you can use "%D" in your mode line format.
;;; You can customize the modeline format (*disk-modeline-fmt*). See the
;;; documentation for *disk-modeline-fmt* for more information.

;;; CODE:
(in-package :stumpwm)

(dolist (a '((#\D disk-modeline)))
  (pushnew a *screen-mode-line-formatters* :test 'equal))

(defvar *disk-usage* nil)

(defun disk-usage-tokenize (usage-line-str)
  (ppcre:split "(\\s+)" usage-line-str))

(defun disk-update-usage (paths)
  (setf *disk-usage*
        (with-input-from-string
            (usage-str (run-shell-command
                        (format nil "df -h ~{~a ~} | grep -v 'Filesystem'" paths) t))
          (loop for i = (read-line usage-str nil nil)
             while i
             collect (disk-usage-tokenize i)))))

(defvar *disk-usage-paths* '("/"))


(defun disk-usage-get-field (path field-number)
  (let ((usage-infos (find-if (lambda (item)
                                (string= (car (last item)) path))
                              *disk-usage*)))
    (nth field-number usage-infos)))
(defun disk-get-device (path)
  (disk-usage-get-field path 0))
(defun disk-get-size (path)
  (disk-usage-get-field path 1))
(defun disk-get-used (path)
  (disk-usage-get-field path 2))
(defun disk-get-available (path)
  (disk-usage-get-field path 3))
(defun disk-get-use-percent (path)
  (disk-usage-get-field path 4))
(defun disk-get-mount-point (path)
  (disk-usage-get-field path 5))

(defun disk-modeline (ml)
  (declare (ignore ml))
  (disk-update-usage *disk-usage-paths*)
  (let ((fmts (loop for p in *disk-usage-paths*
                   collect (format-expand *disk-formatters-alist*
                                          *disk-modeline-fmt*
                                          p))))
    (format nil "~{~a ~}" fmts)))

(defvar *disk-formatters-alist*
  '((#\d disk-get-device)
    (#\s disk-get-size)
    (#\u disk-get-used)
    (#\a disk-get-available)
    (#\p disk-get-use-percent)
    (#\m disk-get-mount-point)))

(defvar *disk-modeline-fmt* "%m: %u/%s"
  "The default value for displaying disk usage information on the modeline.

@table @asis
@item %%
A literal '%'
@item %d
Filesystem device
@item %s
Filesystem size
@item %u
Filesystem used space
@item %a
Filesystem available space
@item %p
Filesystem used space in percent
@item %m
Filesystem mount point
@end table
")


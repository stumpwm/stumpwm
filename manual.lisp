;; Copyright (C) 2003 Shawn Betts
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
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; Generate the texinfo manual from docstrings in the source. Note,
;; this only works in sbcl.
;;
;; Code:

(in-package :stumpwm)

(defun generate-function-doc (s line)
  (ppcre:register-groups-bind (name) ("^@@@ (.*)" line)
			      (let ((fn (if (find #\( name :test 'char=)
					    ;; handle (setf <symbol>) functions
					    (with-standard-io-syntax
					      (let ((*package* (find-package :stumpwm)))
						(fdefinition (read-from-string name))))
					    (symbol-function (find-symbol (string-upcase name) :stumpwm))))
				    (*print-pretty* nil))
				(format s "@defun ~a ~{~a~^ ~}~%~a~&@end defun~%~%"
					name (sb-impl::%simple-fun-arglist fn)
					(documentation fn 'function))
				t)))

(defun generate-macro-doc (s line)
  (declare (ignore s line)))

(defun generate-variable-doc (s line)
  (ppcre:register-groups-bind (name) ("^### (.*)" line)
			      (let ((sym (find-symbol (string-upcase name) :stumpwm)))
				(format s "@defvar ~a~%~a~&@end defvar~%~%"
					name (documentation sym 'variable))
				t)))

(defun generate-hook-doc (s line)
  (ppcre:register-groups-bind (name) ("^\\$\\$\\$ (.*)" line)
			      (let ((sym (find-symbol (string-upcase name) :stumpwm)))
				(format s "@defvr {Hook} ~a~%~a~&@end defvr~%~%"
					name (documentation sym 'variable))
				t)))

(defun generate-command-doc (s line)
  (ppcre:register-groups-bind (name) ("^!!! (.*)" line)
			      (let ((cmd (gethash name *command-hash*)))
				(format s "@deffn {Command} ~a ~{~a~^ ~}~%~a~&@end deffn~%~%"
					name (mapcar 'first (command-args cmd)) (command-docstring cmd))
				t)))

(defun generate-manual (&key (in #p"stumpwm.texi.in") (out #p"stumpwm.texi"))
  (with-open-file (os out :direction :output :if-exists :supersede)
    (with-open-file (is in :direction :input)
      (loop for line = (read-line is nil is)
	 until (eq line is) do
	   (or (generate-function-doc os line)
	       (generate-macro-doc os line)
	       (generate-hook-doc os line)
	       (generate-variable-doc os line)
	       (generate-command-doc os line)
	       (write-line line os))))))

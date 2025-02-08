;; Copyright (C) 2003-2008 Shawn Betts
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

;;; Commentary:
;;
;; This file contains the code for debugging stumpwm.
;;
;;; Code:

(in-package #:stumpwm)

(defvar *debug-level* 0
  "Set this variable to a number > 0 to turn on debugging. The greater the number the more debugging output.")

(defvar *debug-expose-events* nil
  "Set this variable for a visual indication of expose events on internal StumpWM windows.")

(defvar *debug-stream* (make-synonym-stream '*error-output*)
  "This is the stream debugging output is sent to. It defaults to
*error-output*. It may be more convenient for you to pipe debugging
output directly to a file.")

(defun dformat (level fmt &rest args)
  (when (>= *debug-level* level)
    (multiple-value-bind (sec m h) (get-decoded-system-time)
      (format *debug-stream* "~2,'0d:~2,'0d:~2,'0d ~2,' d " h m sec level))
    ;; strip out non base-char chars quick-n-dirty like
    (write-string (map 'string (lambda (ch)
                                 (if (typep ch 'standard-char)
                                     ch #\?))
                       (apply 'format nil fmt args))
                  *debug-stream*)
    (force-output *debug-stream*)))

(defvar *redirect-stream* nil
  "This variable Keeps track of the stream all output is sent to when
`redirect-all-output' is called so if it changes we can close it
before reopening.")

(defun redirect-all-output (file)
  "Elect to redirect all output to the specified file. For instance,
if you want everything to go to ~/.stumpwm.d/debug-output.txt you would
do:

@example
(redirect-all-output (data-dir-file \"debug-output\" \"txt\"))
@end example
"
  (when (typep *redirect-stream* 'file-stream)
    (close *redirect-stream*))
  (setf *redirect-stream* (open file :direction :output :if-exists :append :if-does-not-exist :create)
        *error-output*    *redirect-stream*
        *standard-output* *redirect-stream*
        *trace-output*    *redirect-stream*
        *debug-stream*    *redirect-stream*))

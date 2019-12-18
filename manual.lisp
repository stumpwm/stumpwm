;; Copyright (C) 2007-2008 Shawn Betts
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

;; Commentary:
;;
;; Generate the texinfo manual from docstrings in the source.
;;
;; Code:

(in-package #:stumpwm)

(require :sb-introspect)

(defvar *debug-doc* nil
  "When T, will print extra debugging information from the doc generator.")

(defun dprint (type sym)
  "Handy for figuring out which symbol is borking the documentation."
  (declare (ignorable type sym))
  (when *debug-doc*
    (format t "~&Doing ~a ~a..." type sym)))

(defmacro doc-fmt (stream new-name body-format &body args)
  "Fill in a texinfo template."
  `(format ,stream
           ,(concatenate 'string
                         "@" new-name " " body-format "~&@end " new-name "~%~%")
           ,@args))

(defmacro defdoc (name ((out-stream-var line-var name-var)
                        marker dprint-label)
                  &body body)
  "Define a document generating function."
  `(defun ,name (,out-stream-var ,line-var)
     (ppcre:register-groups-bind (,name-var)
         (,(format nil "~@{~A~}" "^" marker "\\W(.*)") ,line-var)
       (dprint ',dprint-label ,name-var)
       ,@body)))

(defdoc generate-function-doc ((s line name) "@@@" func)
  (let ((fn (if (find #\( name :test 'char=)
                ;; handle (setf <symbol>) functions
                (with-standard-io-syntax
                  (let ((*package* (find-package :stumpwm)))
                    (fdefinition (read-from-string name))))
                (symbol-function (find-symbol (string-upcase name) :stumpwm)))))
    (let ((*print-pretty* nil))
      (doc-fmt s "defun" "{~a} ~{~a~^ ~}~%~a" name
        (sb-introspect:function-lambda-list fn)
        (documentation fn 'function)))
    t))

(defdoc generate-macro-doc ((s line name) "%%%" macro)
  (let* ((symbol (find-symbol (string-upcase name) :stumpwm)))
    (let ((*print-pretty* nil))
      (doc-fmt s "defmac" "{~a} ~{~a~^ ~}~%~a"
        name
        (sb-introspect:function-lambda-list (macro-function symbol))
        (documentation symbol 'function)))
    t))

(defdoc generate-variable-doc ((s line name) "###" var)
  (let ((sym (find-symbol (string-upcase name) :stumpwm)))
    (doc-fmt s "defvar" "~a~%~a"
      name (documentation sym 'variable))
    t))

(defdoc generate-hook-doc ((s line name) "$$$" hook)
  (let ((sym (find-symbol (string-upcase name) :stumpwm)))
    (doc-fmt s "defvr" "{Hook} ~a~%~a"
      name (documentation sym 'variable))
    t))

(defdoc generate-command-doc ((s line name) "!!!" cmd)
  (if-let (symbol (find-symbol (string-upcase name) :stumpwm))
    (let ((cmd (symbol-function symbol))
          (*print-pretty* nil))
      (doc-fmt s "deffn" "{Command} ~a ~{~a~^ ~}~%~a"
        name
        (sb-introspect:function-lambda-list cmd)
        (documentation cmd 'function))
      t)
    (warn "Symbol ~A not found in package STUMPWM" name)))

(defun generate-manual (&key (in #p"stumpwm.texi.in") (out #p"stumpwm.texi"))
  "Generate the texinfo manual from the template texi.in file."
  (let ((*print-case* :downcase))
    (with-open-file (os out :direction :output :if-exists :supersede)
      (with-open-file (is in :direction :input)
        (loop for line = (read-line is nil is)
              until (eq line is) do
              (or (generate-function-doc os line)
                  (generate-macro-doc os line)
                  (generate-hook-doc os line)
                  (generate-variable-doc os line)
                  (generate-command-doc os line)
                  (write-line line os)))))))

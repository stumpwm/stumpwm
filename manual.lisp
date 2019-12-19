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

(require :sb-introspect) ; Function lambda lists.
;; FIXME: Why does SBCL not know how to REQUIRE sb-mop?
;; (require :sb-mop); Class slots lists.

(defvar *valid-doctypes* nil
  "All the line-types that can be generated for the StumpWM documentation.")

(defmacro doc-fmt (stream new-name body-format &body args)
  "Fill in a texinfo template."
  `(format ,stream
           ,(concatenate 'string
                         "@" new-name " " body-format "~&@end " new-name "~%~%")
           ,@args))

(defgeneric generate (specializer out-stream line)
  (:documentation "Generate a texi.in documentation line."))

(defmacro defdoc ((specializer
                   (out-stream-var line-var name-var symbol-var)
                   marker)
                  &body body)
  "Define a document generating method."
  (with-gensyms (gspecializer)
       `(progn
          (pushnew ,specializer *valid-doctypes*)
          (defmethod generate ((,gspecializer (eql ,specializer)) ,out-stream-var ,line-var)
            (ppcre:register-groups-bind (,name-var)
                (,(format nil "~@{~A~}" "^" marker "\\W(.*)") ,line-var)
              (let ((,symbol-var (find-symbol (string-upcase ,name-var) :stumpwm)))
                (format *debug-io* "~&Formatting manual for the ~a ~a...~&"
                        ,gspecializer ,name-var)
                ,@body
                t))))))

(defdoc (:function (s line name fn) "@@@")
  (let ((fn (if (find #\( name :test 'char=)
                ;; handle (setf <symbol>) functions
                (with-standard-io-syntax
                  (let ((*package* (find-package :stumpwm)))
                    (fdefinition (read-from-string name))))
                (symbol-function fn))))
    (let ((*print-pretty* nil))
      (doc-fmt s "defun" "{~a} ~{~a~^ ~}~%~a" name
        (sb-introspect:function-lambda-list fn)
        (documentation fn 'function)))))

(defdoc (:generic-function (s line name fn) "\\+\\+\\+")
  (let ((*print-pretty* nil))
    (doc-fmt s "deffn" "{Generic Function} ~a ~{~a~^ ~}~%~a"
      name
      (sb-introspect:function-lambda-list fn)
      (documentation fn 'function))))

(defdoc (:macro (s line name macro) "%%%")
  (let ((*print-pretty* nil))
    (doc-fmt s "defmac" "{~a} ~{~a~^ ~}~%~a"
      name
      (sb-introspect:function-lambda-list (macro-function macro))
      (documentation macro 'function))))

(defdoc (:variable (s line name var) "###")
  (doc-fmt s "defvar" "~a~%~a"
    name (documentation var 'variable)))

(defdoc (:hook (s line name hook) "\\$\\$\\$")
  (doc-fmt s "defvr" "{Hook} ~a~%~a"
    name (documentation hook 'variable)))

(defdoc (:command (s line name cmd) "!!!")
  (let ((cmd (symbol-function cmd))
        (*print-pretty* nil))
    (doc-fmt s "deffn" "{Command} ~a ~{~a~^ ~}~%~a"
      name
      (sb-introspect:function-lambda-list cmd)
      (documentation cmd 'function))))

(defdoc (:condition (s line name condition) "&&&")
  (doc-fmt s "deffn" "{Condition} ~a ~{~a~^ ~}~%~a"
    ;; There is no ANSI standard way to do get condition documentation, but
    ;; 'type works on SBCL.
    name
    (sb-mop:class-slots (class-of condition))
    (documentation condition'type)))

(defmethod generate ((specializer null) os line)
  (mapc (lambda (spec)
          (when (generate spec os line)
            (return-from generate)))
        *valid-doctypes*)
  (write-line line os))

(defun generate-manual (&key (in #p"stumpwm.texi.in") (out #p"stumpwm.texi"))
  "Generate the texinfo manual from the template texi.in file."
  (let ((*print-case* :downcase))
    (with-open-file (os out :direction :output :if-exists :supersede)
      (with-open-file (is in :direction :input)
        (loop for line = (read-line is nil is)
              until (eq line is) do (generate nil os line))))))

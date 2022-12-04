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

(defun format-lambda-list (texinfo-output list)
  "Print the lambda list LIST to the stream TEXINFO-OUTPUT.  The lambda list is
printed with only the argument names and types, followed by a list of default
arguments when applicable.  This function assumes that it is printing within the
context of a @defun, @deffn, @defmac, or similar.  It is also assumed that 80 is
the maximum line width."
  (let ((*print-pretty* nil)
        (format-string " ~A")
        (stream texinfo-output)
        optionals rest keys aux)
    (declare (ignorable optionals rest keys aux))
    (macrolet ((with-bind-check
                   ((var list &optional (argtypes '(&optional &rest &key &aux))
                         (dispatcher 'dispatch-format))
                    &body body)
                 (let ((l (gensym)))
                   `(let* ((,l ,list)
                           (,var (car ,l)))
                      (cond ((member ,var ',argtypes)
                             (,dispatcher ,var (cdr ,l)))
                            ((null ,var)
                             nil)
                            (t ,@body))))))
      (labels ((dispatch-format (type list)
                 (when type
                   (format stream format-string type)
                   (case type
                     ((&optional) (format-optional list))
                     ((&rest)     (format-rest list))
                     ((&key)      (format-key list))
                     ((&aux)      nil)
                     (otherwise   (format-unknown list)))))
               (format-key-optional (arg)
                 (destructuring-bind (name &optional default provided)
                     (if (atom arg) (list arg) arg)
                   (format stream format-string name)
                   (list name default provided)))
               (format-normal (list &optional in-list)
                 (with-bind-check (arg list)
                   (if (listp arg)
                       (progn
                         (format stream "(")
                         (format-normal arg t)
                         (format stream ")")
                         (format-normal (cdr list)))
                       (progn
                         (format stream
                                 (if in-list
                                     "~A"
                                     format-string)
                                 arg)
                         (format-normal (cdr list))))))
               (format-optional (list)
                 (with-bind-check (arg list '(&rest &key &aux))
                   (push (format-key-optional arg) optionals)
                   (format-optional (cdr list))))
               (format-rest (list)
                 (with-bind-check (arg list '(&key &aux))
                   (format stream format-string arg)
                   (format-rest (cdr list))))
               (format-key (list)
                 (with-bind-check (arg list '(&aux))
                   (push (format-key-optional arg) keys)
                   (format-key (cdr list))))
               (format-unknown (list)
                 (format stream format-string (car list))
                 (dispatch-format (cadr list) (cddr list))))
        (format-normal list)
        (let* ((opts-keys (append (reverse optionals) (reverse keys)))
               (len (length (string (caar (sort (copy-seq opts-keys)
                                                (lambda (a b)
                                                  (> (length (string (car a)))
                                                     (length (string (car b))))))))))
               (argstr (concatenate 'string
                                    "  ~A~"
                                    (format nil "~D" (+ 4 len))
                                    "T"))
               (valstr (if (> len 46)
                           "~%      ~S~%"
                           "~S~%")))
          (terpri stream)
          (when opts-keys
            (format stream "Default Values:~%@verbatim~%")
            (let ((*print-right-margin* (- 80 (+ 4 len))))
              (dolist (var opts-keys)
                (destructuring-bind (name default provided) var
                  (declare (ignore provided))
                  (format stream argstr name)
                  (let ((*print-pretty* t))
                    (format stream valstr default)))))
            (format stream "@end verbatim~%")))))))

(defun generate-function-doc (s line)
  (ppcre:register-groups-bind (name) ("^@@@ (.*)" line)
    (let ((fn-name (with-standard-io-syntax
                     (let ((*package* (find-package :stumpwm)))
                       (read-from-string name)))))
      (if (fboundp fn-name)
          (let ((fn (fdefinition fn-name))
                (*print-pretty* nil))
            (format s "@defun {~A} " name)
            (format-lambda-list s (sb-introspect:function-lambda-list fn))
            (format s "~A~&@end defun~%~%" (documentation fn 'function))
            t)
          (warn "Function ~A not found." fn-name)))))

(defun generate-macro-doc (s line)
  (ppcre:register-groups-bind (name) ("^%%% (.*)" line)
    (let* ((symbol (find-symbol (string-upcase name) :stumpwm))
           (*print-pretty* nil))
      (format s "@defmac {~A} " name)
      (format-lambda-list s (sb-introspect:function-lambda-list
                             (macro-function symbol)))
      (format s "~A~&@end defmac~%~%" (documentation symbol 'function))
      t)))

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
    (if-let (symbol (find-symbol (string-upcase name) :stumpwm))
      (let ((cmd (symbol-function symbol))
            (*print-pretty* nil))
        (format s "@deffn {Command} ~A " name)
        (format-lambda-list s (sb-introspect:function-lambda-list cmd))
        (format s "~A~&@end deffn~%~%" (documentation cmd 'function))
        t)
      (warn "Symbol ~A not found in package STUMPWM" name))))

(defun generate-class-doc (s line)
  (ppcre:register-groups-bind (name) ("^€€€ (.*)" line)
    (let ((sym (find-symbol (string-upcase name) :stumpwm)))
      (if sym
          (let ((class (find-class sym)))
            (if class
                (progn
                  (format s "@deftp {Class} ~A ~{~A~^ ~}~%~ADirect Superclasses: ~{~A~^, ~}@*~&Direct Subclasses: ~{~A~^, ~}@*~&Direct Slots: @*@ @ ~{~{~A~^@ -@ ~}~^@*@ @ ~}@*~&@end deftp~%~%"
                          sym
                          (mapcar #'sb-mop:slot-definition-name
                                  (sb-mop:class-direct-slots class))
                          (let ((doc (documentation class t)))
                            (if doc
                                (concatenate 'string doc "@*")
                                ""))
                          (mapcar #'sb-mop:class-name
                                  (sb-mop:class-direct-superclasses class))
                          (mapcar #'sb-mop:class-name
                                  (sb-mop:class-direct-subclasses class))
                          (mapcar (lambda (slot)
                                    (let ((name (sb-mop:slot-definition-name slot))
                                          (docs (documentation slot t)))
                                      (if docs
                                          (list name docs)
                                          (list name))))
                                  (sb-mop:class-direct-slots class)))
                  t)
                (warn "Symbol ~A does not denote a class" sym)))
          (warn "Symbol ~A not found in package STUMPWM" sym)))))

(defun generate-manual (&key (in #p"stumpwm.texi.in") (out #p"stumpwm.texi"))
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
                  (generate-class-doc os line)
                  (write-line line os)))))))

(defpackage #:stumpwm/manual
  (:use #:cl)
  (:export
   #:generate-manual))
(in-package #:stumpwm/manual)

(defvar *manual*)

(defvar *sigils* '(("@@@" . :function)
                   ("%%%" . :macro)
                   ("$$$" . :hook)
                   ("###" . :variable)
                   ("!!!" . :command)))

(defun parse-line (line)
  (when (> 4 (length line))
    (return-from parse-line (values)))
  (let ((sigil (subseq line 0 3))
        (name (subseq line 4)))
    (values (cdr (assoc sigil *sigils* :test #'string=))
            (with-standard-io-syntax
              (let ((*package* (find-package "STUMPWM")))
                (read-from-string name))))))

(defun document-function (name)
  (if (fboundp name)
      (let* ((fn (fdefinition name) )
             (lambda-list (sb-introspect:function-lambda-list fn))
             (docstring (documentation fn 'function)))
        (format *manual* "@defun {~A} ~{~A~^ ~}~%~A~&@end defun~%~%"
                name
                lambda-list
                docstring))
      (warn "No function found by the name ~A" name)))

(defun document-macro (name))

(defun document-variable (name)
  (format *manual* "@defvar ~A~%~A~&@end defvar~%~%"
          name (documentation sym 'variable)))

(defun document-hook (name)
  (format *manual* "@defvr {Hook} ~A~%~A~&@end defvr~%~%"
          name (documentation sym 'variable)))

(defun document-command (name)
  (format *manual* "@deffn {Command} ~A ~{~A~^ ~}~%~A~&@end deffn~%~%"
          name
          (sb-introspect:function-lambda-list cmd)
          (documentation cmd 'function)))

(defun generate-manual (input-file output-file)
  (with-open-file (*manual* output-file :direction :output :if-exists :supersede)
    (with-open-file (in input-file)
      (loop :for line := (read-line in nil)
            :while line
            :do (multiple-value-bind (kind name)
                    (parse-line line)
                  ;; And alternative would be to use kind as an eql specializer
                  ;; and leave it open-ended.
                  (case kind
                    (:function (document-function name))
                    (:macro (document-macro name))
                    (:variable (document-variable name))
                    (:hook (document-hook name))
                    (:command (document-command name))
                    (t (write-line line *manual*)))
                  )))))

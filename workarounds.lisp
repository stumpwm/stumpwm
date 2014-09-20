;;; workarounds for bugs in clx

(in-package :xlib)

;;; Both clisp and SBCL can't handle incompliant (and in clisp's case,
;;; even compliant) wm-class strings. See test-wm-class in test-wm.lisp.

#+sbcl
(defun get-wm-class (window)
  (declare (type window window))
  (declare (clx-values (or null name-string) (or null class-string)))
  (let ((value (get-property window :WM_CLASS :type :STRING :result-type '(vector card8))))
    (declare (type (or null (vector card8)) value))
    (when value
      ;; Buggy clients may not comply with the format, so deal with
      ;; the unexpected.
      (let* ((first-zero (position 0 (the (vector card8) value)))
             (second-zero (and first-zero
                               (position 0 (the (vector card8) value) :start (1+ first-zero))))
	     (name (subseq (the (vector card8) value) 0 first-zero))
	     (class (and first-zero
                         (subseq (the (vector card8) value) (1+ first-zero) second-zero))))
	(values (and (plusp (length name)) (map 'string #'card8->char name))
		(and (plusp (length class)) (map 'string #'card8->char class)))))))

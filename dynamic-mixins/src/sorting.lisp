(in-package :dynamic-mixins-swm)

(defvar *class-ordering-rules* nil
  "A plist of rules for how to order classes for mixing. Keys are the class
names. Rules have the following shape:

(:before ((string-1 . package-designator-1)
          (string-2 . package-designator-2)
          ...
          (string-n . package-designator-n))
 :after ((string-1 . package-designator-1)
         (string-2 . package-designator-2)
         ...
         (string-n . package-designator-n)))")

(defun set-rule (symbol before after)
  "Add or replace a class ordering rule for SYMBOL."
  (setf (getf *class-ordering-rules* symbol) (list :before before :after after)))

(defun symbol-ordering-rules (symbol)
  (getf *class-ordering-rules* symbol))

(defun symbol-ordering-rules-before-list (symbol &optional rules)
  (getf (or rules (symbol-ordering-rules symbol)) :before))

(defun symbol-ordering-rules-after-list (symbol &optional rules)
  (getf (or rules (symbol-ordering-rules symbol)) :after))

(defun symbol-spec-match (symbol spec)
  (let ((p (find-package (cdr spec))))
    (when p
      (eq (find-symbol (string (car spec)) p)
          symbol))))

(defun symbol-before-p (s1 s2)
  "Return truthy if S1 should be before S2."
  (or (find s2 (symbol-ordering-rules-before-list s1) :test #'symbol-spec-match)
      (find s1 (symbol-ordering-rules-after-list s2) :test #'symbol-spec-match)))

(defun symbol-after-p (s1 s2)
  "Return truthy if S1 should be after S2."
  (or (find s2 (symbol-ordering-rules-after-list s1) :test #'symbol-spec-match)
      (find s1 (symbol-ordering-rules-before-list s2) :test #'symbol-spec-match)))

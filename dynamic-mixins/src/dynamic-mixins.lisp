(in-package :dynamic-mixins-swm)

(defvar *dynamic-mix-classes* (make-hash-table :test 'equal))

(defclass mixin-class (standard-class)
  ((classes :initform nil :initarg :classes :accessor mixin-classes)))

(defmethod sb-mop:validate-superclass ((class mixin-class) (super standard-class))
  t)

(defmethod print-object ((o mixin-class) stream)
  (with-slots (classes) o
    (print-unreadable-object (o stream :identity t)
      (format stream "~S ~S"
              (or (class-name o) 'mixin-class)
              (mapcar #'class-name classes)))))

(defclass mixin-object () ())

(defstruct mix-list (list nil))

(defun %find-class (name-or-class)
  (etypecase name-or-class
    (symbol (find-class name-or-class))
    (class name-or-class)))

(defun %mix (object-or-class &rest class-list)
  "Create a MIX-LIST for MAKE-INSTANCE.  The first element may be an
instance; further elements must be class names or classes."
  (let ((class0 (typecase object-or-class
                  (symbol (list (find-class object-or-class)))
                  (mixin-object
                   (slot-value (class-of object-or-class) 'classes))
                  (t (list (class-of object-or-class))))))
    (make-mix-list
     :list (sort (remove-duplicates
                  (append (mapcar #'%find-class class-list)
                          class0))
                 'symbol-before-p
                 :key 'class-name))))

(defun mix (&rest classes)
  (make-mix-list :list (sort (remove-duplicates (mapcar #'%find-class classes))
                             'symbol-before-p
                             :key 'class-name)))

(defun set-superclasses (class list)
  (reinitialize-instance class :direct-superclasses list))

(defun define-mixin (mix-list)
  (let ((new-class (make-instance 'mixin-class
                     :classes (mix-list-list mix-list))))
    (handler-case
        (progn
          (set-superclasses new-class (list* (find-class 'mixin-object)
                                             (mix-list-list mix-list))))
      (error (e)
        (set-superclasses new-class nil)
        (error e)))
    (setf (gethash (mix-list-list mix-list) *dynamic-mix-classes*)
          new-class)))

(defun ensure-mixin (mix-list)
  (if (cdr (mix-list-list mix-list))
      (if-let ((class (gethash (mix-list-list mix-list)
                               *dynamic-mix-classes*)))
        class
        (define-mixin mix-list))
      (car (mix-list-list mix-list))))

(defun ensure-mix (object &rest classes)
  (let ((new-class (ensure-mixin (apply #'%mix object classes))))
    (change-class object new-class)))

(defun delete-from-mix (object &rest classes)
  (if (typep object 'mixin-object)
      (let* ((classes (mapcar #'%find-class classes))
             (old-classes (slot-value (class-of object) 'classes))
             (new-classes (remove-if (lambda (x) (member (%find-class x) classes))
                                     old-classes))
             (new-class (if (cdr new-classes)
                            (ensure-mixin (apply #'mix new-classes))
                            (car new-classes))))
        (change-class object new-class))
      object))

(defmethod make-instance ((items mix-list) &rest initargs &key &allow-other-keys)
  (apply #'make-instance (ensure-mixin items) initargs))



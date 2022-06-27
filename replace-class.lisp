(in-package :dynamic-mixins)

(defgeneric replace-class-in-mixin (object new-class old-class &rest initargs)
  (:method ((object standard-object) n o &rest rest)
    (declare (ignore o))
    (apply #'change-class object n rest)))

(defmethod replace-class-in-mixin ((object mixin-object)
                                   (new-class class)
                                   (old-class class)
                                   &rest rest)
  (apply #'replace-class-in-mixin 
         object (class-name new-class) (class-name old-class) rest))

(defmethod replace-class-in-mixin ((object mixin-object)
                                   (new-class class)
                                   (old-class symbol)
                                   &rest rest)
  (apply #'replace-class-in-mixin object (class-name new-class) old-class rest))

(defmethod replace-class-in-mixin ((object mixin-object)
                                   (new-class symbol)
                                   (old-class class)
                                   &rest rest)
  (apply #'replace-class-in-mixin object new-class (class-name  old-class) rest))

(defmethod replace-class-in-mixin ((object mixin-object)
                                   (new-class symbol)
                                   (old-class symbol)
                                   &rest initargs)
  (cond ((eql new-class old-class)
         object)
        (t
         ;; First we disable all non-compatible minor modes. 
         (loop for mode in (stumpwm::list-minor-modes object)
               unless (let* ((scope (stumpwm:minor-mode-scope mode))
                             (st (stumpwm::scope-type scope)))
                        (or (eql new-class st)
                            (stumpwm::superclassp new-class st)))
                 do (stumpwm::autodisable-minor-mode mode object))
         (if (typep object 'mixin-object)
             (flet ((mix-it (mix-list)
                      (apply #'change-class
                             object (ensure-mixin mix-list) initargs)
                      (stumpwm::sync-minor-modes object)
                      object))
               (let* ((tag nil)
                      (old-class-obj (find-class old-class))
                      (fn (lambda (e)
                            (when (or (eql e old-class) (eql e old-class-obj))
                              (setf tag t)
                              t)))
                      (mix-list
                        (make-mix-list
                         :list (remove-duplicates
                                (mapcar #'%find-class
                                        (subst-if new-class
                                                  fn
                                                  (mixin-classes
                                                   (class-of object))))))))
                 (if tag
                     (mix-it mix-list)
                     (restart-case
                         (error "~A is not an explicitly mixed class in ~A"
                                old-class object)
                       (continue ()
                         object)
                       (mix-in-new-class ()
                         (ensure-mix object new-class))))))
             (apply #'change-class object new-class initargs)))))

(defgeneric replace-class (object new-class &rest initargs))

(defmethod replace-class :around (object new &rest rest)
  (restart-case (progn
                  (call-next-method)
                  (unless (typep object new)
                    (error "Failed to change class ~A ~A" object new)))
    (force-change ()
      :report (lambda (s)
                (format s "Change class to ~A, removing all mixins" new))
      (apply #'change-class object new rest)))
  object)

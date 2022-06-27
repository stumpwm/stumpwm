(in-package :dynamic-mixins)

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

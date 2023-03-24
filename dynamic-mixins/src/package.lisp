(defpackage :dynamic-mixins-swm
  (:use #:cl #:alexandria)
  (:export #:mixin-class #:mixin-object #:mixin-classes
           #:ensure-mix #:delete-from-mix #:mix
           #:replace-class #:replace-class-in-mixin))

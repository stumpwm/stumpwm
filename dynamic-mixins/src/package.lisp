(defpackage :dynamic-mixins
  (:use #:cl #:alexandria)
  (:export #:mixin-class #:mixin-object
           #:ensure-mix #:delete-from-mix #:mix))

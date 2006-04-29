(defpackage :stumpwm
  (:use :cl)
  (:export #:stumpwm #:define-key #:undefine-key #:*root-map*
	   #:*top-map* #:set-fg-color #:set-bg-color #:set-border-color #:set-font
	   #:sync-keys #:*input-map* #:define-stumpwm-command #:kbd #:set-prefix-key))


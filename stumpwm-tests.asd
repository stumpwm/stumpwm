(defsystem "stumpwm-tests"
  :name "StumpWM tests"
  :serial t
  :depends-on ("stumpwm"
               "fiasco")
  :pathname "tests/"
  :components ((:file "package")
               (:file "kmap")
               (:file "pathnames")
               (:file "mode-line-formatters"))
  :perform (test-op (o c)
             (uiop/package:symbol-call "FIASCO" "RUN-TESTS" 'stumpwm-tests)))

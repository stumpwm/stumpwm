(defsystem "stumpwm-tests"
  :name "StumpWM tests"
  :serial t
  :depends-on ("stumpwm"
               "fiasco"
               "alexandria")
  :pathname "tests/"
  :components ((:file "package")
               (:file "kmap")
               (:file "pathnames")
               (:file "config-system"))
  :perform (test-op (o c)
             (uiop/package:symbol-call "FIASCO" "ALL-TESTS")))

(in-package #:stumpwm-tests)

(deftest test-bar ()
  (is (= 3 (count #\X (bar 60 5 #\X #\= ) :test #'char=)))
  (is (= 2 (count #\= (bar 60 5 #\X #\= ) :test #'char=))))

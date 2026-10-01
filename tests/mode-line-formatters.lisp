(in-package #:stumpwm-tests)

(deftest test-bar ()
  (is (= 3 (count #\X (bar 60 5 #\X #\= ) :test #'char=)))
  (is (= 2 (count #\= (bar 60 5 #\X #\= ) :test #'char=)))
  (is (string= "^[" (subseq (bar 60 5 #\X #\= ) 0 2)))
  (is (string= "^]" (subseq (bar 60 5 #\X #\= ) 12 14))))

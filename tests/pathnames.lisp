(in-package #:stumpwm-tests)

(fiasco:deftest test-directory-pathname-p ()
  (is (stumpwm::directory-pathname-p "/")))

(fiasco:deftest test-ensure-directory-pathname ()
  (is (equal (stumpwm::pathname-as-directory "/") #P"/"))
  (is (equal (stumpwm::pathname-as-directory "/test.lisp") #P"/test.lisp/")))

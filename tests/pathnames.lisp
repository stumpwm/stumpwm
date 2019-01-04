(in-package #:stumpwm-tests)

(deftest test-directory-pathname-p ()
  (is (stumpwm::directory-pathname-p "/")))

(deftest test-ensure-directory-pathname ()
  (is (equal (pathname-as-directory "/") #P"/"))
  (is (equal (pathname-as-directory "/test.lisp") #P"/test.lisp/")))

(in-package #:stumpwm-tests)

(defun expand-key-description (&rest desc)
  (let ((args (list (car desc) :keysym)))
    (dolist (mod (cdr desc))
      (push mod args)
      (push t args))
    (apply 'stumpwm::make-key (nreverse args))))

(defmacro expect-key (kbd &key to-be)
  `(is (equalp (stumpwm::parse-key ,kbd) (expand-key-description ,@to-be))))

(fiasco:deftest test-parse-key ()

  (expect-key "C-l" :to-be (108 :control))
  (expect-key "C-S-l" :to-be (108 :control :shift))
  (expect-key "C-s-l" :to-be (108 :control :super))
  (expect-key "C--" :to-be (45 :control))
  (expect-key "-" :to-be (45))

  (signals stumpwm::kbd-parse-error (stumpwm::parse-key "C-")))


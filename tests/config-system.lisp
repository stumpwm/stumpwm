(in-package #:config-system-tests)

(defmacro defconfig-test (name lambda-list before &body body)
  (multiple-value-bind (forms declarations docstring) (parse-body body :documentation t)
    `(let* ((config-system::*config-vars* (make-hash-table))
            (table config-system::*config-vars*))
       ,@before
       ,(if docstring
            `(fiasco:deftest ,name ,lambda-list
               ,docstring
               ,@declarations
               (let ((config-system::*config-vars* table))
                 ,@forms))
            `(fiasco:deftest ,name ,lambda-list
               ,@declarations
               (let ((config-system::*config-vars* table))
                 ,@forms))))))

(defvar *foo* "test variable")
(defvar *bar* "test variable")

(defconfig-test define-config-var-stores-info ()
  ((let ((default "1")
         (validator #'stringp))
     (define-config-var *foo* default validator "documentation")))
  (let ((info (gethash '*foo* config-system::*config-vars*))
        (default "1")
        (validator #'stringp))
    (is info)
    (is (config-info-default info) default)
    (is (config-info-name info) '*foo*)
    (is (config-info-doc info) "documentation")
    (is (config-info-validator info) validator)))

(defconfig-test define-config-parameter-stores-info ()
  ((let ((default "1")
         (validator #'stringp))
     (define-config-parameter *foo* default validator "documentation")))
  (let ((info (gethash '*foo* config-system::*config-vars*)))
    (is info)
    (is (config-info-default info) "1")
    (is (config-info-name info) '*foo*)
    (is (config-info-doc info) "documentation")
    (is (config-info-validator info) #'stringp)))

(defconfig-test get-configuration-info-finds-info ()
    ((let ((default "1")
           (validator #'stringp))
       (define-config-parameter *foo* default validator "documentation")))
  (let ((info (get-configuration-info '*foo*))
        (default "1")
        (validator #'stringp))
    (is info)
    (is (config-info-default info) default)
    (is (config-info-name info) '*foo*)
    (is (config-info-doc info) "documentation")
    (is (config-info-validator info) validator)))

(defconfig-test list-all-configurations-shows-all ()
    ((let ((validator #'stringp))
       (define-config-parameter *foo* "11" validator "documentation")
       (define-config-parameter *bar* "21" validator "documentation")))
  (let ((configs (list-all-configurations)))
    (is (length configs) 2)
    (is (member (get-configuration-info '*foo*) configs :test #'eql))
    (is (member (get-configuration-info '*bar*) configs :test #'eql))))

(defconfig-test list-all-configurations-gets-correct-values ()
    ((let ((validator #'stringp))
       (define-config-parameter *foo* "11" validator "documentation")
       (define-config-parameter *bar* "21" validator "documentation")))
  (let ((configs (list-all-configurations)))
    (is (length configs) 2)
    (let ((foo (find (get-configuration-info '*foo*) configs :test #'eql))
          (bar (find (get-configuration-info '*bar*) configs :test #'eql)))
      (is foo)
      (is bar)
      (is (config-info-value foo) "11")
      (is (config-info-value bar) "21"))))

(defconfig-test set-configuration-throws-on-not-found ()
    ((let ((validator #'stringp))
       (define-config-parameter *foo* "11" validator "documentation")))
  (signals config-not-found-error (set-configuration *bar* "asdf")))

(defconfig-test set-configuration-throws-on-invalid ()
    ((let ((validator #'stringp))
       (define-config-parameter *foo* "11" validator "documentation")))
  (signals invalid-datum-error (set-configuration *foo* 111)))

(defconfig-test set-configuration-checks-arg-length ()
    ()
  (signals error (macroexpand '(set-configuration *foo* 111 *bar*))))

(defconfig-test set-configuration-sets-valid ()
    ((let ((validator #'stringp))
       (define-config-parameter *foo* "11" validator "documentation")))
  (set-configuration *foo* "12")
  (is *foo* "12"))

(defconfig-test reset-configuration-works ()
    ((define-config-parameter *foo* "11" #'stringp "documentation")
     (define-config-parameter *bar* "22" #'stringp "documentation"))
  (set-configuration *foo* "asdf"
                     *bar* "qwerty")
  (reset-configuration *foo* *bar*)
  (is *foo* "11")
  (is *bar* "22"))

(defconfig-test with-atomic-update-restores-values ()
    ((let ((validator #'stringp))
       (define-config-parameter *foo* "qwerty" validator "documentation")
       (define-config-parameter *bar* "asdf" validator "documentation")))
  (ignore-errors
    (with-atomic-update (*foo* *bar*)
      (set-configuration *foo* "set")
      (set-configuration *bar* 11)))
  (is *foo* "qwerty")
  (is *bar* "asdf"))

(defconfig-test set-configuration-atomic-restores-values ()
    ((let ((validator #'stringp))
       (define-config-parameter *foo* "qwerty" validator "documentation")
       (define-config-parameter *bar* "asdf" validator "documentation")))
  (ignore-errors
    (set-configuration-atomic *foo* "set"
                              *bar* 11))
  (is *foo* "qwerty")
  (is *bar* "asdf"))

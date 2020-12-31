(in-package #:config-system)

(defparameter *config-vars* (make-hash-table)
  "Map that holds all of the configuration variables for the system")

(defstruct config-info
  (name t :type symbol :read-only t)
  (validator #'identity :type (function (*) boolean))
  (default nil :read-only t)
  (value nil)
  (doc "" :type string :read-only t))

(defun %full-symbol-string (symb)
  "Get the full name of the symbol complete with the package name"
  (declare (type symbol symb))
  (let* ((pkg (symbol-package symb))
         (pkg-name (package-name pkg))
         (symb-name (symbol-name symb)))
    (concatenate 'string pkg-name
                 (multiple-value-bind (symb status) (find-symbol symb-name pkg-name)
                   (declare (ignore symb))
                   (if (eql status :internal)
                       "::"
                       ":"))
                 symb-name)))

(defun describe-config-info (info &optional (stream *standard-output*))
  (declare (type config-info info))
  (format stream "Setting Name: ~A~%  Documentation:~%    ~A~%  Default value: ~S~%Current value: ~S~%  Validator function: ~A~%"
          (%full-symbol-string (config-info-name info))
          (config-info-doc info)
          (config-info-default info)
          (config-info-value info)
          (config-info-validator info)))

(defun %make-match-readtable (str)
  "Try to make the given string have the correct case for a symbol"
  (declare (type string str))
  (ccase (readtable-case *readtable*)
    ;; we are missing :invert, but I don't feel like implementing that.
    (:upcase (string-upcase str))
    (:downcase (string-downcase str))
    (:preserve string)))

(defun %map-config-matching (name-matches package-matches function)
  (let ((name-scanner (ppcre:create-scanner (%make-match-readtable name-matches)))
        (pkg-scanner (ppcre:create-scanner (%make-match-readtable package-matches))))
    (maphash (lambda (key value)
               (declare (ignorable key))
               (let ((pkg-name (package-name (symbol-package (config-info-name value))))
                     (symb-name (symbol-name (config-info-name value))))
                 (when (and (funcall name-scanner symb-name 0 (length symb-name))
                            (funcall pkg-scanner pkg-name 0 (length pkg-name)))
                   (funcall function value))))
             *config-vars*)))

(defun describe-all-config-info (&key (stream *standard-output*) (name-matches ".*") (package-matches ".*"))
  (%map-config-matching name-matches package-matches
                        (lambda (info)
                          (describe-config-info info stream)
                          (format stream "~%"))))

(define-condition config-error (error) ())

(define-condition invalid-datum-error (config-error)
  ((place-symbol :initarg :place :initform nil
		 :accessor invalid-datum-error-place
                 :type symbol)
   (value :initarg :value :initform nil
	  :accessor invalid-datum-error-value))
  (:report
   (lambda (c s)
     (with-slots (place-symbol value) c
       (format s "The value ~S is invalid for variable ~S." value place-symbol)))))

(define-condition config-not-found-error (config-error)
  ((place-symbol :initarg :place :initform nil
                 :accessor config-not-found-error-place
                 :type symbol)
   (alternatives :initarg :alternatives :initform nil
                 :accessor config-not-found-alternatives
                 :type list))
  (:report
   (lambda (c s)
     (with-slots (place-symbol alternatives) c
       (if alternatives
        (format s  "Setting ~A not found. Did you mean one of these? ~A"
                (%full-symbol-string place-symbol) alternatives)
        (format s  "Setting ~A not found." (%full-symbol-string place-symbol)))))))

(defun %generate-config-var-code (declare-type name default validator documentation)
  ;; if we wanted to get fancy, we could define a special type for this enumeration at this point too
  (check-type documentation string)
  (check-type name symbol)
  (with-gensyms (default-value is-valid func)
    `(progn
       (,declare-type ,name ,default ,@(when documentation
                                         (list documentation)))
       (let* ((,default-value ,name)
              (,func ,validator)
              (,is-valid (funcall ,func ,default-value)))
         (if ,is-valid
             (setf (gethash (quote ,name) *config-vars*)
                   (make-config-info :name (quote ,name) :default ,default-value
                                     :validator ,func :doc ,documentation))
             (error 'invalid-datum-error :place (quote ,name) :value ,default-value))))))

(defmacro defconfig (name default validator &key documentation reinitialize)
  "Create and register a configurable variable with the given default value,
validator function, and documentation"
  (%generate-config-var-code (if reinitialize `defparameter 'defvar)
                             name default validator documentation))

(defmacro define-config-enum (name default values &key (test-fn #'equal) documentation reinitialize)
  "Same as DEFCONFIG, except the validation function
is built by creating a function that checks if the set value is in the VALUES list."
  `(defconfig ,name ,default (lambda (x) (member x ,values :test ,test-fn))
              :documentation ,documentation :reinitialize reinitialize))

(defun all-config-info (&key (name-matches ".*") (package-matches ".*"))
  "List all of the available customizable settings matching the given criteria."
  (let ((accumulate (list)))
    (%map-config-matching name-matches package-matches
                          (lambda (info)
                            (push info accumulate)))
    accumulate))

(defun %find-possible-settings (setting-name table)
  "Find the settings that have the same symbol name of SETTING-NAME but are in a different package"
  (declare (type hash-table table)
           (type symbol setting-name))
  (let ((name (symbol-name setting-name)))
    ;; for some reason, the package isn't always included with the symbol name even
    ;; if the top level isn't in the symbol's package, so we need to manually get the symbol name.
    (mapcar #'%full-symbol-string
            (remove-if-not (lambda (x) (string-equal (symbol-name x) name))
                           (mapcar #'config-info-name (hash-table-values table))))))

(defun get-config-info (setting-name)
  "Find the info for the config variable stored in the symbol SETTING-NAME."
  (declare (type symbol setting-name))
  (if-let ((info (gethash setting-name *config-vars*)))
    (progn
      ;; Since the setting values can be changed outside of the API,
      ;; we need to update the value of the varible here.
      (setf (config-info-value info) (symbol-value setting-name))
      info)))

(defmacro %set-config (setting-name value)
  (with-gensyms (actual-value info)
    `(let ((,actual-value ,value))
       (if-let ((,info (get-configuration-info (quote ,setting-name))))
         (if (funcall (config-info-validator ,info) ,actual-value)
             (setf ,setting-name ,actual-value)
             (error 'invalid-datum-error :place (quote ,setting-name) :value ,actual-value))
         (error 'config-not-found-error
           :place (quote ,setting-name)
           :alternatives (%find-possible-settings (quote ,setting-name) *config-vars*))))))

(defmacro set-config (&rest settings)
  "Set the given configuration variables to the given values. Used like setf"
  (assert (= (mod (length settings) 2) 0))
  (let ((accumulate (list 'progn)))
    (do ((cur settings (cddr cur)))
        ((not cur))
      (push (list '%set-configuration (first cur) (second cur)) accumulate))
    (nreverse accumulate)))

(defmacro reset-config (&rest settings)
  "Reset the list of settings to their default values"
  (let ((accumulate (list)))
    ;; add everything backwards, as the list is built in reverse
    (dolist (setting-name settings)
      (check-type setting-name symbol)
      (push `(config-info-default (get-configuration-info (quote ,setting-name))) accumulate)
      (push setting-name accumulate))
    (push 'setf accumulate)
    accumulate))

(defun %make-storage-pair (symbol)
  (declare (type symbol symbol))
  (cons (gensym (symbol-name symbol)) symbol))

(defmacro with-atomic-update (settings &body body)
  "If an error occurs during the execution of BODY, reset the provided variables to their original value"
  (let ((setting-pairs (mapcar #'%make-storage-pair settings)))
    `(let ,(let ((settings-list (list)))
             (dolist (item setting-pairs (nreverse settings-list))
               (push (list (car item) (cdr item)) settings-list)))
       (handler-case
           (progn
             ,@body)
         (warning (w)
           (warn w))
         (error (condition)
           (setf ,@(loop for pair in setting-pairs
                         append (list (cdr pair) (car pair))))
           (error condition))
         (t (c)
           (signal c))))))

(defmacro set-config-atomic (&rest settings)
  "Set the listed settings to the provided values. If an error signal is raised during execution,
all of the settings are set back to their original value"
  (assert (= (mod (length settings) 2) 0))
  (let ((setting-vars (do ((cur settings (cddr cur))
                           (vars (list)))
                          ((not cur) (nreverse vars))
                        (push (first cur) vars))))
    `(with-atomic-update ,setting-vars
       (set-configuration ,@settings))))

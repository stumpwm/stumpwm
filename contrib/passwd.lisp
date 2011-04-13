(in-package :stumpwm-user)

(export '(*hmac-static-seed*
          *passphrase-remember-timeout*))

(require 'ironclad)

(defvar *hmac-static-seed* nil
  "Static seed appended to the passphrase to add even more entropy")
(defvar *passphrase-remember-timeout* 5
  "How long will the passphrase be remembered (in minutes)")
(defvar *clipboard-clear-timeout* 10
  "How long will the passphrase be remembered (in seconds)")
(defvar *hmac-algo* :sha1
  "Hashing algorithm used by ironclad to compute the HMAC")

(defvar *passphrase* nil)

(defvar *passphrase-timer*
  #+sbcl (sb-ext:make-timer (lambda ()
                              (setf *passphrase* nil)))
  #-sbcl (error 'not-implemented))

(defvar *old-clipboard* nil)

(defvar *clipboard-timer*
  #+sbcl (sb-ext:make-timer (lambda ()
                              (set-x-selection *old-clipboard*)
                              (setf *old-clipboard* nil)))
  #-sbcl (error 'not-implemented))

(define-stumpwm-type :hmac-passphrase (input prompt)
  (or *passphrase*
      (setf *passphrase*
            (or (argument-pop-rest input)
                (read-one-line (current-screen) prompt :password t)))))

(defun reset-timer (timer timeout)
  #+sbcl (progn
           (when (sb-ext:timer-scheduled-p timer)
             (sb-ext:unschedule-timer timer))
           (sb-ext:schedule-timer timer timeout))
  #-sbcl (error 'not-implemented))

(defcommand get-password (pass key) ((:hmac-passphrase "Key: ")
                                     (:rest "Password for: "))
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array
                                   (concatenate 'string pass *hmac-static-seed*))
                                  *hmac-algo*)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array key))
    (unless *old-clipboard*
      (setf *old-clipboard* (get-x-selection)))
    (when (and *clipboard-clear-timeout*
               (> *clipboard-clear-timeout* 0))
      (reset-timer *clipboard-timer* *clipboard-clear-timeout*))
    (if (and *passphrase-remember-timeout*
             (> *passphrase-remember-timeout* 0))
      (reset-timer *passphrase-timer* (* *passphrase-remember-timeout* 60))
      (setf *passphrase* nil))
    (set-x-selection (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac)))
    (message "Copied to clipboard")))

(setf *passphrase* nil)

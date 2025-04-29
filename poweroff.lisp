(in-package :stumpwm)

(export '(shutdown cancel-shutdown reboot halt suspend hibernate
          *alternative-suspend* *alternative-hibernate*))

(defun path-directories (&optional (name "PATH"))
  (let ((path-dirs (uiop:split-string (uiop:getenv name) :separator '(#\:))))
    (mapcar (lambda (x) (uiop:ensure-pathname x :ensure-directory t)) path-dirs)))

(defun file-in-path-p (name)
  (dolist (dir (path-directories))
    (when (probe-file (merge-pathnames dir name))
      (return dir))))

(defcommand shutdown (&optional (now nil) (time 30)) ((:y-or-n "Now? "))
  "Poweroff the system. If NOW is not NIL poweroff immediately, otherwise asks for a date."
  #+(or bsd linux)
  (let* ((time (if (and %interactivep% (not now))
                   (read-one-line (current-screen) "When? ")
                   (if (and %interactivep% now) "" time)))
         (cmd (format nil "shutdown ~a ~a" time (if now "now" ""))))
    (run-shell-command cmd)))

(defcommand cancel-shutdown () ()
  "Cancels any shutdown process."
  #+(or bsd linux) (run-shell-command (format nil "shutdown -c")))

(defcommand reboot (&optional (now nil) (time 30)) ((:y-or-n "Now? "))
  "Reboot the system. If NOW is not NIL reboot immediately, otherwise asks for a date."
  #+(or bsd linux)
  (let* ((time (if (and %interactivep% (not now))
                   (read-one-line (current-screen) "When? ")
                   (if (and %interactivep% now) "" time)))
         (cmd (format nil "shutdown -r ~a ~a" time (if now "now" ""))))
    (run-shell-command cmd)))

(defcommand halt (&optional (now nil) (time 30)) ((:y-or-n "Now? "))
  "Halt the system. If NOW is not NIL halt immediately, otherwise asks for a date."
  #+(or bsd linux)
  (let* ((time (if (and %interactivep% (not now))
                   (read-one-line (current-screen) "When? ")
                   (if (and %interactivep% now) "" time)))
         (cmd (format nil "shutdown -H ~a ~a" time (if now "now" ""))))
    (run-shell-command cmd)))

(declaim (type (or function string null) *alternative-suspend*))
(defvar *alternative-suspend* nil
  "Alternative shell command or function for the suspend function.")

(declaim (type (or function string null) *alternative-hibernate*))
(defvar *alternative-hibernate* nil
  "Alternative shell command or function used for the hibernate function.")

(flet ((call-alternative (alt)
         (cond
           ((stringp alt) (run-shell-command alt))
           ((functionp alt) (funcall alt)))))

  (defcommand suspend () ()
    "Suspends the system. For unsupported systems set *ALTERNATIVE-SUSPEND* to a shell command or function of your choice."
    #+linux (cond
              (*alternative-suspend* (call-alternative *alternative-suspend*))
              ((file-in-path-p "systemctl")
               (run-shell-command "systemctl suspend"))
              (t (error "Suspend is not implemented for this system.")))
    #+bsd (if *alternative-suspend*
              (call-alternative *alternative-suspend*)
              (error "Suspend is not implemented for this system.")))

  (defcommand hibernate () ()
    "Hibernates the system. For unsupported systems set *ALTERNATIVE-HIBERNATE* to a shell command or function of your choice."
    #+linux (cond
              (*alternative-hibernate* (call-alternative *alternative-hibernate*))
              ((file-in-path-p "systemctl")
               (run-shell-command "systemctl hibernate"))
              (t (error "Hibernate is not implemented for this system.")))
    #+bsd (if *alternative-hibernate*
              (call-alternative *alternative-hibernate*)
              (error "Hibernate is not implemented for this system."))))


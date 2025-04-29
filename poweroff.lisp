(in-package :stumpwm)

(export '(shutdown cancel-shutdown reboot halt suspend hibernate))

(defcommand shutdown (&optional (now nil) (time 30)) ((:y-or-n "Now? "))
  "Poweroff the system. If NOW is not NIL poweroff immediately, otherwise asks for a date."
  #+(or bsd linux)
  (let* ((time (if (and *interactivep* (not now))
                   (read-one-line (current-screen) "When?")
                   (if (and *interactivep* now) "" time)))
         (cmd (format nil "shutdown ~a ~a" time (if now "now" ""))))
    (run-shell-command cmd)))

(defcommand cancel-shutdown () ()
  "Cancels any shutdown process."
  #+(or bsd linux) (run-shell-command (format nil "shutdown -c")))

(defcommand reboot (&optional (now nil) (time 30)) ((:y-or-n "Now? "))
  "Reboot the system. If NOW is not NIL reboot immediately, otherwise asks for a date."
  #+(or bsd linux)
  (let* ((time (if (and *interactivep* (not now))
                   (read-one-line (current-screen) "When?")
                   (if (and *interactivep* now) "" time)))
         (cmd (format nil "shutdown -r ~a ~a" time (if now "now" ""))))
    (run-shell-command cmd)))

(defcommand halt (&optional (now nil) (time 30)) ((:y-or-n "Now? "))
  "Halt the system. If NOW is not NIL halt immediately, otherwise asks for a date."
  #+(or bsd linux)
  (let* ((time (if (and *interactivep* (not now))
                   (read-one-line (current-screen) "When?")
                   (if (and *interactivep* now) "" time)))
         (cmd (format nil "shutdown -H ~a ~a" time (if now "now" ""))))
    (run-shell-command cmd)))

(defcommand suspend () ()
  "Suspends the system. Available only in linux with systemd."
  #+linux (if (or (probe-file #P"/bin/systemctl") (probe-file #P"/usr/bin/systemctl"))
              (run-shell-command "systemctl suspend")
              (error "Suspend is not implemented for this system."))
  #+bsd (error "Suspend is not implemented for this system."))

(defcommand hibernate () ()
  "Hibernates the system. Available only in linux with systemd."
  #+linux (if (or (probe-file #P"/bin/systemctl") (probe-file #P"/usr/bin/systemctl"))
              (run-shell-command "systemctl hibernate")
              (error "Hibernate is not implemented for this system."))
  #+bsd (error "Hibernate is not implemented for this system."))

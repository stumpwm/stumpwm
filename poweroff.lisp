(in-package :stumpwm)

(export '(shutdown cancel-shutdown reboot halt suspend hibernate))

(import '(dbus:list-names dbus:with-introspected-object dbus:with-open-bus dbus:system-server-addresses))

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
  #+(or bsd linux)
  (run-shell-command (format nil "shutdown -c")))

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

(defcommand suspend () ()
  "Suspends the system."
  #+linux
  (with-open-bus (bus (system-server-addresses))
    (cond
      ((find "org.freedesktop.login1" (list-names bus) :test #'string=)
       (with-introspected-object (login bus "/org/freedesktop/login1" "org.freedesktop.login1")
         (if (string= "yes" (login "org.freedesktop.login1.Manager" "CanSuspend"))
             (login "org.freedesktop.login1.Manager" "Suspend" nil)
             (message "^1ERROR:~%Suspend is not available on your system.^*"))))
      ((= 0 (sb-posix:geteuid))
       (with-open-file (state #P"/sys/power/state" :direction :io :if-exists :supersede)
         (if (search "mem" (read-line state) :test #'string=)
             (write-string "mem" state)
             (message "^1ERROR:~%Suspend is not available on your system.^*"))))
      (t
       (message "^1ERROR:~%Suspend is not available on your system.^*"))))
  #+bsd
  (with-open-bus (bus (system-server-addresses))
    (if (find "org.freedesktop.ConsoleKit" (list-names bus) :test #'string=)
        (with-introspected-object (login bus "/org/freedesktop/ConsoleKit" "org.freedesktop.ConsoleKit")
          (if (string= "yes" (login "org.freedesktop.login1.Manager" "CanSuspend"))
              (login "org.freedesktop.ConsoleKit.Manager" "Suspend" nil)
              (message "^1ERROR:~%Suspend is not available on your system.^*")))
        (message "^1ERROR:~%Suspend is not available on your system.^*"))))

(defcommand hibernate () ()
  "Hibernates the system."
  #+linux
  (with-open-bus (bus (system-server-addresses))
    (cond
      ((find "org.freedesktop.login1" (list-names bus) :test #'string=)
       (with-introspected-object (login bus "/org/freedesktop/login1" "org.freedesktop.login1")
         (if (string= "yes" (login "org.freedesktop.login1.Manager" "CanHibernate"))
             (login "org.freedesktop.login1.Manager" "Hibernate" nil)
             (message "^1ERROR:~%Hibernate is not available on your system.^*"))))
      ((= 0 (sb-posix:geteuid))
       (with-open-file (state #P"/sys/power/state" :direction :io :if-exists :supersede)
         (if (search "disk" (read-line state) :test #'string=)
             (with-open-file (disk #P"/sys/power/disk" :direction :output :if-exists :supersede)
               (write-string "platform" disk)
               (write-string "disk" state))
             (message "^1ERROR:~%Hibernate is not available on your system.^*"))))
      (t
       (message "^1ERROR:~%Hibernate is not available on your system.^*"))))
  #+bsd
  (with-open-bus (bus (system-server-addresses))
    (if (find "org.freedesktop.ConsoleKit" (list-names bus) :test #'string=)
        (with-introspected-object (login bus "/org/freedesktop/ConsoleKit" "org.freedesktop.ConsoleKit")
          (if (string= "yes" (login "org.freedesktop.login1.Manager" "CanHibernate"))
              (login "org.freedesktop.ConsoleKit.Manager" "Hibernate" nil)
              (message "^1ERROR:~%Hibernate is not available on your system.^*")))
        (message "^1ERROR:~%Hibernate is not available on your system.^*"))))


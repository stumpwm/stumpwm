;; Copyright (C) 2003-2008 Shawn Betts
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; Window Manager commands that users can use to manipulate stumpwm
;; and write custos.
;;
;; Code:

(in-package :stumpwm)

(export '(defprogram-shortcut
          pathname-is-executable-p
	  programs-in-path
	  restarts-menu
	  run-or-raise
	  run-shell-command
          window-send-string))

(defun restarts-menu (err)
  "Display a menu with the active restarts and let the user pick
one. Error is the error being recovered from. If the user aborts the
menu, the error is re-signalled."
  (let ((restart (select-from-menu (current-screen)
                                   (mapcar (lambda (r)
                                             (list (format nil "[~a] ~a"
                                                           (restart-name r)
                                                           (substitute #\Space
                                                                       #\Newline
                                                                       (write-to-string r :escape nil)))
                                                   r))
                                           ;; a crusty way to get only
                                           ;; the restarts from
                                           ;; stumpwm's top-level
                                           ;; restart inward.
                                           (reverse (member 'top-level
                                                            (reverse (compute-restarts))
                                                            :key 'restart-name)))
                                   (format nil "Error: ~a"
                                           (substitute #\Space
                                                       #\Newline
                                                       (write-to-string err :escape nil))))))
    (when restart
      (invoke-restart (second restart)))))

(defun banish-pointer (&optional (where *banish-pointer-to*))
  "Move the pointer to the lower right corner of the head, or
 WHEREever (one of :screen :head :frame or :window)"
  (let* ((screen (current-screen))
         (group (current-group))
         (head (current-head))
         (frame (tile-group-current-frame group))
         (window (frame-window frame))
         (x (1- (+ (frame-x frame) (frame-width frame))))
         (y (1- (+ (frame-display-y group frame) (frame-display-height group frame)))))
    (ecase where
      (:screen
       (setf x (1- (+ (screen-x screen) (screen-width screen)))
             y (1- (+ (screen-y screen) (screen-height screen)))))
      (:head
       (setf x (1- (+ (head-x head) (head-width head)))
             y (1- (+ (head-y head) (head-height head)))))
      (:frame)
      (:window
       (when window
         (let ((win (window-parent window)))
           (setf x (1- (+ (xlib:drawable-x win) (xlib:drawable-width win)))
                 y (1- (+ (xlib:drawable-y win) (xlib:drawable-height win))))))))
    (warp-pointer (group-screen group) x y)))

(defcommand banish (&optional where) (:rest)
  "Warp the mouse the lower right corner of the current head."
  (if where
      (banish-pointer (intern (string-upcase where) :keyword))
      (banish-pointer)))

(defcommand ratwarp (x y) ((:number "X: ") (:number "Y: "))
  "Warp the mouse to the specified location."
  (warp-pointer (current-screen) x y))

(defcommand ratrelwarp (dx dy) ((:number "Delta X: ") (:number "Delta Y: "))
  "Warp the mouse by the specified amount from its current position."
  (warp-pointer-relative dx dy))

;; FIXME: This function doesn't work.
(defcommand ratclick (&optional (button 1)) (:number)
  (when (current-window)
    (send-fake-click (current-window) button)))

;;; (format-time-stringc ...) section
(defmacro time-lambda (used-var &body body)
  `(lambda (sec min hour dom mon year dow dstp tz)
    (declare (ignore ,@(set-difference '(sec min hour dom mon year dow dstp tz) used-var)))
    ,@body))

(defvar *month-names*
  #("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))

(defvar *day-names*
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

;; `date --help` with date_5.97
;; `date --help` with date_5.97
(defvar *format-time-string-alist*
  `((#\% . ,(time-lambda () "%"))
    (#\a . ,(time-lambda (dow) (subseq (aref *day-names* dow) 0 3)))
    (#\A . ,(time-lambda (dow) (aref *day-names* dow)))
    (#\b . ,(time-lambda (mon) (subseq (aref *month-names* (- mon 1)) 0 3)))
    (#\B . ,(time-lambda (mon) (aref *month-names* (- mon 1))))
    (#\c . ,(time-lambda (dow mon dom hour min sec year)
                         (format nil "~A ~A ~2,'0D ~2,'0D:~2,'0D:~2,'0D ~D"
                                 (subseq (aref *day-names* dow) 0 3)
                                 (subseq (aref *month-names* (- mon 1)) 0 3)
                                 dom hour min sec year)))
    (#\C . ,(time-lambda (year) (subseq (format nil "~D" year) 0 2)))
    (#\d . ,(time-lambda (dom) (format nil "~2,'0D" dom)))
    (#\D . ,(time-lambda (mon dom year)
                         (format nil "~2,'0D/~2,'0D/~A"
                                 mon dom (subseq (format nil "~D" year) 2 4))))
    (#\e . ,(time-lambda (dom) (format nil "~2,' D" dom)))
    (#\F . ,(time-lambda (year mon dom) (format nil "~D-~2,'0D-~2,'0D" year mon dom)))
    ;; %g   last two digits of year of ISO week number (see %G)
    ;; %G   year of ISO week  number (see %V); normally useful only with %V
    (#\h . ,(time-lambda (mon) (subseq (aref *month-names* (- mon 1)) 0 3)))
    (#\H . ,(time-lambda (hour) (format nil "~2,'0D" hour)))
    (#\I . ,(time-lambda (hour)
                         (format nil "~2,'0D" (if (> hour 12) (- hour 12) (if (zerop hour) 12 hour)))))
    ;; %j   day of year (001..366)
    (#\k . ,(time-lambda (hour) (format nil "~2,D" hour)))
    (#\l . ,(time-lambda (hour)
                         (format nil "~2,D" (if (> hour 12) (- hour 12) (if (zerop hour) 12 hour)))))
    (#\m . ,(time-lambda (mon) (format nil "~2,'0D" mon)))
    (#\M . ,(time-lambda (min) (format nil "~2,'0D" min)))
    (#\n . ,(time-lambda () "~%%")) ;; two % to avoid parsing errors
    ;; %N   nanoseconds (000000000..999999999)
    (#\p . ,(time-lambda (hour) (if (>= hour 12) "PM" "AM")))
    (#\P . ,(time-lambda (hour) (if (>= hour 12) "pm" "am")))
    (#\r . ,(time-lambda (hour min sec)
                         (let (hour-local am-pm)
                           (cond
                             ((> hour 12)
                              (setf hour-local (- hour 12) am-pm "PM"))
                             ((= hour 12)
                              (setf hour-local hour am-pm "PM"))
                             (t
                              (setf hour-local (if (zerop hour) 12 hour) am-pm "AM")))
                           (format nil "~2,'0D:~2,'0D:~2,'0D ~A"
                                   hour-local min sec am-pm))))
    (#\R . ,(time-lambda (hour min) (format nil "~2,'0D:~2,'0D" hour min)))
    (#\s . ,(time-lambda ( sec min hour dom mon year)
                         (format nil "~D"
                                 (- (encode-universal-time
                                     sec min hour dom mon year)
                                    (encode-universal-time 0 0 0 1 1 1970 0)))))
    (#\S . ,(time-lambda (sec) (format nil "~2,'0D" sec)))
    (#\t . ,(time-lambda () "~T"))
    (#\T . ,(time-lambda (hour min sec)
                         (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))
    (#\u . ,(time-lambda (dow) (format nil "~D" (+ dow 1))))
    ;; %U   week number of  year, with Sunday as first  day of week (00..53)
    ;; %V   ISO  week number,  with  Monday as  first  day of  week (01..53)
    (#\w . ,(time-lambda (dow) (format nil "~D" (- dow 1))))
    ;; %W   week number of  year, with Monday as first  day of week (00..53)
    ;; %x   locale's date representation (e.g., 12/31/99)
    ;; %X   locale's time representation (e.g., 23:13:48)
    (#\y . ,(time-lambda (year) (subseq (format nil "~D" year) 2 4)))
    (#\Y . ,(time-lambda (year) (format nil "~D" year)))
    (#\z . ,(time-lambda (tz dstp)
                         (multiple-value-bind (hour-local decimal-local)
                             (truncate (+ (* (float tz) -1) (if dstp 1 0)))
                           (format nil "~A~2,'0D~2,'0D"
                                   (if (> hour-local 0) '+ '-) (abs hour-local)
                                   (truncate (if (/= decimal-local 0)
                                                 (* 60 decimal-local) 0))))))
    ;; %:z  +hh:mm numeric timezone (e.g., -04:00)
    ;; %::z +hh:mm:ss numeric time zone (e.g., -04:00:00)
    ;; %:::z numeric time zone with  : to necessary precision (e.g., -04, +05:30)
    ;; %Z   alphabetic time zone abbreviation (e.g., EDT)
    )
  "An alist for the substitution in `format-time-string'.")

(defvar *format-time-string-default* "%a %b %e %k:%M:%S %Y"
  "The default value for `format-time-string', (e.g, Thu Mar  3 23:05:25 2005).")

(defun format-time-string (&optional format-string time)
  "Return a formatted date-time string of TIME or `get-decoded-time'.

FORMAT-STRING defaults to `*format-time-string-default*' and accepts
the 'date' command options except the following ones: %g, %G, %j, %N,
%U, %V, %W, %x, %X, %:z, %::z, %:::z and %Z."
  (let* ((time-string (or format-string
                          *format-time-string-default*)))
    (when (> 2 (length time-string))
      (error "FORMAT-STRING should contains at least two characters."))
    (multiple-value-bind (sec min hour dom mon year dow dstp tz)
        (or time (get-decoded-time))
      (loop
       for format-position = (position #\% time-string :start (or format-position 0))
       while format-position do
       (let* ((format-character (aref time-string (+ format-position 1)))
              (action (or (cdr (assoc format-character
                                      *format-time-string-alist*))
                          (error "Invalid format option %~C"
                                 format-character))))
         (setf time-string (concatenate 'string
                                        (subseq time-string 0 format-position)
                                        (funcall action sec min hour dom mon year dow dstp tz)
                                        (subseq time-string (+ format-position 2))))
         (when (char-equal #\% format-character) ; escape character
           (incf format-position)))))
    (format nil time-string)))

(defcommand echo-date () ()
  "Display the date and time."
  (message "~a" (format-time-string)))

(defcommand-alias time echo-date)

(defun programs-in-path (&optional full-path (path (split-string (getenv "PATH") ":")))
  "Return a list of programs in the path that start with @var{base}. if
@var{full-path} is @var{t} then return the full path, otherwise just
return the filename. @var{path} is by default the @env{PATH}
evironment variable but can be specified. It should be a string containing
each directory seperated by a colon."
  (loop
   for p in path
   for dir = (probe-path p)
   when dir
   nconc (loop
          for file in (directory (merge-pathnames (make-pathname :name :wild) dir))
          for namestring = (file-namestring file)
	    when (pathname-is-executable-p file)
	    collect (if full-path
			(namestring file)
			namestring))))

(defstruct path-cache
  programs modification-dates paths)

(defvar *path-cache* nil
  "A cache containing the programs in the path, used for completion.")

(defun rehash (&optional (paths (mapcar 'parse-namestring (split-string (getenv "PATH") ":"))))
  "Update the cache of programs in the path stored in @var{*programs-list*} when needed."
  (let ((dates (mapcar (lambda (p)
                         (when (probe-path p)
                           (portable-file-write-date p)))
                       paths)))
    (finish-output)
    (unless (and *path-cache*
                 (equal (path-cache-paths *path-cache*) paths)
                 (equal (path-cache-modification-dates *path-cache*) dates))
      (setf *path-cache* (make-path-cache :programs (programs-in-path nil paths)
                                          :modification-dates dates
                                          :paths paths)))))

(defun complete-program (base)
  "return the list of programs in @var{*path-cache*} whose names begin
with base. Automagically update the cache."
  (rehash)
  (remove-if-not #'(lambda (p)
		     (when (<= (length base) (length p))
                       (string= base p
                                :end1 (length base)
                                :end2 (length base)))) (path-cache-programs *path-cache*)))

(defcommand run-shell-command (cmd &optional collect-output-p) ((:shell "/bin/sh -c "))
  "Run the specified shell command. If @var{collect-output-p} is @code{T}
then run the command synchonously and collect the output. Be
careful. If the shell command doesn't return, it will hang StumpWM. In
such a case, kill the shell command to resume StumpWM."
  (if collect-output-p
      (run-prog-collect-output *shell-program* "-c" cmd)
      (run-prog *shell-program* :args (list "-c" cmd) :wait nil)))

(defcommand-alias exec run-shell-command)

(defcommand eval-line (cmd) ((:rest "Eval: "))
  (handler-case
      (message "^20~{~a~^~%~}"
               (mapcar 'prin1-to-string
                       (multiple-value-list (eval (read-from-string cmd)))))
    (error (c)
      (err "^B^1*~A" c))))

(defcommand-alias eval eval-line)

(defcommand echo (string) ((:rest "Echo: "))
  "Display @var{string} in the message bar."
  ;; The purpose of echo is always to pop up a message window.
  (let ((*executing-stumpwm-command* nil))
    (message "~a" string)))

(defun send-meta-key (screen key)
  "Send the prefix key"
  (when (screen-current-window screen)
    (send-fake-key (screen-current-window screen) key)))

(defcommand meta (key) ((:key "Key: "))
"Send a fake key to the current window. @var{key} is a typical StumpWM key, like @kbd{C-M-o}."
  (send-meta-key (current-screen) key))

(defcommand loadrc () ()
"Reload the @file{~/.stumpwmrc} file."
  (handler-case
      (progn
        (with-restarts-menu (load-rc-file nil)))
    (error (c)
      (message "^1*^BError loading rc file: ^n~A" c))
    (:no-error (&rest args)
      (declare (ignore args))
      (message "rc file loaded successfully."))))

(defcommand keyboard-quit () ()
    ""
  ;; This way you can exit from command mode
  (when (pop-top-map)
    (message "Exited.")))

(defcommand-alias abort keyboard-quit)


(defcommand quit () ()
"Quit StumpWM."
  (throw :top-level :quit))

(defcommand soft-restart () ()
  "Soft Restart StumpWM. The lisp process isn't restarted. Instead,
control jumps to the very beginning of the stumpwm program. This
differs from a theoretical hard restart, which would restart the unix
process."
  (throw :top-level :restart))

(defun run-or-raise (cmd props &optional (all-groups *run-or-raise-all-groups*) (all-screens *run-or-raise-all-screens*))
  "Run the shell command, @var{cmd}, unless an existing window
matches @var{props}. @var{props} is a property list with the following keys:

@table @code
@item :class
Match the window's class.
@item :instance
Match the window's instance or resource-name.
@item :role
Match the window's @code{WM_WINDOW_ROLE}.
@item :title
Match the window's title.
@end table

By default, the global @var{*run-or-raise-all-groups*} decides whether
to search all groups or the current one for a running
instance. @var{all-groups} overrides this default. Similarily for
@var{*run-or-raise-all-screens*} and @var{all-screens}."
  (labels
      ;; Raise the window win and select its frame.  For now, it
      ;; does not select the screen.
      ((goto-win (win)
         (let* ((group (window-group win))
                (frame (window-frame win))
                (old-frame (tile-group-current-frame group)))
           (frame-raise-window group frame win)
           (focus-all win)
           (unless (eq frame old-frame)
             (show-frame-indicator group))))
       (sort-windows-by-group (winlist)
         (stable-sort (sort winlist #'< :key #'window-number)
                      #'< :key #'(lambda (w) (group-number (window-group w)))))
       (find-window (winlist)
         (let* ((match (remove-if-not #'(lambda (w)
                                          (apply 'window-matches-properties-p w props))
                                      winlist))
                (match-sorted (sort-windows-by-group match))
                (rest (member (current-window) match-sorted)))
           (if (<= (length rest) 1) ; current win not a match or no matches left
               (car match-sorted)
               (cadr rest)))))
    (let* ((screens (if all-screens
                        *screen-list*
                        (list (current-screen))))
           (winlist (if all-groups
                        (mapcan (lambda (s) (screen-windows s)) screens)
                        (group-windows (current-group))))
           (win (find-window winlist)))
      (if win
          (goto-win win)
          (run-shell-command cmd)))))

(defcommand reload () ()
"Reload StumpWM using @code{asdf}."
  (message "Reloading StumpWM...")
  #+asdf (with-restarts-menu
             (asdf:operate 'asdf:load-op :stumpwm))
  #-asdf (message "^B^1*Sorry, StumpWM can only be reloaded with asdf (for now.)")
  #+asdf (message "Reloading StumpWM...^B^2*Done^n."))

(defcommand emacs () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise "emacs" '(:class "Emacs")))

(defcommand copy-unhandled-error () ()
  "When an unhandled error occurs, StumpWM restarts and attempts to
continue. Unhandled errors should be reported to the mailing list so
they can be fixed. Use this command to copy the unhandled error and
backtrace to the X11 selection so you can paste in your email when
submitting the bug report."
  (if *last-unhandled-error*
      (progn
        (set-x-selection (format nil "~a~%~a" (first *last-unhandled-error*) (second *last-unhandled-error*)))
        (message "Copied to clipboard."))
      (message "There was no unhandled error!")))

(defmacro defprogram-shortcut (name &key (command (string-downcase (string name)))
                                         (props `'(:class ,(string-capitalize command)))
                                         (map *top-map*)
                                         (key (kbd (concat "H-" (subseq command 0 1)))))
  "define a command and key binding to run or raise a program."
  `(progn
     (defcommand ,name () ()
       (run-or-raise ,command ,props))
     (define-key ,map ,key ,(string-downcase (string name)))))

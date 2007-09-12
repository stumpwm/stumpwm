;; Copyright (C) 2003 Shawn Betts
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

(defvar *root-map* nil
  "The default bindings that hang off the prefix key.")

;; Do it this way so its easier to wipe the map and get a clean one.
(when (null *root-map*)
  (setf *root-map*
	(let ((m (make-sparse-keymap)))
	  (define-key m (kbd "c") "exec xterm")
	  (define-key m (kbd "C-c") "exec xterm")
	  (define-key m (kbd "e") "exec emacs")
	  (define-key m (kbd "C-e") "exec emacs")
	  (define-key m (kbd "n") "pull-hidden-next")
	  (define-key m (kbd "C-n") "pull-hidden-next")
	  (define-key m (kbd "M-n") "next")
	  (define-key m (kbd "C-M-n") "next-in-frame")
	  (define-key m (kbd "SPC") "pull-hidden-next")
	  (define-key m (kbd "C-SPC") "pull-hidden-next")
	  (define-key m (kbd "p") "pull-hidden-previous")
	  (define-key m (kbd "C-p") "pull-hidden-previous")
	  (define-key m (kbd "M-p") "prev")
	  (define-key m (kbd "C-M-p") "prev-in-frame")
	  (define-key m (kbd "w") "windows")
	  (define-key m (kbd "C-w") "windows")
	  (define-key m (kbd "W") "syncplace")
	  (define-key m (kbd "k") "delete")
	  (define-key m (kbd "C-k") "delete")
	  (define-key m (kbd "K") "kill")
	  (define-key m (kbd "b") "banish")
	  (define-key m (kbd "C-b") "banish")
	  (define-key m (kbd "a") "time")
	  (define-key m (kbd "C-a") "time")
	  (define-key m (kbd "'") "select")
	  (define-key m (kbd "\"") "windowlist")
	  (define-key m (kbd "C-t") "pull-hidden-other")
	  (define-key m (kbd "M-t") "other-in-frame")
	  (define-key m (kbd "!") "exec")
	  (define-key m (kbd "C-g") "abort")
	  (define-key m (kbd "0") "pull 0")
	  (define-key m (kbd "1") "pull 1")
	  (define-key m (kbd "2") "pull 2")
	  (define-key m (kbd "3") "pull 3")
	  (define-key m (kbd "4") "pull 4")
	  (define-key m (kbd "5") "pull 5")
	  (define-key m (kbd "6") "pull 6")
	  (define-key m (kbd "7") "pull 7")
	  (define-key m (kbd "8") "pull 8")
	  (define-key m (kbd "9") "pull 9")
	  (define-key m (kbd "R") "remove")
	  (define-key m (kbd "s") "vsplit")
	  (define-key m (kbd "S") "hsplit")
	  (define-key m (kbd "r") "iresize")
	  (define-key m (kbd "o") "fnext")
	  (define-key m (kbd "TAB") "fother")
	  (define-key m (kbd "f") "fselect")
	  (define-key m (kbd "F") "curframe")
	  (define-key m (kbd "t") "meta C-t")
	  (define-key m (kbd "C-N") "number")
	  (define-key m (kbd ";") "colon")
	  (define-key m (kbd ":") "eval")
	  (define-key m (kbd "C-h") "help")
	  (define-key m (kbd "-") "fclear")
	  (define-key m (kbd "Q") "only")
	  (define-key m (kbd "Up") "move-focus up")
	  (define-key m (kbd "Down") "move-focus down")
	  (define-key m (kbd "Left") "move-focus left")
	  (define-key m (kbd "Right") "move-focus right")
	  (define-key m (kbd "M-Up") "move-window up")
	  (define-key m (kbd "M-Down") "move-window down")
	  (define-key m (kbd "M-Left") "move-window left")
	  (define-key m (kbd "M-Right") "move-window right")
	  (define-key m (kbd "v") "version")
	  (define-key m (kbd "#") "mark")
	  (define-key m (kbd "m") "lastmsg")
	  (define-key m (kbd "C-m") "lastmsg")
	  (define-key m (kbd "G") "vgroups")
	  (define-key m (kbd "g") '*groups-map*)
	  (define-key m (kbd "F1") "gselect 1")
	  (define-key m (kbd "F2") "gselect 2")
	  (define-key m (kbd "F3") "gselect 3")
	  (define-key m (kbd "F4") "gselect 4")
	  (define-key m (kbd "F5") "gselect 5")
	  (define-key m (kbd "F6") "gselect 6")
	  (define-key m (kbd "F7") "gselect 7")
	  (define-key m (kbd "F8") "gselect 8")
	  (define-key m (kbd "F9") "gselect 9")
	  (define-key m (kbd "F10") "gselect 10")
	  (define-key m (kbd "F11") "fullscreen")
	  (define-key m (kbd "?") "help")
	  (define-key m (kbd "+") "balance-frames")
	  (define-key m (kbd "A") "title")
	  (define-key m (kbd "h") '*help-map*)
	  m)))

(defvar *help-map* nil
  "Help related bindings hang from this keymap")

(when (null *help-map*)
  (setf *help-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "v") "describe-variable")
          (define-key m (kbd "f") "describe-function")
          (define-key m (kbd "k") "describe-key")
          (define-key m (kbd "w") "where-is")
          m)))

(defstruct command
  name args fn)

(defvar *command-hash* (make-hash-table :test 'equal)
  "A list of interactive stumpwm commands.")

(defmacro define-stumpwm-command (name (&rest args) &body body)
  `(setf (gethash ,name *command-hash*)
	 (make-command :name ,name
		       :args ',args
		       :fn (lambda (,@(mapcar 'first args))
			      ,@body))))

(defun all-commands ()
  "Return a list of all interactive commands."
  (let (acc)
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (push k acc))
	     *command-hash*)
    (sort acc 'string<)))

(defun restarts-menu (err)
  "Present a menu of restarts to the user and let them
choose. Run the selected restart."
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

(defmacro with-restarts-menu (&body body)
  "Execute BODY. If an error occurs allow the user to pick a
restart from a menu of possible restarts. If a restart is not
chosen, resignal the error."
  (let ((c (gensym)))
    `(handler-bind
         ((error 
           (lambda (,c)
             (restarts-menu ,c)
             (signal ,c))))
       ,@body)))

(defun focus-next-window (group)
  (focus-forward group (sort-windows group)))

(defun focus-prev-window (group)
  (focus-forward group
		 (reverse
		  (sort-windows group))))

(define-stumpwm-command "next" ()
  (let ((group (current-group)))
    (if (group-current-window group)
	(focus-next-window group)
	(other-window group))))

(define-stumpwm-command "prev" ()
  (let ((group (current-group)))
    (if (group-current-window group)
	(focus-prev-window group)
	(other-window group))))

(defun pull-window (win &optional (to-frame (tile-group-current-frame (window-group win))))
  (let ((f (window-frame win))
	(group (window-group win)))
    (setf (window-frame win) to-frame)
    (sync-frame-windows group to-frame)
    (frame-raise-window group to-frame win)
    ;; if win was focused in its old frame then give the old
    ;; frame the frame's last focused window.
    (when (eq (frame-window f) win)
      ;; the current value is no longer valid.
      (setf (frame-window f) nil)
      (frame-raise-window group f (first (frame-windows group f)) nil))))

;; In the future, this window will raise the window into the current
;; frame.
(defun focus-forward (group window-list &optional pull-p (predicate (constantly t)))
 "Set the focus to the next item in window-list from the focused
window. If PULL-P is T then pull the window into the current
frame."
  ;; The window with focus is the "current" window, so find it in the
  ;; list and give that window focus
  (let* ((w (group-current-window group))
	 (old-frame (tile-group-current-frame group))
	 (wins (remove-if-not predicate (cdr (member w window-list))))
	 (nw (if (null wins)
		 ;; If the last window in the list is focused, then
		 ;; focus the first one.
		 (car (remove-if-not predicate window-list))
                 ;; Otherwise, focus the next one in the list.
                 (first wins))))
    ;; there's still the case when the window is the only one in the
    ;; list, so make sure its not the same as the current window.
    (if (and nw
             (not (eq w nw)))
	(if pull-p
	    (pull-window nw)
	    (progn
	      (frame-raise-window group (window-frame nw) nw)
	      (unless (eq (window-frame nw) old-frame)
		(show-frame-indicator group))))
	(message "No other window."))))

(defun delete-current-window ()
  "Send a delete event to the current window."
  (let ((group (current-group)))
    (when (group-current-window group)
      (delete-window (group-current-window group)))))

(define-stumpwm-command "delete" ()
  (delete-current-window))

(defun kill-current-window ()
  "Kill the client of the current window."
  (let ((group (current-group)))
    (when (group-current-window group)
      (xwin-kill (window-xwin (group-current-window group))))))

(define-stumpwm-command "kill" ()
  (kill-current-window))

(defun banish-pointer ()
  "Move the pointer to the lower right corner of the head"
  (let ((group (current-group)))
    (warp-pointer (group-screen group)
		  (1- (+ (head-x (current-head)) (head-width (current-head))))
		  (1- (+ (head-y (current-head)) (head-height (current-head)))))))

(define-stumpwm-command "banish" ()
  (banish-pointer))

(define-stumpwm-command "ratwarp" ((x :number "X: ") (y :number "Y: "))
  (warp-pointer (current-screen) x y))

(define-stumpwm-command "ratrelwarp" ((dx :number "Delta X: ") (dy :number "Delta Y: "))
  (warp-pointer-relative dx dy))

;; FIXME: This function doesn't work.
(define-stumpwm-command "ratclick" ((button :number))
  (when (current-window)
    (send-fake-click (current-window) (or button 1))))

(defun echo-windows (group fmt windows)
  "Print a list of the windows to the screen."
  (let* ((wins (sort1 windows #'< :key #'window-number))
	 (highlight (position (group-current-window group) wins))
	 (names (mapcar (lambda (w)
			  (format-expand *window-formatters* fmt w)) wins)))
    (if (null wins)
	(echo-string (group-screen group) "No Managed Windows")
      (echo-string-list (group-screen group) names highlight))))

(defun fmt-window-list (group &optional head)
  (declare (ignore head))
  "Using *window-format*, return a 1 line list of the windows, space seperated."
  (format nil "~{~a~^ ~}"
	  (mapcar (lambda (w)
		    (format-expand *window-formatters* *window-format* w)) (sort-windows group))))

(defun fmt-group-list (group &optional head)
  (declare (ignore head))
  "Given a group list all the groups in the group's screen."
  (format nil "~{~a~^ ~}"
	  (mapcar (lambda (w)
		    (format-expand *group-formatters* *group-format* w)) (sort-windows group))))

(defun fmt-head (group head)
  (declare (ignore group))
  (format nil "~d" (head-number head)))

(defun fmt-head-window-list (group head)
  "Using *window-format*, return a 1 line list of the windows, space seperated."
  (format nil "~{~a~^ ~}"
	  (mapcar (lambda (w)
		    (format-expand *window-formatters* *window-format* w)) (sort1 (head-windows group head) #'< :key #'window-number))))

(define-stumpwm-command "windows" ((fmt :rest))
  (echo-windows (current-group) (or fmt *window-format*) (group-windows (current-group))))

(define-stumpwm-command "framewindows" ((fmt :rest))
  (echo-windows (current-group) (or fmt *window-format*) (frame-windows (current-group)
									(tile-group-current-frame (current-group)))))

(define-stumpwm-command "title" ((title :rest "Set window's title to: "))
  (if (current-window)
      (setf (window-user-title (current-window)) title)
      (message "No Focused Window")))

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
	     (format nil "~2,'0D" (if (> hour 12) (- hour 12) hour))))
    ;; %j   day of year (001..366)
    (#\k . ,(time-lambda (hour) (format nil "~2,D" hour)))
    (#\l . ,(time-lambda (hour)
	     (format nil "~2,D" (if (> hour 12) (- hour 12) hour))))
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
                  (setf hour-local hour am-pm "AM")))
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

(defun echo-date ()
  "Print the output of the 'date' command to the screen."
    (message "~a" (format-time-string)))

(define-stumpwm-command "time" ()
  (echo-date))

(defun select-window (group query)
  "Read input from the user and go to the selected window."
    (let (match)
      (labels ((match (win)
		      (let* ((wname (window-name win))
			     (end (min (length wname) (length query))))
			(string-equal wname query :end1 end :end2 end))))
	(unless (null query)
	  (setf match (find-if #'match (group-windows group))))
	(when match
	  (frame-raise-window group (window-frame match) match)))))

(define-stumpwm-command "select" ((win :window-name "Select: "))
  (select-window (current-group) win))

(defun select-window-number (group num)
  (labels ((match (win)
		  (= (window-number win) num)))
    (let ((win (find-if #'match (group-windows group))))
      (when win
	(frame-raise-window group (window-frame win) win)))))

(defun other-window (group)
  (let* ((wins (group-windows group))
	 ;; the frame could be empty
	 (win (if (group-current-window group)
		  (second wins)
		  (first wins))))
    (if win
	(frame-raise-window group (window-frame win) win)
	(echo-string (group-screen group) "No other window."))))

(define-stumpwm-command "other" ()
  (let* ((group (current-group))
	 (old-frame (tile-group-current-frame group)))
    (other-window (current-group))
    (unless (eq old-frame (tile-group-current-frame group))
      (show-frame-indicator group))))

(defun programs-in-path (base &optional full-path (path (split-string (getenv "PATH") ":")))
  "Return a list of programs in the path that start with BASE. if
FULL-PATH is T then return the full path, otherwise just return
the filename."
  (loop
     for p in path
     for dir = (probe-path p)
     when dir
     nconc (loop
	      for file in (directory (merge-pathnames (make-pathname :name :wild) dir))
	      for namestring = (file-namestring file)
	      when (and (string= base namestring
				 :end1 (min (length base)
					    (length namestring))
				 :end2 (min (length base)
					    (length namestring)))
			(pathname-is-executable-p file))
	      collect (if full-path 
			  (namestring file)
			  namestring))))

(defun run-shell-command (cmd &optional collect-output-p)
  "Run a shell command in the background or wait for it to finish
and collect the output if COLLECT-OUTPUT-P is T. Warning! if
COLLECT-OUTPUT-P is stumpwm will hang until your command
returns..which could be forever if you're not careful."
  (if collect-output-p
      (run-prog-collect-output *shell-program* "-c" cmd)
      (run-prog *shell-program* :args (list "-c" cmd) :wait nil)))

(define-stumpwm-command "exec" ((cmd :shell "/bin/sh -c "))
  (run-shell-command cmd))

(defun split-frame-in-dir (group dir)
  (let ((f (tile-group-current-frame group)))
      (if (split-frame group dir)
	  (progn
	    (when (frame-window f)
	      (update-window-border (frame-window f)))
	    (show-frame-indicator group))
	(message "Cannot split smaller than minimum size."))))

(define-stumpwm-command "hsplit" ()
  (split-frame-in-dir (current-group) :column))

(define-stumpwm-command "vsplit" ()
  (split-frame-in-dir (current-group) :row))

(defun remove-split (group)
  (let* ((s (closest-sibling (tile-group-frame-tree group)
                             (tile-group-current-frame group)))
	 ;; grab a leaf of the siblings. The siblings doesn't have to be
	 ;; a frame.
	 (l (tree-accum-fn s
                           (lambda (&rest siblings)
                             (car siblings))
                           #'identity)))

    (if (frame-is-head group l)
	(message "No more frames!")
      (progn
    ;; Only remove the current frame if it has a sibling
    (dformat 3 "~S~%" s)
    (when s
	  (when (frame-is-head group (tile-group-current-frame group))
	    (setf (frame-number l) (frame-number (tile-group-current-frame group))))
      (dformat 3 "~S~%" l)
      ;; Move the windows from the removed frame to its sibling
      (migrate-frame-windows group (tile-group-current-frame group) l)
      ;; If the frame has no window, give it the current window of
      ;; the current frame.
      (unless (frame-window l)
	(setf (frame-window l)
	      (frame-window (tile-group-current-frame group))))
      ;; Unsplit
      (setf (tile-group-frame-tree group)
	    (remove-frame (tile-group-frame-tree group)
			  (tile-group-current-frame group)))
      ;; update the current frame and sync all windows
      (setf (tile-group-current-frame group) l)
      (tree-iterate (tile-group-frame-tree group)
		    (lambda (leaf)
		      (sync-frame-windows group leaf)))
      (frame-raise-window group l (frame-window l))
      (when (frame-window l)
	(update-window-border (frame-window l)))
	  (show-frame-indicator group))))))

(define-stumpwm-command "remove" ()
  (remove-split (current-group)))

(define-stumpwm-command "only" ()
  (let* ((screen (current-screen))
	 (group (screen-current-group screen))
	 (win (frame-window (tile-group-current-frame group)))
	 (head (current-head group))
	 (frame (copy-frame head)))
    (if (atom (tile-group-frame-head group head))
	(message "There's only one frame.")
      (progn
	(mapc (lambda (w)
		;; windows in other frames disappear
		(unless (eq (window-frame w) (tile-group-current-frame group))
		  (hide-window w))
		(setf (window-frame w) frame))
	      (head-windows group head))
	(setf (frame-window frame) win
	      (tile-group-frame-head group head) frame 
	      (tile-group-current-frame group) frame)
	(focus-frame group frame)
	(when (frame-window frame)
	  (update-window-border (frame-window frame)))
	(sync-frame-windows group (tile-group-current-frame group))))))

(define-stumpwm-command "fullscreen" ()
  "Toggle the fullscreen mode of the current widnow. Use this for clients
with broken (non-NETWM) fullscreen implemenations, such as any program
using SDL."
  (update-fullscreen (current-window) 2))

(define-stumpwm-command "curframe" ()
  (show-frame-indicator (current-group)))

(defun focus-frame-next-sibling (group)
  (let* ((sib (next-sibling (tile-group-frame-tree group)
                            (tile-group-current-frame group))))
    (when sib
      (focus-frame group (tree-accum-fn sib
                                        (lambda (x y)
                                          (declare (ignore y))
                                          x)
                                        'identity))
      (show-frame-indicator group))))

(defun focus-last-frame (group)
  ;; make sure the last frame still exists in the frame tree
  (when (and (tile-group-last-frame group)
	     (find (tile-group-last-frame group) (group-frames group)))
    (focus-frame group (tile-group-last-frame group))
    (show-frame-indicator group)))

(defun focus-frame-after (group frames)
  "Given a list of frames focus the next one in the list after
the current frame."
  (let ((rest (cdr (member (tile-group-current-frame group) frames :test 'eq))))
    (focus-frame group
		 (if (null rest)
		     (car frames)
		     (car rest)))))

(defun focus-next-frame (group)
  (focus-frame-after group (group-frames group))
  (show-frame-indicator group))

(defun focus-prev-frame (group)
  (focus-frame-after group (nreverse (group-frames group)))
  (show-frame-indicator group))

(define-stumpwm-command "fnext" ()
  (focus-next-frame (current-group)))

(define-stumpwm-command "sibling" ()
  (focus-frame-next-sibling (current-group)))

(define-stumpwm-command "fother" ()
  (focus-last-frame (current-group)))

(defun choose-frame-by-number (group)
  "show a number in the corner of each frame and wait for the user to
select one. Returns the selected frame or nil if aborted."
  (let* ((wins (progn
		 (draw-frame-outlines group)
		 (draw-frame-numbers group)))
	 (ch (read-one-char (group-screen group)))
	 (num (read-from-string (string ch) nil nil)))
    (dformat 3 "read ~S ~S~%" ch num)
    (mapc #'xlib:destroy-window wins)
    (clear-frame-outlines group)
    (find ch (group-frames group)
	  :test 'char=
	  :key 'get-frame-number-translation)))


(define-stumpwm-command "fselect" ((f :frame t))
  (let ((group (current-group)))
    (focus-frame group f)
    (show-frame-indicator group)))

(define-stumpwm-command "resize" ((w :number "+ Width: ")
				  (h :number "+ Height: "))
      (let* ((group (current-group))
             (f (tile-group-current-frame group)))  
    (if (atom (tile-group-frame-tree group))
	(message "No more frames!")
      (progn
    	(clear-frame-outlines group)
        (resize-frame group f w :width)
	(resize-frame group f h :height)
	(draw-frame-outlines group (current-head))))))

(defun eval-line (cmd)
  (handler-case 
      (message "~{~a~^~%~}"
               (mapcar 'prin1-to-string
                       (multiple-value-list (eval (read-from-string cmd)))))
    (error (c)
      (message "~A" c))))

(define-stumpwm-command "eval" ((cmd :rest "Eval: "))
  (eval-line cmd))

(define-stumpwm-command "echo" ((s :rest "Echo: "))
  (message "~a" s))

;; Simple command & arg parsing
(defun split-by-one-space (string)
  "Returns a list of substrings of string divided by ONE space each.
Note: Two consecutive spaces will be seen as if there were an empty
string between them."
  (loop for i = 0 then (1+ j)
	as j = (position #\Space string :start i)
	collect (subseq string i j)
	while j))

(defstruct argument-line
  string start)

(defvar *command-type-hash* (make-hash-table)
  "A hash table of types and functions to deal with these types.")

(defun argument-line-end-p (input)
  "Return T if we're outta arguments from the input line."
  (>= (argument-line-start input)
      (length (argument-line-string input))))

(defun argument-pop (input)
  "Pop the next argument off."
  (unless (argument-line-end-p input)
    (let* ((p1 (position-if-not (lambda (ch)
				  (char= ch #\Space))
				(argument-line-string input)
				:start (argument-line-start input)))
	   (p2 (or (and p1 (position #\Space (argument-line-string input) :start p1))
		   (length (argument-line-string input)))))
      (prog1
	  ;; we wanna return nil if they're the same
	  (unless (= p1 p2)
	    (subseq (argument-line-string input) p1 p2))
	(setf (argument-line-start input) (1+ p2))))))

(defun argument-pop-or-read (input prompt &optional completions)
  (or (argument-pop input)
      (if completions
	  (completing-read (current-screen) prompt completions)
	  (read-one-line (current-screen) prompt))
      (throw 'error :abort)))

(defun argument-pop-rest (input)
  "Return the remainder of the argument text."
  (unless (argument-line-end-p input)
    (prog1
	(subseq (argument-line-string input) (argument-line-start input))
      (setf (argument-line-start input) (length (argument-line-string input))))))

(defun argument-pop-rest-or-read (input prompt &optional completions)
  (or (argument-pop-rest input)
      (if completions
	  (completing-read (current-screen) prompt completions)
	  (read-one-line (current-screen) prompt))
      (throw 'error :abort)))

(defmacro define-stumpwm-type (type (input prompt) &body body)
  `(setf (gethash ,type *command-type-hash*) 
	 (lambda (,input ,prompt)
	   ,@body)))

(defun lookup-symbol (string)
  ;; FIXME: should we really use string-upcase?
  (let* ((ofs (split-string string ":"))
         (pkg (if (> (length ofs) 1)
                  (find-package (string-upcase (pop ofs)))
                  *package*))
         (var (string-upcase (pop ofs)))
         (ret (find-symbol var pkg)))
    (when (plusp (length ofs))
      (throw 'error "Too many :'s"))
    (if ret
        (values ret pkg var)
        (throw 'error (format nil "No such symbol: ~a::~a."
                              (package-name pkg) var)))))

(define-stumpwm-type :variable (input prompt)
  (lookup-symbol (argument-pop-or-read input prompt)))

(define-stumpwm-type :function (input prompt)
  (multiple-value-bind (sym pkg var)
      (lookup-symbol (argument-pop-or-read input prompt))
    (if (symbol-function sym)
        (symbol-function sym)
        (throw 'error (format nil "the symbol ~a::~a has no function."
                              (package-name pkg) var)))))

(define-stumpwm-type :key-seq (input prompt)
  (labels ((update (seq)
             (message "~a: ~{~a ~}" 
                      prompt
                      (mapcar 'print-key (reverse seq)))))
    (let ((rest (argument-pop-rest input)))
      (or (and rest (parse-key-seq rest))
          ;; read a key sequence from the user
          (unwind-protect
               (progn
                 (grab-keyboard (current-screen))
                 (message "~a" prompt)
                 (nreverse (second (multiple-value-list
                                    (read-from-keymap *top-map* #'update)))))
            (ungrab-keyboard))))))

(define-stumpwm-type :window-number (input prompt)
  (let ((n (or (argument-pop input)
	       (completing-read (current-screen)
				prompt
				(mapcar 'prin1-to-string
					(mapcar 'window-number 
						(group-windows (current-group))))))))
    (when n
      (handler-case
	  (parse-integer n)
	(parse-error (c)
	  (declare (ignore c))
	  (throw 'error "Number required."))))))

(define-stumpwm-type :number (input prompt)
  (let ((n (or (argument-pop input)
	       (read-one-line (current-screen) prompt))))
    (when n
      (handler-case
	  (parse-integer n)
	(parse-error (c)
	  (declare (ignore c))
	  (throw 'error "Number required."))))))
  

(define-stumpwm-type :string (input prompt)
  (or (argument-pop input)
      (read-one-line (current-screen) prompt)))

(define-stumpwm-type :key (input prompt)
  (let ((s (or (argument-pop input)
	       (read-one-line (current-screen) prompt))))
    (when s
      (kbd s))))

(define-stumpwm-type :window-name (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen) prompt
		       (mapcar 'window-name 
			       (group-windows (current-group))))))

(define-stumpwm-type :gravity (input prompt)
  (let* ((values '(("center" :center)
                   ("top" :top)
                   ("right" :right)
                   ("bottom" :bottom)
                   ("left" :left) 
                   ("top-right" :top-right)
                   ("top-left" :top-left)
                   ("bottom-right" :bottom-right)
                   ("bottom-left" :bottom-left)))
         (gravity (second (assoc (string-trim " " (argument-pop-or-read input prompt values)) values :test 'string-equal))))
    (or gravity
        (throw 'error "No matching gravity."))))
    
(define-stumpwm-type :group-name (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen) prompt
		       (mapcar 'group-name
			       (screen-groups (current-screen))))))

(define-stumpwm-type :group (input prompt)
  (or 
   (find (string-trim " " (or (argument-pop input)
			     (completing-read (current-screen) prompt
					      (mapcar 'group-name
						      (screen-groups (current-screen))))))
	(screen-groups (current-screen))
	:test 'string=
	:key 'group-name)
   (throw 'error "No Such Group.")))

(define-stumpwm-type :frame (input prompt)
  (declare (ignore prompt))
  (let ((arg (argument-pop input)))
    (if arg
	(or (find arg (group-frames (current-group))
		  :key (lambda (f)
			 (string (get-frame-number-translation f)))
		  :test 'string=)
	    (throw 'error "Frame not found."))
	(or (choose-frame-by-number (current-group))
	    (throw 'error :abort)))))

(define-stumpwm-type :shell (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt 'programs-in-path)))

(define-stumpwm-type :rest (input prompt)
  (or (argument-pop-rest input)
      (read-one-line (current-screen) prompt)))

(defun parse-and-run-command (input)
  "Parse the command and its arguments given the commands argument
specifications then execute it. Returns a string or nil if user
aborted."
  ;; Catch parse errors
  (catch 'error
    (let* ((arg-line (make-argument-line :string input
					 :start 0))
	   (cmd-str (argument-pop arg-line))
	   (cmd (or (gethash cmd-str *command-hash*)
		    (throw 'error (format nil "Command '~a' not found." cmd-str))))
	   (arg-specs (command-args cmd))
	   ;; Create a list of args to pass to the function. If the
	   ;; input is snarfed and we have more args, then prompt the
	   ;; user for a value.
	   (args (mapcar (lambda (spec)
			   (let* ((type (second spec))
				  (prompt (third spec))
				  (fn (gethash type *command-type-hash*)))
			     (unless fn
			       (throw 'error (format nil "Bad argument type: ~s" type)))
			     ;; If the prompt is NIL then it's
			     ;; considered an optional argument
			     ;; whose value is nil.
			     (if (and (null prompt)
				      (argument-line-end-p arg-line))
				 nil
				 ;; FIXME: Is it presumptuous to assume NIL means abort?
				 (or (funcall fn arg-line prompt)
				     (throw 'error :abort)))))
			 arg-specs)))
      (dformat 3 "arguments: ~S~%" args)
      ;; Did the whole string get parsed?
      (unless (or (argument-line-end-p arg-line)
		  (position-if 'alphanumericp (argument-line-string arg-line) :start (argument-line-start arg-line)))
	(throw 'error (format nil "Trailing garbage: ~{~A~^ ~}" (subseq (argument-line-string arg-line) 
									(argument-line-start arg-line)))))
      ;; Success
      (prog1
	  (apply (command-fn cmd) args)
	(setf *last-command* (command-name cmd))))))

(defun interactive-command (cmd)
  "exec cmd and echo the result."
  (let ((result (handler-case (parse-and-run-command cmd)
                  (error (c)
                    (format nil "Error In Command '~a': ~A" cmd c)))))
    ;; interactive commands update the modeline
    (update-screen-mode-lines) 
    (cond ((stringp result)
           (message "~a" result))
          ((eq result :abort)
           (unless *suppress-abort-messages*
             (message "Abort."))))))

(define-stumpwm-command "colon" ((initial-input :rest))
  (let ((cmd (completing-read (current-screen) ": " (all-commands) (or initial-input ""))))
    (unless cmd
      (throw 'error :abort))
    (when (plusp (length cmd))
      (interactive-command cmd))))

(defun pull-window-by-number (group n)
  "Pull window N from another frame into the current frame and focus it."
  (let ((win (find n (group-windows group) :key 'window-number :test '=)))
    (when win
      (pull-window win))))

(define-stumpwm-command "pull" ((n :window-number "Pull: "))
  (pull-window-by-number (current-group) n))

(defun send-meta-key (screen key)
  "Send the prefix key"
  (when (screen-current-window screen)
    (send-fake-key (screen-current-window screen) key)))

(define-stumpwm-command "meta" ((key :key "Key: "))
  (send-meta-key (current-screen) key))

(defun renumber (group nt)
  "Renumber the current window"
  (let ((nf (window-number (group-current-window group)))
	(win (find-if #'(lambda (win)
			  (= (window-number win) nt))
		      (group-windows group))))
    ;; Is it already taken?
    (if win
	(progn
	  ;; swap the window numbers
	  (setf (window-number win) nf)
	  (setf (window-number (group-current-window group)) nt))
      ;; Just give the window the number
      (setf (window-number (group-current-window group)) nt))))

(define-stumpwm-command "number" ((n :number "Number: "))
  (renumber (current-group) n))

(define-stumpwm-command "gravity" ((gravity :gravity "Gravity: "))
  (when (current-window)
    (setf (window-gravity (current-window)) gravity)
    (maximize-window (current-window))))

(define-stumpwm-command "loadrc" ()
  (handler-case
      (progn
        (with-restarts-menu (load-rc-file nil)))
    (error (c)
      (message "Error loading rc file: ~A" c))
    (:no-error (&rest args)
      (declare (ignore args))
      (message "rc file loaded successfully."))))

(defun columnize (list columns &key col-aligns (pad 1) (char #\Space) (align :left))
  ;; only somewhat nasty
  (let* ((rows (truncate (length list) columns))
         (data (loop for i from 0 below (length list) by rows
                  collect (subseq list i (min (+ i rows) (length list)))))
         (max (mapcar (lambda (col)
                        (reduce 'max col :key 'length :initial-value 0))
                      data))
         (padstr (make-string pad :initial-element char)))
    (apply 'mapcar 'concat
           ;; normalize width
           (loop
              for i in data
              for j in max
              for c from 0
              collect (loop
                         for k from 0 below rows
                         for s = (or (nth k i) "")
                         for len = (make-string (- j (length s))
                                                :initial-element char)
                         collect (ecase (or (nth c col-aligns) align)
                                   (:left (format nil "~a~a~a" (if (= c 0) "" padstr) s len))
                                   (:right (format nil "~a~a~a" (if (= c 0) "" padstr) len s))))))))

(defun display-keybinding (kmap-var)
  (let* ((screen (current-screen))
         (data (mapcar-hash (lambda (k v) (format nil "~5a ~a" (print-key k) v)) (symbol-value kmap-var)))
         (cols (ceiling (length data)
			(truncate (head-height (current-head))
                                  (font-height (screen-font screen))))))
    (message-no-timeout "Prefix: ~{~a~^ | ~}~%~{~a~^~%~}"
                        (mapcar 'print-key-seq (search-kmap kmap-var *top-map*))
                        (columnize data cols))))

(define-stumpwm-command "help" ()
  (display-keybinding '*root-map*))

;; Trivial function
(define-stumpwm-command "abort" ()
  ;; This way you can exit from command mode
  (when (pop-top-map)
    (message "Exited.")))

(defun set-prefix-key (key)
  "Change the stumpwm prefix key to KEY."
  (check-type key key)
  (let (prefix)
    (dolist (i (lookup-command *top-map* '*root-map*))
      (setf prefix i)
      (undefine-key *top-map* i))
    (define-key *top-map* key '*root-map*)
    (let* ((meta (make-key :keysym (key-keysym key)))
	   (old-cmd (concatenate 'string "meta " (print-key prefix)))
	   (cmd (concatenate 'string "meta " (print-key key))))
      (dolist (i (lookup-command *root-map* old-cmd))
	(undefine-key *root-map* i))
      (define-key *root-map* meta cmd))
    (define-key *root-map* key "other")
    (sync-keys)))

(define-stumpwm-command "quit" ()
  (throw :quit nil))

(defun clear-frame (frame group)
  "Clear the given frame."
  (frame-raise-window group frame nil (eq (tile-group-current-frame group) frame)))

(define-stumpwm-command "fclear" ()
  (clear-frame (tile-group-current-frame (current-group)) (current-group)))

(defun get-edge (frame edge)
  "Returns the specified edge of FRAME.  Valid values for EDGE are :TOP, :BOTTOM, :LEFT, and :RIGHT.
  An edge is a START, END, and OFFSET. For horizontal edges, START is the left coordinate, END is
  the right coordinate, and OFFSET is the Y coordinate.  Similarly, for vertical lines, START is
  top, END is bottom, and OFFSET is X coordinate."
  (let* ((x1 (frame-x frame))
         (y1 (frame-y frame))
         (x2 (+ x1 (frame-width frame)))
         (y2 (+ y1 (frame-height frame))))
    (ecase edge
      (:top
       (values x1 x2 y1))
      (:bottom
       (values x1 x2 y2))
      (:left
       (values y1 y2 x1))
      (:right
       (values y1 y2 x2)))))

(defun neighbour (direction frame frameset)
  "Returns the best neighbour of FRAME in FRAMESET on the DIRECTION edge.
   Valid directions are :UP, :DOWN, :LEFT, :RIGHT.
   eg: (NEIGHBOUR :UP F FS) finds the frame in FS that is the 'best'
   neighbour above F."
  (let ((src-edge (ecase direction
                    (:up :top)
                    (:down :bottom)
                    (:left :left)
                    (:right :right)))
        (opposite (ecase direction
                    (:up :bottom)
                    (:down :top)
                    (:left :right)
                    (:right :left)))
        (best-frame nil)
        (best-overlap 0))
    (multiple-value-bind (src-s src-e src-offset)
        (get-edge frame src-edge)
      (dolist (f frameset)
        (multiple-value-bind (s e offset)
            (get-edge f opposite)
          (let ((overlap (- (min src-e e)
                            (max src-s s))))
            ;; Two edges are neighbours if they have the same offset and their starts and ends
            ;; overlap.  We want to find the neighbour that overlaps the most.
            (when (and (= src-offset offset)
                       (>= overlap best-overlap))
                (setf best-frame f)
                (setf best-overlap overlap))))))
    best-frame))

(defun move-focus-and-or-window (dir &optional win-p)
  (let* ((group (current-group))
         (direction (intern (string-upcase dir) :keyword))
         (new-frame (neighbour direction (tile-group-current-frame group) (group-frames group)))
    	 (window (current-window)))
    (when new-frame
      (focus-frame group new-frame)
      (when win-p
	      (pull-window window)))
    (show-frame-indicator group)))

(define-stumpwm-command "move-focus" ((dir :string "Direction: "))
  (move-focus-and-or-window dir))

(define-stumpwm-command "move-window" ((dir :string "Direction: "))
  (move-focus-and-or-window dir t))

(defun run-or-raise (cmd props &optional (all-groups *run-or-raise-all-groups*) (all-screens *run-or-raise-all-screens*))
  "If a window matching PROPS can be found, select it.  Otherwise simply run cmd."
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
     (find-window (group)
		  (find-if (lambda (w)
			     (apply 'window-matches-properties-p w props))
			   (group-windows group))))
    (let*
      ((screens (if all-screens
		  *screen-list*
		  (list (current-screen))))
       (win
	 ;; If no qualifiers are set don't bother looking for a match.
	 ;; search all groups
	 (if all-groups
	   (loop named outer
		 for s in screens
		 do (loop
		      for g in (screen-groups s)
		      for win = (find-window g)
		      when win
		      do (return-from outer win)))
	   (find-window (current-group)))))
      (if win
	(goto-win win)
	(run-shell-command cmd)))))

(define-stumpwm-command "escape" ((key :string "Key: "))
  (set-prefix-key (kbd key)))

(defvar *lastmsg-nth* nil)

(define-stumpwm-command "lastmsg" ()
  ;; Allow the user to go back through the message history
  (if (string= *last-command* "lastmsg")
      (progn
	(incf *lastmsg-nth*)
	(if (>= *lastmsg-nth* (length (screen-last-msg (current-screen))))
	    (setf *lastmsg-nth* 0)))
      (setf *lastmsg-nth* 0))
  (if (screen-last-msg (current-screen))
      (echo-nth-last-message (current-screen) *lastmsg-nth*)
      (message "No last message.")))

;;; A resize minor mode. Something a bit better should probably be
;;; written. But it's an interesting way of doing it.

(defvar *resize-map* nil
  "The keymap used for resizing a window")

(defvar *resize-backup* nil)

(defvar *resize-increment* 10
  "Number of pixels to increment by when interactively resizing frames.")

(defun set-resize-increment (val)
  (setf *resize-increment* val)
  (update-resize-map))

(defun update-resize-map ()
  (let ((m (or *resize-map* (setf *resize-map* (make-sparse-keymap)))))
    (let ((i *resize-increment*))
      (labels ((dk (m k c)
		   (define-key m k (format nil c i))))
	(dk m (kbd "Up") "resize 0 -~D")
	(dk m (kbd "C-p") "resize 0 -~D")
	(dk m (kbd "p") "resize 0 -~D")
	(dk m (kbd "k") "resize 0 -~D")

	(dk m (kbd "Down") "resize 0 ~D")
	(dk m (kbd "C-n") "resize 0 ~D")
	(dk m (kbd "n") "resize 0 ~D")
	(dk m (kbd "j") "resize 0 ~D")

	(dk m (kbd "Left") "resize -~D 0")
	(dk m (kbd "C-b") "resize -~D 0")
	(dk m (kbd "b") "resize -~D 0")
	(dk m (kbd "h") "resize -~D 0")

	(dk m (kbd "Right") "resize ~D 0")
	(dk m (kbd "C-f") "resize ~D 0")
	(dk m (kbd "f") "resize ~D 0")
	(dk m (kbd "l") "resize ~D 0")
	(define-key m (kbd "RET") "exit-iresize")
	(define-key m (kbd "C-g") "abort-iresize")
	(define-key m (kbd "ESC") "abort-iresize")))))

(update-resize-map)

(define-stumpwm-command "iresize" ()
  (let ((frame (tile-group-current-frame (current-group))))
    (if	(atom (tile-group-frame-head (current-group) (frame-head (current-group) frame)))
      (message "There's only 1 frame!")
      (progn
	(when *resize-hides-windows*
	  (dolist (f (head-frames (current-group) (current-head)))
	    (clear-frame f (current-group))))
	(draw-frame-outlines (current-group) (current-head))
	(message "Resize Frame")
	(push-top-map *resize-map*))
      ;;   (setf *resize-backup* (copy-frame-tree (current-group)))
      )))

(defun resize-unhide ()
  (clear-frame-outlines (current-group))
  (when *resize-hides-windows*
    (dolist (w (reverse (head-windows (current-group) (current-head))))
      (setf (frame-window (window-frame w)) w)
      (raise-window w))
    (when (current-window)
      (focus-window (current-window)))))

(define-stumpwm-command "abort-iresize" ()
  (resize-unhide)
  (message "Abort resize")
  ;; TODO: actually revert the frames
  (pop-top-map))

(define-stumpwm-command "exit-iresize" ()
  (resize-unhide)
  (message "Resize Complete")
  (pop-top-map))

;;; group commands

;; FIXME: groups are to screens exactly as windows are to
;; groups. There is a lot of duplicate code that could be globbed
;; together.

(defvar *groups-map* nil
  "The default group related bindings hang off this map.")

(when (null *groups-map*)
  (setf *groups-map*
	(let ((m (make-sparse-keymap)))
	  (define-key m (kbd "g") "groups")
	  (define-key m (kbd "c") "gnew")
	  (define-key m (kbd "n") "gnext")
	  (define-key m (kbd "C-n") "gnext")
	  (define-key m (kbd "SPC") "gnext")
	  (define-key m (kbd "C-SPC") "gnext")
	  (define-key m (kbd "p") "gprev")
	  (define-key m (kbd "C-p") "gprev")
	  (define-key m (kbd "'") "gselect")
	  (define-key m (kbd "m") "gmove")
	  (define-key m (kbd "M") "gmove-marked")
	  (define-key m (kbd "k") "gkill")
	  (define-key m (kbd "1") "gselect 1")
	  (define-key m (kbd "2") "gselect 2")
	  (define-key m (kbd "3") "gselect 3")
	  (define-key m (kbd "4") "gselect 4")
	  (define-key m (kbd "5") "gselect 5")
	  (define-key m (kbd "6") "gselect 6")
	  (define-key m (kbd "7") "gselect 7")
	  (define-key m (kbd "8") "gselect 8")
	  (define-key m (kbd "9") "gselect 9")
	  (define-key m (kbd "0") "gselect 10")
	  m)))

(defun group-forward (current list)
  (let ((ng (next-group current list)))
    (when ng
      (switch-to-group ng))))

(define-stumpwm-command "gnew" ((name :string "Group Name: "))
  (let ((group (add-group (current-screen) name)))
    (if group
      (switch-to-group group)
      (message "Groups must have a name!"))))

(define-stumpwm-command "gnewbg" ((name :string "Group Name: "))
  (unless (find-group (current-screen) name)
    (add-group (current-screen) name)))

(define-stumpwm-command "gnext" ()
  (group-forward (current-group)
		 (sort-groups (current-screen))))

(define-stumpwm-command "gprev" ()
  (group-forward (current-group)
		 (reverse (sort-groups (current-screen)))))

(defun echo-groups (screen fmt &optional verbose (wfmt *window-format*))
  "Print a list of the windows to the screen."
  (let* ((groups (sort-groups screen))
	 (names (reduce 'nconc 
			(mapcar (lambda (g)
				  (list*
				   (format-expand *group-formatters* fmt g)
				   (when verbose
				     (mapcar (lambda (w)
					       (format-expand *window-formatters*
							      (concatenate 'string "  " wfmt)
							      w))
					     (sort-windows g)))))
				(if *list-hidden-groups* groups (remove 1 groups :test #'> :key #'group-number))))))
    (echo-string-list screen names)))

(define-stumpwm-command "groups" ((fmt :rest))
  (echo-groups (current-screen) (or fmt *group-format*)))

(define-stumpwm-command "vgroups" ((gfmt :string) (wfmt :rest))
  (echo-groups (current-screen)
	       (or gfmt *group-format*)
	       t (or wfmt *window-format*)))

(defun select-group (screen query)
  "Read input from the user and go to the selected window."
  (let (match
	(num (ignore-errors (parse-integer query))))
    (labels ((match (grp)
	       (let* ((name (group-name grp))
		      (end (min (length name) (length query))))
		 ;; try by name or number
		 (or (string-equal name query :end1 end :end2 end)
		     (eql (group-number grp) num)))))
      (unless (null query)
	(setf match (find-if #'match (screen-groups screen))))
      (when match
	(switch-to-group match)))))

(define-stumpwm-command "gselect" ((query :group-name "Select Group: "))
  (select-group (current-screen) query))

(define-stumpwm-command "gmove" ((to-group :group "To Group: "))
  (when (and to-group
	     (current-window))
    (move-window-to-group (current-window) to-group)))

(define-stumpwm-command "gmove-marked" ((to-group :group "To Group: "))
  (when to-group
    (let ((group (current-group)))
      (dolist (i (marked-windows group))
	(setf (window-marked i) nil)
	(move-window-to-group i to-group)))))

(define-stumpwm-command "gkill" ()
  (let ((dead-group (current-group))
	(to-group (next-group (current-group))))
    (switch-to-group to-group)
    (kill-group dead-group to-group)))

(define-stumpwm-command "gmerge" ((from :group "From Group: "))
  (if (eq from (current-group))
      (message "Cannot merge group with itself!")
      (merge-groups from (current-group))))

;;; interactive menu

(defvar *menu-map* nil
  "The keymap used by the interactive menu.")

(when (null *menu-map*)
  (setf *menu-map*
	(let ((m (make-sparse-keymap)))
	  (define-key m (kbd "C-p") 'menu-up)
	  (define-key m (kbd "Up") 'menu-up)
	  (define-key m (kbd "k") 'menu-up)

	  (define-key m (kbd "C-n") 'menu-down)
	  (define-key m (kbd "Down") 'menu-down)
	  (define-key m (kbd "j") 'menu-down)
	  (define-key m (kbd "C-g") 'menu-abort)
	  (define-key m (kbd "ESC") 'menu-abort)
	  (define-key m (kbd "RET") 'menu-finish)
	  m)))

(defstruct menu-state
  table prompt selected)

(defun bound-check-menu (menu)
  (setf (menu-state-selected menu)
	(cond ((< (menu-state-selected menu) 0)
	       (1- (length (menu-state-table menu))))
	      ((>= (menu-state-selected menu) (length (menu-state-table menu)))
	       0)
	      (t (menu-state-selected menu)))))
  
(defun menu-up (menu)
  (decf (menu-state-selected menu))
  (bound-check-menu menu))

(defun menu-down (menu)
  (incf (menu-state-selected menu))
  (bound-check-menu menu))

(defun menu-finish (menu)
  (throw :menu-quit (nth (menu-state-selected menu) (menu-state-table menu))))

(defun menu-abort (menu)
  (declare (ignore menu))
  (throw :menu-quit nil))

(defun select-from-menu (screen table &optional prompt (initial-selection 0))
  "Prompt the user to select from a menu on SCREEN. TABLE can be
a list of values or an alist. If it's an alist, the CAR of each
element is displayed in the menu. What is displayed as menu items
must be strings. Returns the selected element in TABLE or nil if aborted.

See *menu-map* for menu bindings."
  (check-type screen screen)
  (check-type table list)
  (check-type prompt (or null string))
  (check-type initial-selection integer)
  (let* ((menu (make-menu-state
	       :table table
	       :prompt prompt
	       :selected initial-selection))
        (menu-options (mapcar (lambda (elt)
                                (if (listp elt)
                                    (first elt)
                                    elt))
                              table))
        (menu-text (if prompt 
                       (cons prompt menu-options)
                       menu-options))
	(*record-last-msg-override* t)
	(*suppress-echo-timeout* t))
    (bound-check-menu menu)
    (catch :menu-quit
      (grab-keyboard screen)
      (unwind-protect
	   (loop
	      (echo-string-list screen menu-text
                                (+ (menu-state-selected menu) (if prompt 1 0)))
	      (let ((action (read-from-keymap *menu-map*)))
		(when action
		  (funcall action menu))))
	(ungrab-keyboard)
	(unmap-all-message-windows)))))

(define-stumpwm-command "windowlist" ((fmt :rest))
  (if (null (group-windows (current-group)))
      (message "No Managed Windows")
      (let* ((group (current-group))
             (window (second (select-from-menu
                              (current-screen)
                              (mapcar (lambda (w)
                                        (list (format-expand *window-formatters* (or fmt *window-format*) w) w))
                                      (sort-windows group))))))

        (if window
            (frame-raise-window group (window-frame window) window)
            (throw 'error :abort)))))

(define-stumpwm-command "reload" ()
  (message "Reloading StumpWM...")
  #+asdf (with-restarts-menu
             (asdf:operate 'asdf:load-op :stumpwm))
  #-asdf (message "Stumpwm can only be reloaded with asdf, for now.")
  #+asdf (message "Reloading StumpWM...Done."))

(defun run-commands (&rest commands)
  "Run each stumpwm command in sequence. This could be used if
you're used to ratpoison's rc file and you just want to run
commands or don't know lisp very well."
  (loop for i in commands do
       (interactive-command i)))

(define-stumpwm-command "snext" ()
  (switch-to-screen (next-screen))
  (show-frame-indicator (current-group) t))

(define-stumpwm-command "sprev" ()
  (switch-to-screen (next-screen (reverse (sort-screens))))
  (show-frame-indicator (current-group) t))

(define-stumpwm-command "sother" ()
  (switch-to-screen (cadr *screen-list*))
  (show-frame-indicator (current-group) t))

(defun window-send-string (window string)
  "Send the string of characters to the window as if they'd been typed."
  (when window
    (map nil (lambda (ch) 
	       ;; exploit the fact that keysyms for ascii characters
	       ;; are the same as their ascii value.
	       (let ((sym (cond ((<= 32 (char-code ch) 127)
				 (char-code ch))
				((char= ch #\Tab)
				 (stumpwm-name->keysym "TAB"))
				((char= ch #\Newline)
				 (stumpwm-name->keysym "RET"))
				(t nil))))
		 (when sym
		   (send-fake-key window
				  (make-key :keysym sym)))))
	 string)))

(define-stumpwm-command "insert" ((string :rest "Insert: "))
  (window-send-string (current-window) string))

(define-stumpwm-command "putsel" ((string :rest "Text: "))
  (set-x-selection string))

;; FIXME: this function is basically useless atm.
(define-stumpwm-command "getsel" ()
  (message "~a" (get-x-selection)))

(defun other-hidden-window (group)
  "Return the last window that was accessed and that is hidden."
  (let ((wins (remove-if (lambda (w) (eq (frame-window (window-frame w)) w)) (group-windows group))))
    (first wins)))

(defun pull-other-hidden-window (group)
  "pull the last accessed hidden window from any frame into the
current frame and raise it."
  (let ((win (other-hidden-window group)))
    (if win
	(pull-window win)
	(echo-string (group-screen group) "No other window."))))

(defun other-window-in-frame (group)
  (let* ((f (tile-group-current-frame group))
	 (wins (frame-windows group f))
	 (win (if (frame-window f)
		  (second wins)
		  (first wins))))
    (if win
	(frame-raise-window group (window-frame win) win)
	(echo-string (group-screen group) "No other window."))))  

(define-stumpwm-command "pull-hidden-next" ()
  (let ((group (current-group)))
    (focus-forward group (sort-windows group) t (lambda (w) (not (eq (frame-window (window-frame w)) w))))))

(define-stumpwm-command "pull-hidden-previous" ()
  (let ((group (current-group)))
    (focus-forward group (nreverse (sort-windows group)) t (lambda (w) (not (eq (frame-window (window-frame w)) w))))))

(define-stumpwm-command "pull-hidden-other" ()
  (let ((group (current-group)))
    (pull-other-hidden-window group)))

(define-stumpwm-command "next-in-frame" ()
  (let ((group (current-group)))
    (if (group-current-window group)
	(focus-forward group (frame-sort-windows group (tile-group-current-frame group)))
	(other-window-in-frame group))))

(define-stumpwm-command "prev-in-frame" ()
  (let ((group (current-group)))
    (if (group-current-window group)
	(focus-forward group (reverse (frame-sort-windows group (tile-group-current-frame group))))
	(other-window-in-frame group))))

(define-stumpwm-command "other-in-frame" ()
  (other-window-in-frame (current-group)))

(define-stumpwm-command "command-mode" ()
  (message "Press C-g to exit command-mode.")
  (push-top-map *root-map*))

(define-stumpwm-command "mark" ()
  (let ((win (current-window)))
    (when win
      (setf (window-marked win) (not (window-marked win)))
      (message (if (window-marked win)
                   "Marked!"
                   "Unmarked!")))))

(define-stumpwm-command "clear-marks" ()
  (let ((group (current-group)))
    (clear-window-marks group)))

(define-stumpwm-command "pull-marked" ()
  (let ((group (current-group)))
    (dolist (i (marked-windows group))
      (pull-window i))
    (clear-window-marks group)))

(define-stumpwm-command "balance-frames" ()
  (let ((tree (tree-parent (tile-group-frame-tree (current-group))
                           (tile-group-current-frame (current-group)))))
    (if tree
        (balance-frames (current-group) tree)
        (message "There's only 1 frame!"))))

(define-stumpwm-command "describe-key" ((keys :key-seq "Describe Key: "))
  (let ((cmd (lookup-key-sequence *top-map* keys)))
    (if cmd
        (message "~{~a~^ ~} is bound to \"~a\"." (mapcar 'print-key keys)  cmd)
        (message "~{~a~^ ~} is not bound." (mapcar 'print-key keys)))))

(define-stumpwm-command "describe-variable" ((var :variable "Describe Variable: "))
  (message-no-timeout "~a"
                      (with-output-to-string (s)
                        (describe var s))))

(define-stumpwm-command "describe-function" ((fn :function "Describe Function: "))
  (message-no-timeout "~a"
                      (with-output-to-string (s)
                        (describe fn s))))

(define-stumpwm-command "where-is" ((cmd :rest "Where is command: "))
  (message-no-timeout "\"~a\" is on ~{~a~^, ~}"
                      cmd
                      (mapcar 'print-key-seq (search-kmap cmd *top-map*))))

(define-stumpwm-command "syncplace" ()
  (sync-window-placement))


;;; Layout save and restore routines.

(defun frame-dumper (f)
  (format nil "#S(frame :number ~d :x ~d :y ~d :width ~d :height ~d)"
	  (frame-number f)
	  (frame-x f)
	  (frame-y f)
	  (frame-width f)
	  (frame-height f)))

(defun frame-dump (tree)
  (labels ((copy (tree)
		 (cond ((null tree) tree)
		       ((typep tree 'frame)
			(frame-dumper tree))
		       (t
			(mapcar 'frame-dump tree)))))
	  (copy tree)))

(defun dump-to-file (foo name)
  (with-open-file (fp name :direction :output :if-exists :supersede)
		  (with-standard-io-syntax
		   (let ((*package* (find-package :stumpwm))
			 (*print-pretty* t))
		     (princ foo fp)))))

(define-stumpwm-command "fdump" ((file :rest))
  "Dumps the frames of the current group of the current screen to the named file."
  (dump-to-file (frame-dump (tile-group-frame-tree (current-group))) file)
  (message "Frames dumped"))

(defun dump-all-groups-in-screen (screen)
  (mapcar (lambda (group) 
	    (list (format nil "\"~a\"" (group-name group)) (frame-dump (tile-group-frame-tree group))))
	  (sort (copy-list (screen-groups screen)) '< :key 'group-number)))

(define-stumpwm-command "afdump" ((file :rest))
  "Dumps the frames of all groups of the current screen to the named file"
  (dump-to-file (dump-all-groups-in-screen (current-screen)) file)
  (message "Frames dumped"))

(define-stumpwm-command "asfdump" ((file :rest))
  "Dumps the frames of all groups of all screens to the named file"
  (dump-to-file
   (mapcar (lambda (screen)
	     (list (screen-id screen) (dump-all-groups-in-screen screen)))
	   *screen-list*)
   file)
  (message "Frames dumped"))

(defun restore-from-file (file)
  (with-open-file (fp file :direction :input)
		  (with-standard-io-syntax
		   (let ((*package* (find-package :stumpwm)))
		     (read fp)))))

(defun populate-frames (group)
  "Try to fill empty frames in GROUP with hidden windows"
  (dolist (f (group-frames group))
    (unless (frame-window f)
      (choose-new-frame-window f group)
      (when (frame-window f)
	(unhide-window (frame-window f))))))

(defun frame-restore (group frames)
  (labels 
   ((tree-frames (tree)
		 (tree-accum-fn tree 'nconc 'list))
    (get-frame-by-number (tree n)
			 (find n
			       (tree-frames tree)
			       :key (lambda (f)
				      (frame-number f))
			       :test '=)))
   (let ((default (get-frame-by-number frames 0))
	 (focused-window (group-current-window group)))
     ;; Do a frame-number-wise mapping of windows before and
     ;; after the restore.
     (dolist (w (group-windows group))
       (setf (window-frame w)
	     (or
	      (dolist (f2 (tree-frames frames))
		(when (= (frame-number f2) (frame-number (window-frame w)))
		  (when (eq (frame-window (window-frame w)) w)
		    (setf (frame-window f2) w))
		  (return f2)))
	      default))
       (message "Assigning window ~d to frame ~d of group ~a" (window-number w)
		(frame-number (window-frame w)) (group-name group)))
     (setf (tile-group-current-frame group) default
	   (tile-group-last-frame group) default)
     ;; FIXME: call new-window-prefered-frame hook?
     ;; restore the tree
     (setf (tile-group-frame-tree group) frames)
     ;; ensure that the same window which had focus before the restore gets it afterwards.
     (when focused-window
       (setf (tile-group-current-frame group) (window-frame focused-window))
       (focus-window focused-window))))
  (populate-frames group)
  ;; update windows
  (sync-all-frame-windows group))

(define-stumpwm-command "frestore" ((file :rest))
  "Restores frames in current group of current screen from named file."
  (frame-restore (current-group) (restore-from-file file))
  (message "Frames restored"))

(defun restore-all-groups-in-screen (screen group_list)
  "Restore all frames in all groups of given screen. Create groups if
 they don't already exist."
  (dolist (gdesc group_list)
    (destructuring-bind (group-name frames) gdesc
			(frame-restore (or	(find-group screen group-name)
						(add-group screen group-name)) frames))))

(define-stumpwm-command "afrestore" ((file :rest))
  "Restores frames in all groups of current screen from named file."
  (restore-all-groups-in-screen (current-screen) (restore-from-file file))
  (message "Frames restored"))

(defun screen-by-id (id)
  (find id
	*screen-list*
	:key (lambda (s)
	       (screen-id s))
	:test '=))

(define-stumpwm-command "asfrestore" ((file :rest))
  "Restores frames in all groups of all screens from named file."
  (dolist (screen (restore-from-file file))
    (restore-all-groups-in-screen (screen-by-id (first screen)) (second screen))
    (message "Frames restored")))

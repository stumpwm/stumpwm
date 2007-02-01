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
	  (define-key m (kbd "n") "next")
	  (define-key m (kbd "C-n") "next")
	  (define-key m (kbd "M-n") "pull-hidden-next")
	  (define-key m (kbd "C-M-n") "next-in-frame")
	  (define-key m (kbd "SPC") "next")
	  (define-key m (kbd "C-SPC") "next")
	  (define-key m (kbd "p") "prev")
	  (define-key m (kbd "C-p") "prev")
	  (define-key m (kbd "M-p") "pull-hidden-previous")
	  (define-key m (kbd "C-M-p") "prev-in-frame")
	  (define-key m (kbd "w") "windows")
	  (define-key m (kbd "C-w") "windows")
	  (define-key m (kbd "k") "delete")
	  (define-key m (kbd "C-k") "delete")
	  (define-key m (kbd "K") "kill")
	  (define-key m (kbd "b") "banish")
	  (define-key m (kbd "C-b") "banish")
	  (define-key m (kbd "a") "time")
	  (define-key m (kbd "C-a") "time")
	  (define-key m (kbd "'") "select")
	  (define-key m (kbd "\"") "windowlist")
	  (define-key m (kbd "C-t") "other-in-frame")
	  (define-key m (kbd "M-t") "pull-hidden-other")
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
	  (define-key m (kbd "?") "help")
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

(defun focus-next-window (group)
  (focus-forward group (sort-windows group)))

(defun focus-prev-window (group)
  (focus-forward group
		 (reverse
		  (sort-windows group))))

(define-stumpwm-command "next" ()
  (let ((group (screen-current-group (current-screen))))
    (if (group-current-window group)
	(focus-next-window group)
	(other-window group))))

(define-stumpwm-command "prev" ()
  (let ((group (screen-current-group (current-screen))))
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
	 nw)
    ;;(assert wins)
    (setf nw (if (null wins)
		 ;; If the last window in the list is focused, then
		 ;; focus the first one.
		 (car (remove-if-not predicate window-list))
	       ;; Otherwise, focus the next one in the list.
	       (first wins)))
    (if nw
	(if pull-p
	    (pull-window nw)
	    (progn
	      (frame-raise-window group (window-frame nw) nw)
	      (unless (eq (window-frame nw) old-frame)
		(show-frame-indicator group))))
	(echo-string (current-screen) "No other window."))))

(defun delete-current-window ()
  "Send a delete event to the current window."
  (let ((group (screen-current-group (current-screen))))
    (when (group-current-window group)
      (delete-window (group-current-window group)))))

(define-stumpwm-command "delete" ()
  (delete-current-window))

(defun kill-current-window ()
  "Kill the client of the current window."
  (let ((group (screen-current-group (current-screen))))
    (when (group-current-window group)
      (xwin-kill (window-xwin (group-current-window group))))))

(define-stumpwm-command "kill" ()
  (kill-current-window))

(defun banish-pointer ()
  "Move the pointer to the lower right corner of the screen"
  (let ((group (screen-current-group (current-screen))))
    (warp-pointer (group-screen group)
		  (1- (screen-width (current-screen)))
		  (1- (screen-height (current-screen))))))

(define-stumpwm-command "banish" ()
  (banish-pointer))

(define-stumpwm-command "ratwarp" ((x :number "X: ") (y :number "Y: "))
  (warp-pointer (current-screen) x y))

(define-stumpwm-command "ratrelwarp" ((dx :number "Delta X: ") (dy :number "Delta Y: "))
  (warp-pointer-relative dx dy))

;; FIXME: This function doesn't work.
(define-stumpwm-command "ratclick" ((button :number))
  (when (screen-current-window (current-screen))
    (send-fake-click (screen-current-window (current-screen)) (or button 1))))

(defun echo-windows (group fmt)
  "Print a list of the windows to the screen."
  (let* ((wins (sort-windows group))
	 (highlight (position (group-current-window group) wins))
	 (names (mapcar (lambda (w)
			  (format-expand *window-formatters* fmt w)) wins)))
    (if (null wins)
	(echo-string (group-screen group) "No Managed Windows")
      (echo-string-list (group-screen group) names highlight))))

(defun fmt-window-list (group)
  "Using *window-format*, return a 1 line list of the windows, space seperated."
  (format nil "~{~a~^ ~}" 
	  (mapcar (lambda (w)
		    (format-expand *window-formatters* *window-format* w)) (sort-windows group))))

(defun fmt-group-list (group)
  "Given a group list all the groups in the group's screen."
  (format nil "~{~a~^ ~}" 
	  (mapcar (lambda (w)
		    (format-expand *group-formatters* *group-format* w)) (sort-groups (group-screen group)))))

(define-stumpwm-command "windows" ((fmt :rest))
  (echo-windows (screen-current-group (current-screen)) (or fmt *window-format*)))

(defun format-time-string (&optional time)
  "Return a formatted date-time string. FIXME: how about being able to pass a format string in?"
  (let* ((month-names
	  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	 (day-names
	  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
    (multiple-value-bind (sec min hour dom mon year dow)
	(or time (get-decoded-time))
      (format nil "~A ~A ~A ~A:~2,,,'0@A:~2,,,'0@A ~A"
	      (aref day-names dow)
	      (aref month-names (- mon 1))
	      dom hour min sec year))))

(defun echo-date ()
  "Print the output of the 'date' command to the screen."
    (echo-string (current-screen) (format-time-string)))

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
  (select-window (screen-current-group (current-screen)) win))

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
  (let* ((group (screen-current-group (current-screen)))
	 (old-frame (tile-group-current-frame group)))
    (other-window (screen-current-group (current-screen)))
    (unless (eq old-frame (tile-group-current-frame group))
      (show-frame-indicator group))))

(defun programs-in-path (base &optional full-path (path (split-string (getenv "PATH") ":")))
  "Return a list of programs in the path that start with BASE. if
FULL-PATH is T then return the full path, otherwise just return
the filename."
  (loop
     for p in path
     for dir = (probe-file p)
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

(defun horiz-split-frame (group)
  (split-frame group (lambda (f) (split-frame-h group f)))
  (let ((f (tile-group-current-frame group)))
    (when (frame-window f)
      (update-window-border (frame-window f))))
  (show-frame-indicator group))

(define-stumpwm-command "hsplit" ()
  (horiz-split-frame (screen-current-group (current-screen))))

(defun vert-split-frame (group)
  (split-frame group (lambda (f) (split-frame-v group f)))
  (let ((f (tile-group-current-frame group)))
    (when (frame-window f)
      (update-window-border (frame-window f))))
  (show-frame-indicator group))

(define-stumpwm-command "vsplit" ()
  (vert-split-frame (screen-current-group (current-screen))))

(defun remove-split (group)
  (let* ((s (sibling (tile-group-frame-tree group)
		    (tile-group-current-frame group)))
	 ;; grab a leaf of the sibling. The sibling doesn't have to be
	 ;; a frame.
	 (l (tree-accum-fn s
                           (lambda (x y)
                             (declare (ignore y))
                             x)
                           #'identity)))
    ;; Only remove the current frame if it has a sibling
    (dformat 3 "~S~%" s)
    (when s
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
      (show-frame-indicator group))))

(define-stumpwm-command "remove" ()
  (remove-split (screen-current-group (current-screen))))

(define-stumpwm-command "only" ()
  (let* ((screen (current-screen))
	 (group (screen-current-group screen))
	 (frame (make-initial-frame (screen-x screen) (screen-y screen)
				    (screen-width screen) (screen-height screen)))
	 (win (frame-window (tile-group-current-frame group))))
    (mapc (lambda (w)
	    ;; windows in other frames disappear
	    (unless (eq (window-frame w) (tile-group-current-frame group))
	      (hide-window w))
	    (setf (window-frame w) frame))
	  (group-windows group))
    (setf (frame-window frame) win
	  (tile-group-frame-tree group) frame)
    (focus-frame group frame)
    (when (frame-window frame)
      (update-window-border (frame-window frame)))
    (sync-frame-windows group (tile-group-current-frame group))))

(define-stumpwm-command "curframe" ()
  (show-frame-indicator (screen-current-group (current-screen))))

(defun focus-frame-sibling (group)
  (let* ((sib (sibling (tile-group-frame-tree group)
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
  (focus-next-frame (screen-current-group (current-screen))))

(define-stumpwm-command "sibling" ()
  (focus-frame-sibling (screen-current-group (current-screen))))

(define-stumpwm-command "fother" ()
  (focus-last-frame (screen-current-group (current-screen))))

(defun choose-frame-by-number (group)
  "show a number in the corner of each frame and wait for the user to
select one. Returns the selected frame or nil if aborted."
  (let* ((wins (progn
		 (draw-frame-outlines group)
		 (draw-frame-numbers group)))
	 (ch (read-one-char (group-screen group)))
	 (num (read-from-string (string ch))))
    (dformat 3 "read ~S ~S~%" ch num)
    (mapc #'xlib:destroy-window wins)
    (clear-frame-outlines group)
    (find ch (group-frames group)
	  :test 'char=
	  :key 'get-frame-number-translation)))


(define-stumpwm-command "fselect" ((f :frame t))
  (let ((group (screen-current-group (current-screen))))
    (focus-frame group f)
    (show-frame-indicator group)))

(define-stumpwm-command "resize" ((w :number "+ Width: ")
				  (h :number "+ Height: "))
  (let* ((group (screen-current-group (current-screen)))
	 (f (tile-group-current-frame group)))
    (resize-frame group f w 'width)
    (resize-frame group f h 'height)))

(defun eval-line (screen cmd)
  (echo-string screen
	       (handler-case (prin1-to-string (eval (read-from-string cmd)))
		 (error (c)
		   (format nil "~A" c)))))

(define-stumpwm-command "eval" ((cmd :rest "Eval: "))
  (eval-line (current-screen) cmd))

(define-stumpwm-command "echo" ((s :rest "Echo: "))
  (echo-string (current-screen) s))

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
      (throw 'error "Abort.")))

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
      (throw 'error "Abort.")))

(defmacro define-stumpwm-type (type (input prompt) &body body)
  `(setf (gethash ,type *command-type-hash*) 
	 (lambda (,input ,prompt)
	   ,@body)))

(define-stumpwm-type :window-number (input prompt)
  (let ((n (or (argument-pop input)
	       (completing-read (current-screen)
				prompt
				(mapcar 'prin1-to-string
					(mapcar 'window-number 
						(group-windows (screen-current-group (current-screen)))))))))
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
			       (group-windows (screen-current-group (current-screen)))))))

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
	(or (find arg (group-frames (screen-current-group (current-screen)))
		  :key (lambda (f)
			 (string (get-frame-number-translation f)))
		  :test 'string=)
	    (throw 'error "Frame not found."))
	(or (choose-frame-by-number (screen-current-group (current-screen)))
	    (throw 'error "Abort.")))))

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
				     (throw 'error "Abort.")))))
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
    (when (screen-mode-line (current-screen))
      (redraw-mode-line-for (screen-mode-line (current-screen)) (current-screen)))
    (when (stringp result)
      (echo-string (current-screen) result))))

(define-stumpwm-command "colon" ((initial-input :rest))
  (let ((cmd (completing-read (current-screen) ": " (all-commands) (or initial-input ""))))
    (unless cmd
      (throw 'error "Abort."))
    (when (plusp (length cmd))
      (interactive-command cmd))))

(defun pull-window-by-number (group n)
  "Pull window N from another frame into the current frame and focus it."
  (let ((win (find n (group-windows group) :key 'window-number :test '=)))
    (when win
      (pull-window win))))

(define-stumpwm-command "pull" ((n :window-number "Pull: "))
  (pull-window-by-number (screen-current-group (current-screen)) n))

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
  (renumber (screen-current-group (current-screen)) n))

(define-stumpwm-command "reload" ()
  (echo-string (current-screen) "Reloading StumpWM...")
  (asdf:operate 'asdf:load-op :stumpwm)
  (echo-string (current-screen) "Reloading StumpWM...Done."))

(define-stumpwm-command "loadrc" ()
  (multiple-value-bind (success err rc) (load-rc-file)
    (echo-string (current-screen)
		 (if success
		     "RC File loaded successfully."
		     (format nil "Error loading ~A: ~A" rc err)))))

(defun display-keybinding (kmap)
  (echo-string-list (current-screen) (mapcar-hash #'(lambda (k v) (format nil "~A -> ~A" (print-key k) v)) kmap)))

(define-stumpwm-command "help" ()
  (display-keybinding *root-map*))

;; Trivial function
(define-stumpwm-command "abort" ()
  ;; This way you can exit from command mode
  (when (pop-top-map)
    (echo-string (current-screen) "Exited.")))

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
  (clear-frame (tile-group-current-frame (screen-current-group (current-screen))) (screen-current-group (current-screen))))

(defun find-closest-frame (ref-frame framelist closeness-func lower-bound-func
			   upper-bound-func)
  (loop for f in framelist
     with r = nil
     do (when (and
	       ;; Frame is on the side that we want.
	       (<= 0 (funcall closeness-func f))
	       ;; Frame is within the bounds set by the reference frame.
	       (or (<= (funcall lower-bound-func ref-frame)
		       (funcall lower-bound-func f)
		       (funcall upper-bound-func ref-frame))
		   (<= (funcall lower-bound-func ref-frame)
		       (funcall upper-bound-func f)
		       (funcall upper-bound-func ref-frame))
		   (<= (funcall lower-bound-func f)
		       (funcall lower-bound-func ref-frame)
		       (funcall upper-bound-func f)))
	       ;; Frame is closer to the reference and the origin than the
	       ;; previous match
	       (or (null r)
		   (< (funcall closeness-func f) (funcall closeness-func r))
		   (and (= (funcall closeness-func f) (funcall closeness-func r))
			(< (funcall lower-bound-func f) (funcall lower-bound-func r)))))
	  (setf r f))
     finally (return r)))

(define-stumpwm-command "move-focus" ((dir :string "Direction: "))
  (let ((group (screen-current-group (current-screen))))
    (destructuring-bind (perp-coord perp-span parall-coord parall-span)
	(cond
	  ((or (string= dir "left") (string= dir "right"))
	   '(frame-y frame-height frame-x frame-width))
	  ((or (string= dir "up") (string= dir "down"))
	   '(frame-x frame-width frame-y frame-height))
	  (t
	   (error "Valid directions: up, down, left, right")))
      (when perp-coord
	(let ((new-frame (find-closest-frame
			  (tile-group-current-frame group)
			  (group-frames group)
			  (if (or (string= dir "left") (string= dir "up"))
			      (lambda (f)
				(- (funcall parall-coord (tile-group-current-frame group))
				   (funcall parall-coord f) (funcall parall-span f)))
			      (lambda (f)
				(- (funcall parall-coord f)
				   (funcall parall-coord (tile-group-current-frame group))
				   (funcall parall-span (tile-group-current-frame group)))))
			  perp-coord
			  (lambda (f)
			    (+ (funcall perp-coord f) (funcall perp-span
							       f))))))
	  (when new-frame
	    (focus-frame group new-frame))
	  (show-frame-indicator group))))))

(defun run-or-raise (cmd &key class instance title (all-groups *run-or-raise-all-groups*))
  "If any of class, title, or instance are set and a matching window can
be found, select it.  Otherwise simply run cmd."
  (labels ((win-app-info (win)
	     (list (window-class win)
		   (window-res win)
		   (window-name win)))
	   ;; Raise the window win and select its frame.  For now, it
	   ;; does not select the screen.
	   (goto-win (win)
	     (let* ((group (window-group win))
		    (frame (window-frame win))
		    (old-frame (tile-group-current-frame group)))
	       (switch-to-group group)
	       (frame-raise-window group frame win)
	       (focus-frame group frame)
	       (unless (eq frame old-frame)
		 (show-frame-indicator group))))
	   ;; Compare two lists of strings representing window
	   ;; attributes.  If an element is nil it matches anything.
	   ;; Doesn't handle lists of different lengths: extra
	   ;; elements in one list will be ignored.
	   (app-info-cmp (match1 match2)
	     (or (not match1)
		 (not match2)
		 (let ((a (car match1))
		       (b (car match2)))
		   (and
		    (or (not a)
			(not b)
			(string= a b))
		    (app-info-cmp (cdr match1) (cdr match2))))))
	   (find-window (group)
	     (find (list class instance title)
		   (group-windows group)
		   :key #'win-app-info
		   :test #'app-info-cmp)))
    (let ((win
	   ;; If no qualifiers are set don't bother looking for a match.
	   (and (or class instance title)
		;; search all groups
		(if all-groups
		    (loop
		       for g in (screen-groups (current-screen))
		       for win = (find-window g)
		       when win
		       return win)
		    (find-window (screen-current-group (current-screen)))))))
      (if win
	  (goto-win win)
	  (run-shell-command cmd)))))

(define-stumpwm-command "shell" ()
  (run-or-raise "xterm -title '*shell*'" :title "*shell*"))

(define-stumpwm-command "web" ()
  (run-or-raise "firefox" :class "Firefox-bin"))

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
  (echo-nth-last-message (current-screen) *lastmsg-nth*))

;;; A resize minor mode. Something a bit better should probably be
;;; written. But it's an interesting way of doing it.

(defvar *resize-map* nil
  "The keymap used for resizing a window")

(defvar *resize-backup* nil)

(when (null *resize-map*)
  (setf *resize-map*
	(let ((m (make-sparse-keymap)))
	  (define-key m (kbd "Up") "resize 0 -10")
	  (define-key m (kbd "C-p") "resize 0 -10")
	  (define-key m (kbd "p") "resize 0 -10")
	  (define-key m (kbd "k") "resize 0 -10")

	  (define-key m (kbd "Down") "resize 0 10")
	  (define-key m (kbd "C-n") "resize 0 10")
	  (define-key m (kbd "n") "resize 0 10")
	  (define-key m (kbd "j") "resize 0 10")

	  (define-key m (kbd "Left") "resize -10 0")
	  (define-key m (kbd "C-b") "resize -10 0")
	  (define-key m (kbd "b") "resize -10 0")
	  (define-key m (kbd "h") "resize -10 0")

	  (define-key m (kbd "Right") "resize 10 0")
	  (define-key m (kbd "C-f") "resize 10 0")
	  (define-key m (kbd "f") "resize 10 0")
	  (define-key m (kbd "l") "resize 10 0")
	  (define-key m (kbd "RET") "exit-iresize")
	  (define-key m (kbd "C-g") "abort-iresize")
	  (define-key m (kbd "ESC") "abort-iresize")
	  m)))

(define-stumpwm-command "iresize" ()
  (if (atom (tile-group-frame-tree (screen-current-group (current-screen))))
      (echo-string (current-screen) "There's only 1 frame!")
      (progn
	(echo-string (current-screen) "Resize Frame")
	(push-top-map *resize-map*))
      ;;   (setf *resize-backup* (copy-frame-tree screen))
      ))

(define-stumpwm-command "abort-iresize" ()
  (echo-string (current-screen) "Abort resize")
  ;; TODO: actually revert the frames
  (pop-top-map))

(define-stumpwm-command "exit-iresize" ()
 (echo-string (current-screen) "Resize Complete")
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
  (switch-to-group (add-group (current-screen) name)))

(define-stumpwm-command "gnewbg" ((name :string "Group Name: "))
  (add-group (current-screen) name))

(define-stumpwm-command "gnext" ()
  (group-forward (screen-current-group (current-screen))
		 (sort-groups (current-screen))))

(define-stumpwm-command "gprev" ()
  (group-forward (screen-current-group (current-screen))
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
				groups))))
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
	     (screen-current-window (current-screen)))
    (move-window-to-group (screen-current-window (current-screen)) to-group)))

(define-stumpwm-command "gkill" ()
  (let ((dead-group (screen-current-group (current-screen)))
	(to-group (next-group (screen-current-group (current-screen)))))
    (switch-to-group to-group)
    (kill-group dead-group to-group)))

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

(defun select-from-menu (screen table prompt &optional (initial-selection 0))
  "Prompt the user to select from a menu on SCREEN. TABLE can be
a list of values or an alist. If it's an alist, the CAR of each
element is displayed in the menu. What is displayed as menu items
must be strings. Returns the selected element in TABLE or nil if aborted.

See *menu-map* for menu bindings."
  (check-type table list)
  (check-type prompt string)
  (let ((menu (make-menu-state
	       :table table
	       :prompt prompt
	       :selected initial-selection))
	(*record-last-msg-override* t)
	(*supress-echo-timeout* t))
    (bound-check-menu menu)
    (catch :menu-quit
      (grab-keyboard screen)
      (unwind-protect
	   (loop
	      (echo-string-list screen (mapcar (lambda (elt)
						 (if (listp elt)
						     (first elt)
						     elt))
					       table)
				(menu-state-selected menu))
	      (let ((action (read-from-keymap *menu-map*)))
		(when action
		  (funcall action menu))))
	(ungrab-keyboard)
	(unmap-all-message-windows)))))

(define-stumpwm-command "windowlist" ((fmt :rest))
  (let* ((group (screen-current-group (current-screen)))
	 (window (when (group-windows group)
		   (second (select-from-menu
			    (current-screen)
			    (mapcar (lambda (w)
				      (list (format-expand *window-formatters* (or fmt *window-format*) w) w))
				    (sort-windows group))
			    "")))))
    (if window
      (frame-raise-window group (window-frame window) window)
      (echo-string (group-screen group) "No Managed Windows"))))

(defun run-commands (&rest commands)
  "Run each stumpwm command in sequence. This could be used if
you're used to ratpoison's rc file and you just want to run
commands or don't know lisp very well."
  (loop for i in commands do
       (interactive-command i)))

(define-stumpwm-command "snext" ()
  (switch-to-screen (next-screen))
  (show-frame-indicator (screen-current-group (current-screen)) t))

(define-stumpwm-command "sprev" ()
  (switch-to-screen (next-screen (reverse (sort-screens))))
  (show-frame-indicator (screen-current-group (current-screen)) t))

(define-stumpwm-command "sother" ()
  (switch-to-screen (cadr *screen-list*))
  (show-frame-indicator (screen-current-group (current-screen)) t))

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
  (window-send-string (screen-current-window (current-screen)) string))

(define-stumpwm-command "putsel" ((string :rest "Text: "))
  (set-x-selection string))

;; FIXME: this function is basically useless atm.
(define-stumpwm-command "getsel" ()
  (echo-string (current-screen) (get-x-selection)))

(defun pull-other-hidden-window (group)
  "pull the last accessed hidden window from any frame into the
current frame and raise it."
  (let* ((f (tile-group-current-frame group))
	 (wins (remove-if (lambda (w) (eq (frame-window (window-frame w)) w)) (group-windows group)))
	 (win (first wins)))
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
  (let ((group (screen-current-group (current-screen))))
    (focus-forward group (sort-windows group) t (lambda (w) (not (eq (frame-window (window-frame w)) w))))))

(define-stumpwm-command "pull-hidden-previous" ()
  (let ((group (screen-current-group (current-screen))))
    (focus-forward group (nreverse (sort-windows group)) t (lambda (w) (not (eq (frame-window (window-frame w)) w))))))

(define-stumpwm-command "pull-hidden-other" ()
  (let ((group (screen-current-group (current-screen))))
    (pull-other-hidden-window group)))

(define-stumpwm-command "next-in-frame" ()
  (let ((group (screen-current-group (current-screen))))
    (if (group-current-window group)
	(focus-forward group (frame-sort-windows group (tile-group-current-frame group)))
	(other-window-in-frame group))))

(define-stumpwm-command "prev-in-frame" ()
  (let ((group (screen-current-group (current-screen))))
    (if (group-current-window group)
	(focus-forward group (reverse (frame-sort-windows group (tile-group-current-frame group))))
	(other-window-in-frame group))))

(define-stumpwm-command "other-in-frame" ()
  (other-window-in-frame (screen-current-group (current-screen))))

(define-stumpwm-command "command-mode" ()
  (push-top-map *root-map*))

(define-stumpwm-command "mark" ()
  (let ((win (screen-current-window (current-screen))))
    (when win
      (setf (window-marked win) (not (window-marked win)))
      (echo-string (current-screen)
		   (if (window-marked win)
		       "Marked!"
		       "Unmarked!")))))

(define-stumpwm-command "clear-marks" ()
  (let ((group (screen-current-group (current-screen))))
    (clear-window-marks group)))

(define-stumpwm-command "pull-marked" ()
  (let ((group (screen-current-group (current-screen))))
    (dolist (i (marked-windows group))
      (pull-window i))
    (clear-window-marks group)))

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

(defun set-key-binding (key mod fn)
  "Bind KEY to the function FN."
  (setf (gethash (list key mod) *key-bindings*) fn))

(defun set-default-bindings ()
  "Put the default bindings in the key-bindings hash table."
  (set-key-binding #\n '() 'focus-next-window)
  (set-key-binding #\n '(:control) 'focus-next-window)
  (set-key-binding #\p '() 'focus-prev-window)
  (set-key-binding #\p '(:control) 'focus-prev-window)
  (set-key-binding #\w '() 'echo-windows)
  (set-key-binding #\w '(:control) 'echo-windows)
  (set-key-binding #\k '() 'delete-current-window)
  (set-key-binding #\k '(:control) 'delete-current-window)
  (set-key-binding #\K '() 'kill-current-window)
  (set-key-binding #\b '() 'banish-pointer)
  (set-key-binding #\b '(:control) 'banish-pointer)
  (set-key-binding #\a '() 'echo-date)
  (set-key-binding #\a '(:control) 'echo-date)
  (set-key-binding #\' '() 'select-window)
  (set-key-binding #\t '(:control) 'other-window)
  (set-key-binding #\! '() 'shell-command)
  (set-key-binding #\g '(:control) (lambda (s))) ; abort
  (set-key-binding #\0 '() (lambda (s) (pull-window-by-number s 0)))
  (set-key-binding #\1 '() (lambda (s) (pull-window-by-number s 1)))
  (set-key-binding #\2 '() (lambda (s) (pull-window-by-number s 2)))
  (set-key-binding #\3 '() (lambda (s) (pull-window-by-number s 3)))
  (set-key-binding #\4 '() (lambda (s) (pull-window-by-number s 4)))
  (set-key-binding #\5 '() (lambda (s) (pull-window-by-number s 5)))
  (set-key-binding #\6 '() (lambda (s) (pull-window-by-number s 6)))
  (set-key-binding #\7 '() (lambda (s) (pull-window-by-number s 7)))
  (set-key-binding #\8 '() (lambda (s) (pull-window-by-number s 8)))
  (set-key-binding #\9 '() (lambda (s) (pull-window-by-number s 9)))
  (set-key-binding #\r '() 'remove-split)
  (set-key-binding #\s '() 'horiz-split-frame)
  (set-key-binding #\S '() 'vert-split-frame)
  (set-key-binding #\o '() 'focus-frame-sibling)
  (set-key-binding #\f '() 'focus-frame-by-number)
  (set-key-binding #\t '() 'send-meta-key)
  (set-key-binding #\N '(:control) 'renumber)
  (set-key-binding #\: '() 'eval-line))

(defun focus-next-window (screen)
  (focus-forward screen (frame-sort-windows screen
					    (frame-data screen
							(screen-current-frame screen)))))

(defun focus-prev-window (screen)
  (focus-forward screen
		 (reverse
		  (frame-sort-windows screen
				      (frame-data screen
						  (screen-current-frame screen))))))

;; In the future, this window will raise the window into the current
;; frame.
(defun focus-forward (screen window-list)
 "Set the focus to the next item in window-list from the focused window."
  ;; The window with focus is the "current" window, so find it in the
  ;; list and give that window focus
  (let* ((w (xlib:input-focus *display*))
	 (wins (member w window-list))
	 nw)
    (print w)
    (print window-list)
    (print wins)
    ;;(assert wins)
    (setf nw (if (null (cdr wins))
		 ;; If the last window in the list is focused, then
		 ;; focus the first one.
		 (car window-list)
	       ;; Otherwise, focus the next one in the list.
	       (cadr wins)))
    (when nw
      (frame-raise-window screen (window-frame screen nw) nw))))

(defun delete-current-window (screen)
  "Send a delete event to the current window."
  (when (screen-current-window screen)
    (delete-window (screen-current-window screen))))

(defun kill-current-window (screen)
  "Kill the client of the current window."
  (when (screen-current-window screen)
    (kill-window (screen-current-window screen))))

(defun banish-pointer (screen)
  "Move the pointer to the lower right corner of the screen"
  (warp-pointer screen
		(1- (screen-width screen))
		(1- (screen-height screen))))

(defun echo-windows (screen)
  "Print a list of the windows to the screen."
  (let* ((wins (sort-windows screen))
	 (highlight (position (screen-current-window screen) wins :test #'xlib:window-equal))
	 (names (mapcar (lambda (w)
			  (funcall *window-format-fn* screen w)) wins)))
    (if (null wins)
	(echo-string screen "No Managed Windows")
      (echo-string-list screen names highlight))))

(defun echo-date (screen)
  "Print the output of the 'date' command to the screen."
  (let* ((month-names
	  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	 (day-names
	  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
	 (date-string (multiple-value-bind (sec min hour dom mon year dow)
			 (get-decoded-time)
		       (format nil "~A ~A ~A ~A:~2,,,'0@A:~2,,,'0@A ~A"
			       (aref day-names dow)
			       (aref month-names (- mon 1))
			       dom hour min sec year))))
    (echo-string screen date-string)))


(defun select-window (screen)
  "Read input from the user and go to the selected window."
    (let ((query (read-one-line screen "Select: "))
	  match)
      (labels ((match (win)
		      (let* ((wname (window-name win))
			     (end (min (length wname) (length query))))
			(string-equal wname query :end1 end :end2 end))))
	(unless (null query)
	  (setf match (find-if #'match (screen-mapped-windows screen))))
	(when match
	  (focus-window match)))))

(defun select-window-number (screen num)
  (labels ((match (win)
		  (= (window-number screen win) num)))
    (let ((win (find-if #'match (screen-mapped-windows screen))))
      (when win
	(frame-raise-window screen (window-frame screen win) win)))))

(defun other-window (screen)
  (let* ((f (frame-data screen (screen-current-frame screen)))
	 (wins (frame-windows screen f))
	 (win (second wins)))
  (when win
    (frame-raise-window screen (window-frame screen win) win))))

(defun shell-command (screen)
  (let ((cmd (read-one-line screen "/bin/sh -c ")))
    (unless (null cmd)
      (port:run-prog *shell-program* :args (list "-c" cmd) :wait nil))))

(defun horiz-split-frame (screen)
  (split-frame screen (lambda (f) (split-frame-h screen f))))

(defun vert-split-frame (screen)
  (split-frame screen (lambda (f) (split-frame-v screen f))))

(defun remove-split (screen)
  (let* ((s (sibling (screen-frame-tree screen)
		    (screen-current-frame screen)))
	 ;; grab a leaf from the sibling
	 (l (tree-accum-fn s (lambda (x y) x) (lambda (x) x))))
    ;; Only remove the current frame if it has a sibling
    (pprint s)
    (when s
      (pprint l)
      ;; Move the windows from the removed frame to it's sibling
      (migrate-frame-windows screen (screen-current-frame screen) l)
      ;; If the frame has no window, give it the current window of
      ;; the current frame.
      (unless (frame-window (frame-data screen l))
	(setf (frame-window (frame-data screen l))
	      (frame-window (frame-data screen
					(screen-current-frame screen)))))
      ;; Unsplit
      (setf (screen-frame-tree screen)
	    (remove-frame screen
			  (screen-frame-tree screen)
			  (screen-current-frame screen)))
      ;; update the current frame and sync it's windows
      (setf (screen-current-frame screen) l)
      (sync-frame-windows screen l)
      (frame-raise-window screen l (frame-window (frame-data screen l))))))

(defun focus-frame-sibling (screen)
  (let* ((sib (sibling (screen-frame-tree screen)
		      (screen-current-frame screen))))
    (when sib
      (focus-frame screen (tree-accum-fn sib (lambda (x y) x) (lambda (x) x))))))

(defun focus-frame-by-number (screen)
  (let* ((wins (draw-frame-numbers screen))
	 (ch (read-one-char screen))
	 (num (read-from-string (string ch))))
    (pprint (list 'read ch num))
    (when (and (char>= ch #\0)
	       (char<= ch #\9))
      (when (member num (mapcar-hash (lambda (f) (frame-number f)) (screen-frame-hash screen)))
	(focus-frame screen num)))
    (mapc #'xlib:destroy-window wins)))

(defun eval-line (screen)
  (let ((cmd (read-one-line screen ": ")))
    (unless (null cmd)
      (echo-string screen
		   (handler-case (prin1-to-string (eval (read-from-string cmd)))
                     (error (c)
		       (format nil "~A" c)))))))

(defun pull-window-by-number (screen n)
  "Pull window N from another frame into the current frame and focus it."
  (labels ((match (win)
		  (= (window-number screen win) n)))
    (let ((win (find-if #'match (screen-mapped-windows screen))))
      (when win
	(setf (window-frame screen win) (screen-current-frame screen))
	(sync-frame-windows screen (screen-current-frame screen))
	(frame-raise-window screen (screen-current-frame screen) win)))))

(defun send-meta-key (screen)
  "Send the prefix key"
  (send-fake-key (screen-current-window screen) *prefix-key* *prefix-modifiers*))

(defun renumber (screen)
  "Renumber the current window"
  (let* ((s (read-one-line screen "number: "))
	 (nt (read-from-string s)))
    (if (numberp nt)
      (let ((nf (window-number screen (screen-current-window screen)))
	    (win (find-if #'(lambda (win)
			      (= (window-number screen win) nt))
			  (screen-mapped-windows screen))))
	;; Is it already taken?
	(if win
	    (progn
	      ;; swap the window numbers
	      (setf (window-number screen win) nf)
	      (setf (window-number screen (screen-current-window screen)) nt))
	  ;; Just give the window the number
	  (setf (window-number screen (screen-current-window screen)) nt)))
      (echo-string screen "Number expected"))))

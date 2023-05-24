;; Copyright (C) 2006-2008 Shawn Betts
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
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

(in-package :stumpwm)

(export '(*hidden-window-color*
          *mode-line-highlight-template*
          bar
          bar-zone-color
          format-with-on-click-id))

;;; Settings

(defvar *hidden-window-color* "^5*"
  "Color command for hidden windows when using the
fmt-head-window-list-hidden-windows formatter. To disable coloring
hidden windows, set this to an empty string.")

(defvar *mode-line-highlight-template* "^R~A^r"
  "The string passed to FORMAT to highlight things in the mode line.")

;;; Clickable Text

(defun format-with-on-click-id (string id &rest arguments)
  "Wrap STRING in :on-click and :on-click-end color formatters, using ID as the id
to call when clicked and ARGUMENTS as the arguments to pass to the ID's
function. STRING may not contain the :> color formatter, but may contain any
other color formatters."
  (format nil "^(:on-click ~S ~{~S~^ ~})~A^(:on-click-end)"
          id arguments string))

;;; Utilities

(defun mode-line-current-group (ml)
  (screen-current-group (mode-line-screen ml)))

;;; Formatters

(add-screen-mode-line-formatter #\u 'fmt-urgent-window-list)
(defun fmt-urgent-window-list (ml)
  "Using `*window-format*', return a 1 line list of the urgent windows, space separated."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (w)
                    (format-with-on-click-id
                     (let ((str (format-expand *window-formatters*
                                               *window-format*
                                               w)))
                       (if (eq w (current-window))
                           (fmt-highlight str)
                           str))
                     :ml-on-click-focus-window
                     (window-id w)))
                  (screen-urgent-windows (mode-line-screen ml)))))

(add-screen-mode-line-formatter #\w 'fmt-window-list)
(defun fmt-window-list (ml)
  "Using *window-format*, return a 1 line list of the windows, space separated."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (w)
                    (format-with-on-click-id 
                     (format-expand *window-formatters* *window-format* w)
                     :ml-on-click-focus-window
                     (window-id w)))
                  (sort-windows (mode-line-current-group ml)))))

(add-screen-mode-line-formatter #\g 'fmt-group-list)
(defun fmt-group-list (ml)
  "Given a group list all the groups in the group's screen."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (g)
                    (format-with-on-click-id 
                     (let* ((str (format-expand *group-formatters*
                                                *group-format*
                                                g)))
                       (if (eq g (current-group))
                           (fmt-highlight str)
                           str))
                     :ml-on-click-switch-to-group
                     (group-name g)))
                  (let ((groups (sort-groups (group-screen (mode-line-current-group ml)))))
                    (if *list-hidden-groups*
                        groups
                        (non-hidden-groups groups))))))

(add-screen-mode-line-formatter #\h 'fmt-head)
(defun fmt-head (ml)
  (format nil "~d" (head-number (mode-line-head ml))))

(add-screen-mode-line-formatter #\n 'fmt-group)
(defun fmt-group (ml)
  (format nil "~a" (group-name (mode-line-current-group ml))))

(defun fmt-highlight (s)
  (format nil *mode-line-highlight-template* s))

(add-screen-mode-line-formatter #\W 'fmt-head-window-list)
(defun fmt-head-window-list (ml)
  "Using *window-format*, return a 1 line list of the windows, space seperated."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (w)
                    (format-with-on-click-id 
                     (let ((str (format-expand *window-formatters*
                                               *window-format*
                                               w)))
                       (if (eq w (current-window))
                           (fmt-highlight str)
                           str))
                     :ml-on-click-focus-window
                     (window-id w)))
                  (sort1 (head-windows (mode-line-current-group ml) (mode-line-head ml))
                         #'< :key #'window-number))))

(defun fmt-hidden (s)
  (format nil (concat "^[" *hidden-window-color* "~A^]") s))

(add-screen-mode-line-formatter #\v 'fmt-head-window-list-hidden-windows)
(defun fmt-head-window-list-hidden-windows (ml)
  "Using *window-format*, return a 1 line list of the windows, space
separated. The currently focused window is highlighted with
fmt-highlight. Any non-visible windows are colored the
*hidden-window-color*."
  (let* ((all (head-windows (mode-line-current-group ml) (mode-line-head ml)))
         (non-top (set-difference all (top-windows))))
    (format nil "~{~a~^ ~}"
            (mapcar (lambda (w)
                      (format-with-on-click-id 
                       (let ((str (format-expand *window-formatters*
                                                 *window-format* w)))
                         (cond ((eq w (current-window)) (fmt-highlight str))
                               ((find w non-top) (fmt-hidden str))
                               (t str)))
                       :ml-on-click-focus-window
                       (window-id w)))
                    (sort1 all #'< :key #'window-number)))))

(add-screen-mode-line-formatter #\d 'fmt-modeline-time)
(defun fmt-modeline-time (ml)
  (declare (ignore ml))
  (time-format *time-modeline-string*))

(defun format-minor-modes-for-mode-line (mode-objects)
  (with-output-to-string (s)
    (loop for modes on mode-objects
          for list = (minor-mode-lighter (car modes))
          do (loop for text in list
                   unless (string= text "")
                     do (write-string " " s)
                        (write-string text s)))))

(add-screen-mode-line-formatter #\m 'fmt-minor-modes)
(defun fmt-minor-modes (ml)
  (let ((total-string
          (format-minor-modes-for-mode-line (list-current-mode-objects
                                             :screen (mode-line-screen ml)))))
    (if (string= "" total-string)
        total-string
        (subseq total-string 1))))

(add-screen-mode-line-formatter #\M 'fmt-all-minor-modes)
(defun fmt-all-minor-modes (ml)
  (declare (ignore ml))
  (let ((total-string (format-minor-modes-for-mode-line (list-mode-objects nil))))
    (if (string= "" total-string)
        total-string
        (subseq total-string 1))))

(defvar *bar-med-color* "^B")
(defvar *bar-hi-color* "^B^3*")
(defvar *bar-crit-color* "^B^1*")

(defun bar-zone-color (amount &optional (med 20) (hi 50) (crit 90) reverse)
  "Return a color command based on the magnitude of the argument. If
the limits for the levels aren't specified, they default to sensible
values for a percentage. With reverse, lower numbers are more
critical."
  (flet ((past (n) (funcall (if reverse #'<= #'>=) amount n)))
    (cond ((past crit) *bar-crit-color*)
          ((past hi) *bar-hi-color*)
          ((past med) *bar-med-color*)
          (t ""))))

(defun repeat (n char)
  (make-string n :initial-element char))

(defun bar (percent width full empty)
  "Return a progress bar string of WIDTH characters composed of characters FULL
  and EMPTY at PERCENT complete."
  (let ((chars (truncate (* width percent) 100))
        (color (bar-zone-color percent)))
    (let ((bar (make-string (+ width (length color)) :initial-element full)))
      (replace bar color)
      (fill bar empty :start chars))))

(defun make-string-alternator ()
  "Returns a function that takes two arguments, `STRINGS' and `PERIOD'.
Show each of `STRINGS', alternating at most once every `PERIOD' seconds.
`STRINGS' can either be a string or a callable returning a string."
  (let ((prev-time 0)
        (prev-index 0)
        (prev-string ""))
    (flet ((alternate (strings period)
             (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
               (when (>= (- now prev-time) period)
                 (setf prev-time now)
                 (let ((new-string (elt strings prev-index)))
                   (setf prev-string
                         (etypecase new-string
                           (string new-string)
                           (function (funcall new-string)))))
                 (if (< prev-index (1- (length strings)))
                     (incf prev-index)
                     (setf prev-index 0)))
               prev-string)))
      ;; It'll capture its own lexical bindings of prev-time, prev-index, and prev-string
      ;; to call alternate with
      #'alternate)))

(defmacro alternate (strings period)
  "Show each of STRINGS, alternating at most once every PERIOD seconds."
  (let ((alternate (make-string-alternator)))
    (declare (type function alternate))
    `(funcall ,alternate ,strings ,period)))

(defun make-string-scroller ()
  "Return a function that takes three arguments, `STRING', `WIDTH', and `DELAY'.
Scroll `STRING' within the space of `WIDTH' characters, with a step of `DELAY'."
  (let ((prev-time 0)
        (prev-index 0)
        (prev-dir :forward)
        (prev-string ""))
    (flet ((scroll (string width delay)
             (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
               (when (>= (- now prev-time) delay)
                 (setf prev-time now)
                 (setf prev-string (subseq string prev-index (+ prev-index width)))
                 (case prev-dir
                   (:forward
                    (if (< prev-index (- (length string) width))
                        (incf prev-index)
                        (setf prev-dir :backward)))
                   (:backward
                    (if (> prev-index 0)
                        (decf prev-index)
                        (setf prev-dir :forward)))))
               prev-string)))
      #'scroll)))

(defmacro scroll (string width delay)
  "Scroll STRING within the space of WIDTH characters, with a step of DELAY"
  (let ((scroll (make-string-scroller)))
    (declare (type function scroll))
    `(funcall ,scroll ,string ,width ,delay)))

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
          bar-zone-color))

;;; Settings

(defvar *hidden-window-color* "^5*"
  "Color command for hidden windows when using the
fmt-head-window-list-hidden-windows formatter. To disable coloring
hidden windows, set this to an empty string.")

(defvar *mode-line-highlight-template* "^R~A^r"
  "The string passed to FORMAT to highlight things in the mode line.")

;;; Utilities

(defun mode-line-current-group (ml)
  (screen-current-group (mode-line-screen ml)))

;;; Formatters

(add-screen-mode-line-formatter #\u 'fmt-urgent-window-list)
(defun fmt-urgent-window-list (ml)
  "Using `*window-format*', return a 1 line list of the urgent windows, space seperated."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (w)
                    (let ((str (format-expand *window-formatters* *window-format* w)))
                      (if (eq w (current-window))
                          (fmt-highlight str)
                          str)))
                  (screen-urgent-windows (mode-line-screen ml)))))

(add-screen-mode-line-formatter #\w 'fmt-window-list)
(defun fmt-window-list (ml)
  "Using *window-format*, return a 1 line list of the windows, space seperated."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (w) (format-expand *window-formatters* *window-format* w))
                  (sort-windows (mode-line-current-group ml)))))

(add-screen-mode-line-formatter #\g 'fmt-group-list)
(defun fmt-group-list (ml)
  "Given a group list all the groups in the group's screen."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (w)
                    (let* ((str (format-expand *group-formatters* *group-format* w)))
                      (if (eq w (current-group))
                          (fmt-highlight str)
                          str)))
                  (sort-groups (group-screen (mode-line-current-group ml))))))

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
                    (let ((str (format-expand *window-formatters* *window-format* w)))
                      (if (eq w (current-window))
                          (fmt-highlight str)
                          str)))
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
                      (let ((str (format-expand *window-formatters*
                                                *window-format* w)))
                        (cond ((eq w (current-window)) (fmt-highlight str))
                              ((find w non-top) (fmt-hidden str))
                              (t str))))
                    (sort1 all #'< :key #'window-number)))))

(add-screen-mode-line-formatter #\d 'fmt-modeline-time)
(defun fmt-modeline-time (ml)
  (declare (ignore ml))
  (time-format *time-modeline-string*))

(defvar *bar-med-color* "^B")
(defvar *bar-hi-color* "^B^3*")
(defvar *bar-crit-color* "^B^1*")

(defun bar-zone-color (amount &optional (med 20) (hi 50) (crit 90) reverse)
  "Return a color command based on the magnitude of the argument. If
the limits for the levels aren't specified, they default to sensible
values for a percentage. With reverse, lower numbers are more
critical."
  (labels ((past (n) (funcall (if reverse #'<= #'>=) amount n)))
    (cond ((past crit) *bar-crit-color*)
          ((past hi) *bar-hi-color*)
          ((past med) *bar-med-color*)
          (t ""))))

(defun repeat (n char)
  (make-string n :initial-element char))

(defun bar (percent width full empty)
  "Return a progress bar string of WIDTH characters composed of characters FULL
  and EMPTY at PERCENT complete."
  (let ((chars (truncate (* (/ width 100) percent))))
    (format nil "^[~A~A^]~A" (bar-zone-color percent)
            (repeat chars full)
            (repeat (- width chars) empty))))

(defvar *alt-prev-index* 0)
(defvar *alt-prev-time* 0)

;; TODO: Figure out a way to objectify fmt-alternate and fmt-scroll so that
;; multiple instances can coexist.

(defun alternate (strings period)
  "Show each of STRINGS, alternating at most once every PERIOD seconds."
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    (when (>= (- now *alt-prev-time*) period)
      (setf *alt-prev-time* now)
      (if (< *alt-prev-index* (1- (length strings)))
          (incf *alt-prev-index*)
          (setf *alt-prev-index* 0))))
  (elt strings *alt-prev-index*))

(defvar *scroll-prev-index* 0)
(defvar *scroll-prev-time* 0)
(defvar *scroll-prev-dir* :forward)

(defun scroll (string width delay)
  "Scroll STRING within the space of WIDTH characters, with a step of DELAY"
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    (when (>= (- now *scroll-prev-time*) delay)
      (setf *scroll-prev-time* now)
      (case *scroll-prev-dir*
        (:forward
         (if (< *scroll-prev-index* (- (length string) width))
             (incf *scroll-prev-index*)
             (setf *scroll-prev-dir* :backward)))
        (:backward
         (if (> *scroll-prev-index* 0)
             (decf *scroll-prev-index*)
             (setf *scroll-prev-dir* :forward))))))
  (subseq string *scroll-prev-index* (+ *scroll-prev-index* width)))

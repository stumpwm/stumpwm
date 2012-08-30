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

(export '(*mode-line-background-color*
	  *mode-line-border-color*
	  *mode-line-border-width*
	  *mode-line-foreground-color*
	  *mode-line-pad-x*
	  *mode-line-pad-y*
	  *mode-line-position*
	  *mode-line-timeout*
          *hidden-window-color*
	  *screen-mode-line-format*
	  *screen-mode-line-formatters*
          add-screen-mode-line-formatter
	  enable-mode-line
	  toggle-mode-line
	  bar-zone-color))

(defstruct mode-line
  screen
  head
  window
  format
  position
  contents
  cc
  height
  factor
  (mode :stump))

(defun mode-line-gc (ml)
  (ccontext-gc (mode-line-cc ml)))

(defvar *mode-line-position* :top
  "Specifies where the mode line is displayed. Valid values are :top and :bottom.")

(defvar *mode-line-border-width* 1
  "Specifies how thick the mode line's border will be. Integer value.")

(defvar *mode-line-pad-x* 5
  "Specifies the number of padding pixels between the text and the side of the mode line. Integer value.")

(defvar *mode-line-pad-y* 1
  "The number of padding pixels between the modeline text and the top/bottom of the modeline? Integer value.")

(defvar *mode-line-background-color* "Gray20"
  "The mode line background color.")

(defvar *mode-line-foreground-color* "Gray50"
  "The mode line foreground color.")

(defvar *mode-line-border-color* "Gray30"
  "The mode line border color.")

(defvar *hidden-window-color* "^5*"
  "Color command for hidden windows when using the
fmt-head-window-list-hidden-windows formatter. To disable coloring
hidden windows, set this to an empty string.")

(defvar *screen-mode-line-format* "[^B%n^b] %W"
  "This variable describes what will be displayed on the modeline for each screen.
Turn it on with the function TOGGLE-MODE-LINE or the mode-line command.

It is a list where each element may be a string, a symbol, or a list.

For a symbol its value is used.

For a list of the form (:eval FORM) FORM is evaluated and the
result is used as a mode line element.

If it is a string the string is printed with the following formatting
options:

@table @asis
@item %h
List the number of the head the mode-line belongs to

@item %w
List all windows in the current group windows using @var{*window-format*}

@item %W
List all windows on the current head of the current group using
@var{*window-format*}

@item %g
List the groups using @var{*group-format*}
@end table")

(defvar *screen-mode-line-formatters* '((#\w fmt-window-list)
                                        (#\g fmt-group-list)
                                        (#\h fmt-head)
                                        (#\n fmt-group)
                                        (#\W fmt-head-window-list)
                                        (#\u fmt-urgent-window-list)
                                        (#\v fmt-head-window-list-hidden-windows)
                                        (#\d fmt-modeline-time))
  "An alist containing format character format function pairs for
formatting screen mode-lines. functions are passed the screen's
current group.")

(defvar *current-mode-line-formatters* nil
  "used in formatting modeline strings.")

(defvar *current-mode-line-formatter-args* nil
  "used in formatting modeline strings.")

(defvar *mode-line-timeout* 60
  "The modeline updates after each command, when a new window appears or
an existing one disappears, and on a timer. This variable controls how
many seconds elapse between each update. If this variable is changed
while the modeline is visible, you must toggle the modeline to update
timer.")

(defvar *mode-line-timer* nil
  "The timer that updates the modeline")

;;; Formatters

(defun add-screen-mode-line-formatter (character fmt-fun)
  "Add a format function to a format character (or overwrite an existing one)."
  (setf *screen-mode-line-formatters*
        (cons (list character fmt-fun)
              (remove character *screen-mode-line-formatters* :key #'first))))

;; All mode-line formatters take the mode-line they are being invoked from
;; as the first argument. Additional arguments (everything between the first
;; ',' and the ';' are provided as strings [not yet implemented]).

(defun fmt-urgent-window-list (ml)
  "Using *window-format*, return a 1 line list of the urgent windows, space seperated."
   (format nil "~{~a~^ ~}"
          (mapcar (lambda (w)
                    (let ((str (format-expand *window-formatters* *window-format* w)))
                      (if (eq w (current-window))
                          (fmt-highlight str)
                          str)))
                  (screen-urgent-windows (mode-line-screen ml)))))

(defun fmt-window-list (ml)
   "Using *window-format*, return a 1 line list of the windows, space seperated."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (w) (format-expand *window-formatters* *window-format* w))
                  (sort-windows (mode-line-current-group ml)))))

(defun fmt-group-list (ml)
  "Given a group list all the groups in the group's screen."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (w)
                    (let* ((str (format-expand *group-formatters* *group-format* w)))
                      (if (eq w (current-group))
                          (fmt-highlight str)
                          str)))
                  (sort-groups (group-screen (mode-line-current-group ml))))))

(defun fmt-head (ml)
  (format nil "~d" (head-number (mode-line-head ml))))

(defun fmt-group (ml)
  (format nil "~a" (group-name (mode-line-current-group ml))))

(defun fmt-highlight (s)
  (format nil "^R~A^r" s))

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



(defun make-mode-line-window (parent screen)
  "Create a window suitable for a modeline."
  (xlib:create-window
   :parent parent
   :x 0 :y 0 :width 1 :height 1
   :background (alloc-color screen *mode-line-background-color*)
   :border (alloc-color screen *mode-line-border-color*)
   :border-width *mode-line-border-width*
   ;; You can click the modeline
   :event-mask (xlib:make-event-mask :button-press :exposure)
   ;; these windows are not controlled by the window manager
   :override-redirect :on))

(defun resize-mode-line (ml)
  (when (eq (mode-line-mode ml) :stump)
    ;; This is a StumpWM mode-line
    (setf (xlib:drawable-height (mode-line-window ml)) 
          (+ (* (1+ (count #\Newline (mode-line-contents ml) :test #'equal))
                (font-height (screen-font (current-screen))))
             (* *mode-line-pad-y* 2))))
  (setf (xlib:drawable-width (mode-line-window ml)) (- (frame-width (mode-line-head ml))
                                                       (* 2 (xlib:drawable-border-width (mode-line-window ml))))
        (xlib:drawable-height (mode-line-window ml)) (min (xlib:drawable-height (mode-line-window ml))
                                                          (truncate (head-height (mode-line-head ml)) 4))
        (mode-line-height ml) (+ (xlib:drawable-height (mode-line-window ml))
                                 (* 2 (xlib:drawable-border-width (mode-line-window ml))))
        (mode-line-factor ml) (- 1 (/ (mode-line-height ml)
                                      (head-height (mode-line-head ml))))
        (xlib:drawable-x (mode-line-window ml)) (head-x (mode-line-head ml))
        (xlib:drawable-y (mode-line-window ml)) (if (eq (mode-line-position ml) :top)
                                                    (head-y (mode-line-head ml))
                                                    (- (+ (head-y (mode-line-head ml))
                                                          (head-height (mode-line-head ml)))
                                                       (mode-line-height ml)))))

(defgeneric mode-line-format-elt (elt))

(defmethod mode-line-format-elt ((elt string))
  (apply 'format-expand *current-mode-line-formatters* elt
         *current-mode-line-formatter-args*))

(defmethod mode-line-format-elt ((elt symbol))
  (if (boundp elt)
      (let ((val (symbol-value elt)))
        ;; ignore T and nil, like emacs.
        (unless (or (eq val t)
                    (eq val nil))
          (mode-line-format-elt val)))
      (symbol-name elt)))

(defmethod mode-line-format-elt ((elt null))
  "")

(defmethod mode-line-format-elt ((elt list))
  (etypecase (first elt)
    ((or string list)
     (apply 'concatenate 'string
            (mapcar 'mode-line-format-elt elt)))
    (symbol
     (mode-line-format-elt
      (case (first elt)
        ;; FIXME: silently failing is probably not the best idea.
        (:eval (ignore-errors (eval (second elt))))
        (t (and (boundp (first elt))
                (symbol-value (first elt))
                (second elt))))))))

(defun mode-line-format-string (ml)
  (mode-line-format-elt (mode-line-format ml)))

(defun make-mode-line-gc (window screen)
  (xlib:create-gcontext :drawable window
                        :font (when (typep (screen-font screen) 'xlib:font))
                        :foreground (alloc-color screen *mode-line-foreground-color*)
                        :background (alloc-color screen *mode-line-background-color*)))


(defun update-mode-line-color-context (ml)
  (let* ((cc (mode-line-cc ml))
         (screen (mode-line-screen ml))
         (bright (lookup-color screen *mode-line-foreground-color*)))
    (adjust-color bright 0.25)
    (setf (ccontext-default-bright cc) (alloc-color screen bright))))

(defun make-head-mode-line (screen head format)
  (let* ((w (make-mode-line-window (screen-root screen) screen))
         (gc (make-mode-line-gc w screen)))
    (make-mode-line :window w
                    :screen screen
                    :head head
                    :format format
                    :position *mode-line-position*
                    :cc (make-ccontext :gc gc
                                       :win w
                                       :default-fg (xlib:gcontext-foreground gc)
                                       :default-bg (xlib:gcontext-background gc)))))

(defun mode-line-current-group (ml)
  (screen-current-group (mode-line-screen ml)))

(defun redraw-mode-line (ml &optional force)
  (when (eq (mode-line-mode ml) :stump)
    (let* ((*current-mode-line-formatters* *screen-mode-line-formatters*)
           (*current-mode-line-formatter-args* (list ml))
           (string (mode-line-format-string ml)))
      (when (or force (not (string= (mode-line-contents ml) string)))
        (setf (mode-line-contents ml) string)
        (resize-mode-line ml)
        (render-strings (mode-line-screen ml) (mode-line-cc ml)
                        *mode-line-pad-x*     *mode-line-pad-y*
                        (split-string string (string #\Newline)) '())))))

(defun find-mode-line-window (xwin)
  (dolist (s *screen-list*)
    (dolist (h (screen-heads s))
      (let ((mode-line (head-mode-line h)))
        (when (and mode-line (eq (mode-line-window mode-line) xwin))
          (return-from find-mode-line-window mode-line))))))

(defun sync-mode-line (ml)
  (dolist (group (screen-groups (mode-line-screen ml)))
    (group-sync-head group (mode-line-head ml))))

(defun set-mode-line-window (ml xwin)
  "Use an external window as mode-line."
  (xlib:destroy-window (mode-line-window ml))
  (setf (mode-line-window ml) xwin
        (mode-line-mode ml) :visible
        (xlib:window-priority (mode-line-window ml)) :above)
  (resize-mode-line ml)
  (sync-mode-line ml))

(defun destroy-mode-line-window (ml)
  (xlib:destroy-window (mode-line-window ml))
  (setf (head-mode-line (mode-line-head ml)) nil)
  (sync-mode-line ml))

(defun move-mode-line-to-head (ml head)
  (if (head-mode-line head)
      (when (mode-line-head ml)
        ;; head already has a mode-line. Try swapping them.
        (let ((old-head (mode-line-head ml)))
          (setf (mode-line-head ml) head
                (head-mode-line old-head) (head-mode-line head)
                (mode-line-head (head-mode-line head)) old-head
                (head-mode-line head) ml)))
      (progn
        (when (mode-line-head ml)
          (setf (head-mode-line (mode-line-head ml)) nil))
        (setf (head-mode-line head) ml
              (mode-line-head ml) head))))

(defun update-mode-line-position (ml x y)
  (let ((head
         ;; Find the appropriate head
         (find-if (lambda (h) (and (= x (head-x h))
                                   (>= y (head-y h))
                                   (< y (+ (head-y h) (head-height h)))))
                  (screen-heads (mode-line-screen ml)))))
    (when (or (not head)
              (not (eq (head-mode-line head) ml)))
      ;; No luck. Just try to find a head without a mode-line already.
      (setf head (find-if-not #'head-mode-line (screen-heads (mode-line-screen ml)))))
    (if head
        (progn
          (unless (eq ml (head-mode-line head))
            (move-mode-line-to-head ml head))
          (when (mode-line-head ml)
            (setf (mode-line-position ml) (if (< y (/ (head-height (mode-line-head ml)) 2)) :top :bottom))))
        nil)))

(defun place-mode-line-window (screen xwin)
  (let ((ml (make-mode-line :window xwin :screen screen :mode :visible :position *mode-line-position*)))
    (xlib:reparent-window xwin (screen-root screen) 0 0)
    (when (update-mode-line-position ml (xlib:drawable-x xwin) (xlib:drawable-y xwin))
      (resize-mode-line ml)
      (xlib:map-window xwin)
      (sync-mode-line ml))))

(defun update-mode-lines (screen)
  "Update all mode lines on SCREEN"
  (dolist (h (screen-heads screen))
    (let ((mode-line (head-mode-line h)))
      (when mode-line
        (redraw-mode-line mode-line)))))

(defun update-all-mode-lines ()
  "Update all mode lines."
  (mapc 'update-mode-lines *screen-list*))

(defun turn-on-mode-line-timer ()
  (when (timer-p *mode-line-timer*)
    (cancel-timer *mode-line-timer*))
  (setf *mode-line-timer* (run-with-timer *mode-line-timeout*
                                          *mode-line-timeout*
                                          'update-all-mode-lines)))

(defun all-heads ()
  "Return all heads on all screens."
  (loop for s in *screen-list*
        nconc (copy-list (screen-heads s))))

(defun maybe-cancel-mode-line-timer ()
  (unless (find-if 'head-mode-line (all-heads))
    (when (timer-p *mode-line-timer*)
      (cancel-timer *mode-line-timer*)
      (setf *mode-line-timer* nil))))

(defun toggle-mode-line (screen head &optional (format '*screen-mode-line-format*))
  "Toggle the state of the mode line for the specified screen"
  (check-type format (or symbol list string))
  (let ((ml (head-mode-line head)))
    (if ml
        (case (mode-line-mode ml)
          (:visible
           ;; Hide it.
           (setf (mode-line-mode ml) :hidden)
           (xlib:unmap-window (mode-line-window ml)))
          (:hidden
           ;; Show it.
           (setf (mode-line-mode ml) :visible)
           (xlib:map-window (mode-line-window ml)))
          (:stump
           ;; Delete it
           (xlib:destroy-window (mode-line-window ml))
           (xlib:free-gcontext (mode-line-gc ml))
           (setf (head-mode-line head) nil)
           (maybe-cancel-mode-line-timer)))
        (progn
          (setf (head-mode-line head) (make-head-mode-line screen head format))
          (update-mode-line-color-context (head-mode-line head))
          (resize-mode-line (head-mode-line head))
          (xlib:map-window (mode-line-window (head-mode-line head)))
          (redraw-mode-line (head-mode-line head))
          (dformat 3 "modeline: ~s~%" (head-mode-line head))
          ;; setup the timer
          (turn-on-mode-line-timer)))
    (dolist (group (screen-groups screen))
      (group-sync-head group head))))

(defun enable-mode-line (screen head state &optional format)
  "Set the state of SCREEN's HEAD's mode-line. If STATE is T and FORMAT is
  specified, then the mode-line's format is updated."
  (check-type screen screen)
  (check-type head head)
  (check-type format (or symbol list string))
  (if state
      (if (head-mode-line head)
          (when format
            (setf (mode-line-format (head-mode-line head)) format))
          (toggle-mode-line screen head (or format *screen-mode-line-format*)))
      (when (head-mode-line head)
        (toggle-mode-line screen head))))

(defcommand mode-line () ()
  "A command to toggle the mode line visibility."
  (toggle-mode-line (current-screen) (current-head)))

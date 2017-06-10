;; Copyright (C) 2006-2008 Shawn Betts
;; Copyright (C) 2016 Joram Schrijver
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
          *screen-mode-line-format*
          *screen-mode-line-formatters*
          add-screen-mode-line-formatter
          enable-mode-line
          toggle-mode-line))

;;; Settings

(defvar *mode-line-position* :top
  "Specifies where the mode line is displayed. Valid values are :top and :bottom.")

(defvar *mode-line-border-width* 1
  "Specifies how thick the mode line's border will be. Integer value.")

(defvar *mode-line-pad-x* 5
  "Specifies the number of padding pixels between the text and the side of the mode line. Integer value.")

(defvar *mode-line-pad-y* 1
  "The number of padding pixels between the modeline text and the top/bottom of the modeline. Integer value.")

(defvar *mode-line-background-color* "Gray20"
  "The mode line background color.")

(defvar *mode-line-foreground-color* "Gray50"
  "The mode line foreground color.")

(defvar *mode-line-border-color* "Gray30"
  "The mode line border color.")

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

@item %n
The current group's name

@item %u
Using @var{*window-format*}, return a 1 line list of the urgent windows, space seperated.

@item %v
Using @var{*window-format*}, return a 1 line list of the windows, space
separated. The currently focused window is highlighted with
fmt-highlight. Any non-visible windows are colored the
*hidden-window-color*.

@item %d
Using @var{*time-modeline-string*}, print the time.

@end table

A number of modules have been written that extends the possible
formatting strings.  See their documentation for details.")

(defvar *screen-mode-line-formatters* ()
  "An alist containing format character format function pairs for
formatting screen mode-lines. functions are passed the mode line.")

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

;;; Mode lines

(defvar *mode-lines* ()
  "All current mode lines.")

(defstruct (mode-line (:constructor %make-mode-line))
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

;;; Utilities

(defun screen-mode-lines (screen)
  (remove-if (lambda (mode-line)
               (not (eq screen (mode-line-screen mode-line))))
             *mode-lines*))

(defun head-mode-line (head)
  (find head *mode-lines* :key #'mode-line-head))

(defun find-mode-line-by-window (xwin)
  (find xwin *mode-lines* :key #'mode-line-window))

(defun mode-line-gc (ml)
  (ccontext-gc (mode-line-cc ml)))

(defun turn-on-mode-line-timer ()
  (when (timer-p *mode-line-timer*)
    (cancel-timer *mode-line-timer*))
  (setf *mode-line-timer* (run-with-timer *mode-line-timeout*
                                          *mode-line-timeout*
                                          'update-all-mode-lines)))

(defun maybe-cancel-mode-line-timer ()
  (unless *mode-lines*
    (when (timer-p *mode-line-timer*)
      (cancel-timer *mode-line-timer*)
      (setf *mode-line-timer* nil))))

;;; Creation

(defun resize-mode-line (mode-line)
  (when (eq (mode-line-mode mode-line) :stump)
    ;; This is a StumpWM mode-line
    (setf (xlib:drawable-height (mode-line-window mode-line))
          (+ (* 2 *mode-line-pad-y*)
             (nth-value 1 (rendered-size
                           (split-string (mode-line-contents mode-line)
                                         (string #\Newline))
                           (mode-line-cc mode-line))))))
  (with-accessors ((window mode-line-window)
                   (head mode-line-head)
                   (position mode-line-position)
                   (height mode-line-height)
                   (factor mode-line-factor))
      mode-line
    (setf (xlib:drawable-width window) (- (frame-width head)
                                          (* 2 (xlib:drawable-border-width
                                                window)))
          (xlib:drawable-height window) (min (xlib:drawable-height window)
                                             (truncate (head-height head) 4))
          height (+ (xlib:drawable-height window)
                    (* 2 (xlib:drawable-border-width window)))
          factor (- 1 (/ height
                         (head-height head)))
          (xlib:drawable-x window) (head-x head)
          (xlib:drawable-y window) (if (eq position :top)
                                       (head-y head)
                                       (- (+ (head-y head)
                                             (head-height head))
                                          height)))))

(defun update-mode-line-color-context (ml)
  (let* ((cc (mode-line-cc ml))
         (screen (mode-line-screen ml))
         (bright (lookup-color screen *mode-line-foreground-color*)))
    (adjust-color bright 0.25)
    (setf (ccontext-default-bright cc) (alloc-color screen bright))))

(defun make-mode-line-window (screen)
  "Create a window suitable for a modeline."
  (xlib:create-window
   :parent (screen-root screen)
   :x 0 :y 0 :width 1 :height 1
   :background (alloc-color screen *mode-line-background-color*)
   :border (alloc-color screen *mode-line-border-color*)
   :border-width *mode-line-border-width*
   ;; You can click the modeline
   :event-mask (xlib:make-event-mask :button-press :exposure)
   ;; these windows are not controlled by the window manager
   :override-redirect :on))

(defun make-mode-line-gc (window screen)
  (xlib:create-gcontext
   :drawable window
   :font (when (typep (screen-font screen) 'xlib:font)
           (screen-font screen))
   :foreground (alloc-color screen *mode-line-foreground-color*)
   :background (alloc-color screen *mode-line-background-color*)))

(defun make-mode-line-cc (window screen gc)
  (make-ccontext :gc gc
                 :screen screen
                 :font (screen-font screen)
                 :win window
                 :default-fg (xlib:gcontext-foreground gc)
                 :default-bg (xlib:gcontext-background gc)))

(defun make-mode-line (screen head format)
  (let* ((window (make-mode-line-window screen))
         (gc (make-mode-line-gc window screen))
         (cc (make-mode-line-cc window screen gc))
         (mode-line (%make-mode-line :window window
                                     :screen screen
                                     :head head
                                     :format format
                                     :position *mode-line-position*
                                     :cc cc)))
    (prog1 mode-line
      (push mode-line *mode-lines*)
      (update-mode-line-color-context mode-line)
      (resize-mode-line mode-line)
      (xlib:map-window window)
      (redraw-mode-line mode-line)
      (dformat 3 "modeline: ~s~%" mode-line)
      (turn-on-mode-line-timer)
      (run-hook-with-args *new-mode-line-hook* mode-line))))

;;; Destruction

(defun sync-mode-line (ml)
  (dolist (group (screen-groups (mode-line-screen ml)))
    (group-sync-head group (mode-line-head ml))))

(defun destroy-mode-line (ml)
  (run-hook-with-args *destroy-mode-line-hook* ml)
  (xlib:destroy-window (mode-line-window ml))
  (xlib:free-gcontext (mode-line-gc ml))
  (setf *mode-lines* (remove ml *mode-lines*))
  (sync-mode-line ml)
  (maybe-cancel-mode-line-timer))

;;; Formatting

(defvar *current-mode-line-formatters* nil
  "used in formatting modeline strings.")

(defvar *current-mode-line-formatter-args* nil
  "used in formatting modeline strings.")

(defgeneric mode-line-format-elt (elt))

(defmethod mode-line-format-elt ((elt string))
  (apply 'format-expand
         *current-mode-line-formatters*
         elt
         *current-mode-line-formatter-args*))

(defmethod mode-line-format-elt ((elt symbol))
  (if (boundp elt)
      (let ((val (symbol-value elt)))
        ;; ignore T and nil, like emacs.
        (unless (typep val 'boolean)
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

(defun redraw-mode-line (ml &optional force)
  (when (eq (mode-line-mode ml) :stump)
    (let* ((*current-mode-line-formatters* *screen-mode-line-formatters*)
           (*current-mode-line-formatter-args* (list ml))
           (string (mode-line-format-string ml)))
      (when (or force (not (string= (mode-line-contents ml) string)))
        (setf (mode-line-contents ml) string)
        (resize-mode-line ml)
        (render-strings (mode-line-cc ml) *mode-line-pad-x* *mode-line-pad-y*
                        (split-string string (string #\Newline)) ())))))

(defun update-mode-lines (screen)
  "Update all mode lines on SCREEN"
  (dolist (mode-line (screen-mode-lines screen))
    (redraw-mode-line mode-line)))

(defun update-all-mode-lines ()
  "Update all mode lines."
  (mapc 'redraw-mode-line *mode-lines*))

;;; External mode lines

(defun move-mode-line-to-head (mode-line head)
  (cond ((not (head-mode-line head))
         (setf (mode-line-head mode-line) head))
        ((mode-line-head mode-line)
         (rotatef (mode-line-head mode-line)
                  (mode-line-head (head-mode-line head))))))

(defun update-mode-line-position (mode-line x y)
  (let ((head (or (find-if (lambda (h) (and (= x (head-x h))
                                            (>= y (head-y h))
                                            (< y (+ (head-y h)
                                                    (head-height h)))))
                           (screen-heads (mode-line-screen mode-line)))
                  ;; No luck. Just try to find a head without a mode-line
                  ;; already.
                  (find-if-not #'head-mode-line
                               (screen-heads (mode-line-screen mode-line))))))
    (when head
      (unless (eq head (mode-line-head mode-line))
        (move-mode-line-to-head mode-line head))
      (when (mode-line-head mode-line)
        (setf (mode-line-position mode-line)
              (if (< y (/ (head-height (mode-line-head mode-line)) 2))
                  :top
                  :bottom))))))

(defun place-mode-line-window (screen xwin)
  (let ((ml (%make-mode-line
             :window xwin
             :screen screen
             :mode :visible
             :position *mode-line-position*)))
    (push ml *mode-lines*)
    (xlib:reparent-window xwin (screen-root screen) 0 0)
    (when (update-mode-line-position ml
                                     (xlib:drawable-x xwin)
                                     (xlib:drawable-y xwin))
      (resize-mode-line ml)
      (xlib:map-window xwin)
      (sync-mode-line ml))))

;;; Toggling

(defun toggle-mode-line (screen head
                         &optional (format '*screen-mode-line-format*))
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
           (destroy-mode-line ml)))
        (make-mode-line screen head format))
    (dolist (group (screen-groups screen))
      (group-sync-head group head))))

(defun enable-mode-line (screen head state &optional format)
  "Set the state of SCREEN's HEAD's mode-line. If STATE is T and FORMAT is
  specified, then the mode-line's format is updated."
  (check-type screen screen)
  (check-type head head)
  (check-type format (or symbol list string))
  (let ((mode-line (head-mode-line head)))
    (cond
      ((and state mode-line)
       (when format
         (setf (mode-line-format mode-line) format)))
      (state
       (toggle-mode-line screen head (or format '*screen-mode-line-format*)))
      (mode-line
       (toggle-mode-line screen head)))))

(defcommand mode-line () ()
  "A command to toggle the mode line visibility."
  (toggle-mode-line (current-screen) (current-head)))

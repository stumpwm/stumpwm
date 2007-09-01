;; Copyright (C) 2006 Shawn Betts
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

(in-package :stumpwm)

(defstruct mode-line
  screen
  head
  window
  format
  position
  gc
  height
  (mode :stump))

(defvar *mode-line-position* :top
  "Where should the mode line be displayed? :top or :bottom.")

(defvar *mode-line-border-width* 1
  "How thick shall the mode line border be?")

(defvar *mode-line-pad-x* 5
  "How much padding should be between the modeline text and the sides")

(defvar *mode-line-pad-y* 1
  "How much padding should be between the modeline text and the top/bottom")

(defvar *mode-line-background-color* "Gray20"
  "mode line background color")

(defvar *mode-line-foreground-color* "Gray50"
  "mode line foreground color")

(defvar *mode-line-border-color* "Gray30"
  "mode line border color")

(defvar *screen-mode-line-format* "%w"
  "A template for displaying mode line for each screen. Turn it
on with the function TOGGLE-MODE-LINE or the mode-line command.

It is a list where each element may be a string, a symbol, or a list.

For a symbol its value is used.

For a list of the form (:eval FORM) FORM is evaluated and the
result is used as a mode line element.

A string is printed verbatim in the mode line except for
%-constructs:

%w -- print the window list
")

(defvar *screen-mode-line-formatters* '((#\w fmt-window-list)
					(#\g fmt-group-list)
					(#\h fmt-head)
					(#\W fmt-head-window-list))
  "An alist containing format character format function pairs for
formatting screen mode-lines. functions are passed the screen's
current group.")

(defvar *current-mode-line-formatters* nil
  "used in formatting modeline strings.")

(defvar *current-mode-line-formatter-args* nil
  "used in formatting modeline strings.")

(defvar *mode-line-timeout* 60
  "The amount of time between modeline updates.")

(defvar *mode-line-timer* nil
  "The timer that updates the modeline")

(defun netwm-remove-systray-window (screen xwin)
  ;; update _KDE_NET_WM_SYSTRAY_WINDOWS
  (let ((windows (xlib:get-property (screen-root screen)
                                        :_KDE_NET_WM_SYSTRAY_WINDOWS
                                        :type :window)))
    (xlib:change-property (screen-root screen)
                          :_KDE_NET_WM_SYSTRAY_WINDOWS
                          (remove (xlib:drawable-id xwin)
                                  windows)
                          :window 32
                          :mode :replace)))

(defun mode-line-add-systray-window (screen xwin)
  (cond
    ((xlib:get-property xwin :_KDE_NET_WM_SYSTEM_TRAY_WINDOW_FOR)
     (xlib:change-property (screen-root screen)
			   :_KDE_NET_WM_SYSTRAY_WINDOWS
			   (list (xlib:drawable-id xwin))
			   :window 32
			   :mode :append)
     t)
    (t nil)))

(defun make-mode-line-window (parent screen)
  "Create a window suitable for a modeline."
  (xlib:create-window
   :parent parent
   :x 0 :y 0 :width 1 :height 1
   :background (get-color-pixel screen *mode-line-background-color*)
   :border (get-color-pixel screen *mode-line-border-color*)
   :border-width *mode-line-border-width*
   ;; You can click the modeline
   :event-mask (xlib:make-event-mask :button-press :exposure)
   ;; these windows are not controlled by the window manager
   :override-redirect :on))

(defun resize-mode-line (ml)
  (when (eq (mode-line-mode ml) :stump)
    ;; This is a StumpWM mode-line
    (setf (xlib:drawable-height (mode-line-window ml)) (+ (font-height (xlib:gcontext-font (mode-line-gc ml))) (* *mode-line-pad-y* 2))))
  (setf (xlib:drawable-width (mode-line-window ml)) (- (frame-width (mode-line-head ml)) (* 2 (xlib:drawable-border-width (mode-line-window ml))))
	(xlib:drawable-height (mode-line-window ml)) (min (xlib:drawable-height (mode-line-window ml)) (/ (head-height (mode-line-head ml)) 4))
	(mode-line-height ml) (xlib:drawable-height (mode-line-window ml))
	(xlib:drawable-x (mode-line-window ml)) (head-x (mode-line-head ml))
	(xlib:drawable-y (mode-line-window ml)) (if (eq (mode-line-position ml) :top)
						  0 
						  (- (head-height (mode-line-head ml)) (mode-line-height ml)))))

(defun frame-display-y (group frame)
  "Return a Y for frame that doesn't overlap the mode-line."
  (let* ((head (frame-head group frame))
	 (ml (head-mode-line head)))
    (if (and ml (not (eq (mode-line-mode ml) :hidden)))
      (progn
	(case (mode-line-position ml)
	  (:top
	    (if (<= (frame-y frame)
		    (+ (head-y head) (mode-line-height ml)))
	      (+ (frame-y frame) (- (+ (head-y head) (mode-line-height ml)) (frame-y frame)))
	      (+ (frame-y frame) (mode-line-height ml))))
	  (:bottom
	    (if (> (+ (frame-y frame) (frame-height frame))
		   (+ (head-y head) (head-height head)) (mode-line-height ml))
	      (- (frame-y frame) (mode-line-height ml))
	      (frame-y frame)))))
      (frame-y frame))))

(defun frame-display-height (group frame)
  "Return a HEIGHT for frame that doesn't overlap the mode-line."
  (let* ((head (frame-head group frame))
	 (ml (head-mode-line head)))
    (if (and ml (not (eq (mode-line-mode ml) :hidden)))
      (progn
	(case (mode-line-position ml)
	  (:top
	    (if (> (+ (frame-y frame) (frame-height frame) (mode-line-height ml))
		   (+ (head-y head) (head-height head)))
	      (- (frame-height frame) (mode-line-height ml))
	      (frame-height frame)))
	  (:bottom
	    (if (> (+ (frame-y frame) (frame-height frame))
		   (- (+ (head-y head) (head-height head)) (mode-line-height ml)))
	      (- (frame-height frame) (mode-line-height ml))
	      (frame-height frame)))))
      (frame-height frame))))

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
			:font (screen-font screen)
			:foreground (get-color-pixel screen *mode-line-foreground-color*)
			:background (get-color-pixel screen *mode-line-background-color*)))

(defun make-head-mode-line (screen head format)
  (let ((w (make-mode-line-window (screen-root screen) screen)))
    (xlib:map-window w)
    (make-mode-line :window w
		    :screen screen
		    :head head
		    :format format
		    :position *mode-line-position*
		    :gc (make-mode-line-gc w screen))))

(defun redraw-mode-line (ml)
  (when (eq (mode-line-mode ml) :stump)
    (let* ((*current-mode-line-formatters* *screen-mode-line-formatters*)
	   (*current-mode-line-formatter-args* (list (screen-current-group (mode-line-screen ml)) (mode-line-head ml)))
	   (string (mode-line-format-string ml)))
      (xlib:draw-image-glyphs (mode-line-window ml) (mode-line-gc ml)
			      *mode-line-pad-x*
			      (+ (xlib:font-ascent (xlib:gcontext-font (mode-line-gc ml)))
				 *mode-line-pad-y*)
			      string
			      :translate #'translate-id
			      :size 16)
      ;; Just clear what we need to. This reduces flicker.
      (xlib:clear-area (mode-line-window ml)
		       :x (+ *mode-line-pad-x*
			     (xlib:text-width (xlib:gcontext-font (mode-line-gc ml)) string
					      :translate #'translate-id))))))

(defun find-mode-line-window (xwin)
  (dolist (s *screen-list*)
    (dolist (h (screen-heads s))
      (let ((mode-line (head-mode-line h)))
	(when (and mode-line (eq (mode-line-window mode-line) xwin))
	  (return-from find-mode-line-window mode-line))))))

(defun sync-mode-line (ml)
  (dolist (group (screen-groups (mode-line-screen ml)))
    (sync-all-frame-windows group)))

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
	  (setf (mode-line-position ml) (if (= y (head-y (mode-line-head ml))) :top :bottom))))
      nil)))

(defun place-mode-line-window (screen xwin)
  (let ((ml (make-mode-line :window xwin :screen screen :mode :visible :position *mode-line-position*)))
    (xlib:reparent-window xwin (screen-root screen) 0 0)
    (when (update-mode-line-position ml (xlib:drawable-x xwin) (xlib:drawable-y xwin))
      (resize-mode-line ml)
      (xlib:map-window xwin)
      (sync-mode-line ml))))

(defun update-screen-mode-lines ()
  (dolist (s *screen-list*)
    (dolist (h (screen-heads s))
      (let ((mode-line (head-mode-line h)))
	(when mode-line
	  (redraw-mode-line mode-line))))))

(defun turn-on-mode-line-timer ()
  (when (timer-p *mode-line-timer*)
    (cancel-timer *mode-line-timer*))
  (setf *mode-line-timer* (run-with-timer *mode-line-timeout*
                                          *mode-line-timeout*
                                          'update-screen-mode-lines)))

#|
(defun maybe-cancel-mode-line-timer ()
  (unless (find-if 'screen-mode-line *screen-list*)
    (when (timer-p *mode-line-timer*)
      (cancel-timer *mode-line-timer*)
      (setf *mode-line-timer* nil))))
|#

(defun toggle-mode-line (screen head &optional (format '*screen-mode-line-format*))
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
	  ;;        (maybe-cancel-mode-line-timer)
	  ))
      (progn
	(setf (head-mode-line head) (make-head-mode-line screen head format))
	(resize-mode-line (head-mode-line head))
	(redraw-mode-line (head-mode-line head))
	;; move the frames
	(dformat 3 "modeline: ~s~%" (head-mode-line head))
	;; setup the timer
	(turn-on-mode-line-timer)))

    (dolist (group (screen-groups screen))
      (sync-all-frame-windows group))))

(define-stumpwm-command "mode-line" ()
  (toggle-mode-line (current-screen) (current-head)))

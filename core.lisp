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
;; This file contains core functionality including functions on
;; windows, screens, and events.
;;
;; Code:

(in-package :stumpwm)

;; Do it this way so its easier to wipe the map and get a clean one.
(when (null *top-map*)
  (setf *top-map*
	(let ((m (make-sparse-keymap)))
	  (define-key m (kbd "C-t") '*root-map*)
	  m)))

;; Screen helper functions

(defun translate-id (src src-start src-end font dst dst-start)
  "A simple replacement for xlib:translate-default.  just the
identity with a range check."
  (let ((min (xlib:font-min-char font))
	(max (xlib:font-max-char font)))
    (decf src-end)
    (if (stringp src)	   ; clx does this test so i guess it's needed
	(loop for i from src-start to src-end
	   for j from dst-start
	   as c = (char-code (char src i))
	   if (<= min c max) do (setf (aref dst j) c)
	   else do (return i)
	   finally (return i))
	(loop for i from src-start to src-end
	   for j from dst-start
	   as c = (elt src i)
	   as n = (if (characterp c) (char-code c) c)
	   if (and (integerp n) (<= min n max)) do (setf (aref dst j) n)
	   else do (return i)
	   finally (return i)))))

(defun screen-x (screen)
  (declare (ignore screen))
  0)

(defun screen-y (screen)
  (let ((ml (screen-mode-line screen)))
    (if (and ml
	     (eq (mode-line-position ml) :top))
	(+ (xlib:drawable-height (mode-line-window ml))
	   (* 2 (xlib:drawable-border-width (mode-line-window ml))))
	0)))

(defun screen-height (screen)
  (let ((root (screen-root screen))
	(ml (screen-mode-line screen)))
    (- (xlib:drawable-height root)
       (or (and ml (true-height (mode-line-window ml)))
	   0))))

(defun screen-width (screen)
  (let ((root (screen-root screen)))
    (xlib:drawable-width root)))

(defun find-screen (root)
  "Return the screen containing the root window."
  (find-if (lambda (s)
	     (xlib:window-equal (screen-root s) root))
	   *screen-list*))


;;; Group function

(defun current-group (&optional (screen (current-screen)))
  "Return the current group for the current screen, unless
otherwise specified."
  (screen-current-group screen))

(defun move-group-to-head (screen group)
  "Move window to the head of the group's window list."
  ;(assert (member window (screen-mapped-windows screen)))
  (setf (screen-groups screen) (delete group (screen-groups screen)))
  (push group (screen-groups screen)))

(defun sort-groups (screen)
  "Return a copy of the screen's group list sorted by number."
  (sort1 (screen-groups screen)
	 (lambda (a b)
	   (< (group-number a)
	      (group-number b)))))

(defun fmt-group-status (group)
  (let ((screen (group-screen group)))
    (cond ((eq group (screen-current-group screen))
	   #\*)
	  ((and (typep (second (screen-groups screen)) 'group)
		(eq group (second (screen-groups screen))))
	   #\+)
	  (t #\-))))

(defun find-free-group-number (screen)
  "Return a free window number for GROUP."
  (find-free-number (mapcar 'group-number (screen-groups screen)) 1))

(defun group-current-window (group)
  (frame-window (tile-group-current-frame group)))

(defun switch-to-group (new-group)
  (let* ((screen (group-screen new-group))
	 (old-group (screen-current-group screen)))
    (unless (eq new-group old-group)
      ;; restore the visible windows
      (dolist (w (group-windows old-group))
	(when (eq (window-state w) +normal-state+)
	  (xwin-hide (window-xwin w) (window-parent w))))
      (dolist (w (group-windows new-group))
	(when (eq (window-state w) +normal-state+)
	  (xwin-unhide (window-xwin w) (window-parent w))))
      (setf (screen-current-group screen) new-group)
      (move-group-to-head screen new-group)
      ;; restore the focus
      (setf (screen-focus screen) nil)
      (focus-frame new-group (tile-group-current-frame new-group))
      (show-frame-indicator new-group))))

(defun move-window-to-group (window to-group)
  (unless (eq (window-group window) to-group)
    (let ((old-group (window-group window))
	  (old-frame (window-frame window)))
      (hide-window window)
      ;; house keeping
      (setf (group-windows (window-group window))
	    (remove window (group-windows (window-group window))))
      (setf (window-frame window) (tile-group-current-frame to-group)
	    (window-group window) to-group
	    (window-number window) (find-free-window-number to-group))
      (push window (group-windows to-group))
      (sync-frame-windows to-group (tile-group-current-frame to-group))
      ;; maybe pick a new window for the old frame
      (when (eq (frame-window old-frame) window)
	(setf (frame-window old-frame) (first (frame-windows old-group old-frame)))
	(focus-frame old-group old-frame)))))

(defun next-group (current &optional (list (screen-groups (group-screen current))))
  (let ((matches (member current list)))
    (if (null (cdr matches))
	;; If the last one in the list is current, then
	;; use the first one.
	(car list)
	;; Otherwise, use the next one in the list.
	(cadr matches))))

(defun merge-groups (from-group to-group)
  "Merge all windows in FROM-GROUP into TO-GROUP."
  (dolist (window (group-windows from-group))
    (move-window-to-group window to-group)))

(defun kill-group (group to-group)
  (when (> (length (screen-groups (group-screen group))) 1)
    (let ((screen (group-screen group)))
      (merge-groups group to-group)
      (setf (screen-groups screen) (remove group (screen-groups screen))))))

(defun add-group (screen name)
  (check-type screen screen)
  (check-type name string)
  (let* ((initial-frame (make-initial-frame
			 (screen-x screen) (screen-y screen)
			 (screen-width screen) (screen-height screen)))
	 (ng (make-tile-group
	      :frame-tree initial-frame
	      :current-frame initial-frame
	      :screen screen
	      :number (find-free-group-number screen)
	      :name name)))
    (setf (screen-groups screen) (append (screen-groups screen) (list ng)))
    ng))


;;; Window functions

(defun window-in-current-group-p (window)
  (eq (window-group window)
      (screen-current-group (window-screen window))))

(defun window-screen (window)
  (group-screen (window-group window)))

(defun update-window-border (window)
  ;; give it a colored border but only if there are more than 1 frames.
  (let* ((group (window-group window))
	 (screen (group-screen group)))
    (setf (xlib:window-border (window-parent window))
	  (if (and (> (length (group-frames group)) 1)
		   (eq (group-current-window group) window))
	      (get-color-pixel screen *focus-color*)
	      (get-color-pixel screen *unfocus-color*)))))

(defun send-client-message (window type &rest data)
  "Send a client message to a client's window."
  (xlib:send-event (window-xwin window)
		   :client-message nil
		   :window (window-xwin window)
		   :type type
		   :format 32
		   :data data))

(defun fmt-window-status (window)
  (let ((group (window-group window)))
    (cond ((eq window (group-current-window group))
	   #\*)
	  ((and (typep (second (group-windows group)) 'window)
		(eq window (second (group-windows group))))
	   #\+)
	  (t #\-))))

(defun fmt-window-marked (window)
  (if (window-marked window)
      #\#
      #\Space))

;; (defun update-window-mark (window)
;;   "Called when we need to draw or clear the mark."
;;   ;; FIXME: This doesn't work at all. I'd like to have little squares
;;   ;; that look like clamps on the corners of the window, likes its
;;   ;; sorta grabbed. But i dunno how to properly draw them.
;;   (let* ((screen (window-screen window)))
;;     (if (window-marked window)
;; 	(xlib:draw-rectangle (window-parent window) (screen-marked-gc (window-screen window))
;; 			     0 0 300 200 t)
;; 	(xlib:clear-area (window-parent window)))))

(defun xwin-name (win)
  (xlib:wm-name win))

;; FIXME: should we raise the winodw or its parent?
(defun raise-window (win)
  "Map the window if needed and bring it to the top of the stack. Does not affect focus."
  (when (window-hidden-p win)
    (unhide-window win))
  (when (window-in-current-group-p win)
    (setf (xlib:window-priority (window-parent win)) :top-if)))

;; some handy wrappers

(defun true-height (win)
  (+ (xlib:drawable-height win) (* (xlib:drawable-border-width win) 2)))

(defun true-width (win)
  (+ (xlib:drawable-width win) (* (xlib:drawable-border-width win) 2)))

(defun xwin-border-width (win)
  (xlib:drawable-border-width win))

(defun (setf xwin-border-width) (width win)
  (setf (xlib:drawable-border-width win) width))

(defun default-border-width-for-type (type)
  (ecase type
    (:normal *normal-border-width*)
    (:maxsize *maxsize-border-width*)
    (:transient *transient-border-width*)))

(defun xwin-class (win)
  (multiple-value-bind (res class) (xlib:get-wm-class win)
    (declare (ignore res))
    class))

(defun xwin-res-name (win)
  (multiple-value-bind (res class) (xlib:get-wm-class win)
    (declare (ignore class))
    res))

(defmacro def-window-attr (attr)
  "Create a new window attribute and corresponding get/set functions."
  (let ((win (gensym))
	(val (gensym)))
    `(progn
       (defun ,(intern (format nil "WINDOW-~a" attr)) (,win)
	 (gethash ,attr (window-plist ,win)))
       (defun (setf ,(intern (format nil "WINDOW-~a" attr))) (,val ,win)
	 (setf (gethash ,attr (window-plist ,win))) ,val))))

(defun sort-windows (group)
  "Return a copy of the screen's window list sorted by number."
  (sort1 (group-windows group)
	 (lambda (a b)
	   (< (window-number a)
	      (window-number b)))))

(defun marked-windows (group)
  "Return the marked windows in the specified group."
  (loop for i in (sort-windows group)
       when (window-marked i)
       collect i))

(defun clear-window-marks (group)
  (dolist (w (group-windows group))
    (setf (window-marked w) nil)))

(defun (setf xwin-state) (state xwin)
  "Set the state (iconic, normal, withdrawn) of a window."
  (xlib:change-property xwin
			:WM_STATE
			(list state)
			:WM_STATE
			32))

(defun xwin-state (xwin)
  "Get the state (iconic, normal, withdraw of a window."
  (first (xlib:get-property xwin :WM_STATE)))

(defun window-hidden-p (window)
  (eql (window-state window) +iconic-state+))

(defun xwin-unhide (xwin parent)
  (xlib:map-window parent)
  (xlib:map-subwindows parent)
  (setf	(xwin-state xwin) +normal-state+))

(defun unhide-window (window)
  (when (window-in-current-group-p window)
    (xwin-unhide (window-xwin window) (window-parent window)))
  (setf (window-state window) +normal-state+))

(defun xwin-hide (xwin parent)
  (setf	(xwin-state xwin) +iconic-state+)
  (xlib:unmap-window parent))

(defun hide-window (window)
  (dformat 2 "hide window: ~a~%" (window-name window))
  (setf (window-state window) +iconic-state+)
  (when (window-in-current-group-p window)
    (xwin-hide (window-xwin window) (window-parent window))))

(defun xwin-type (win)
  "Return one of :maxsize, :transient, or :normal."
  (or (and (xlib:get-property win :WM_TRANSIENT_FOR)
	   :transient)
      (and (let ((hints (xlib:wm-normal-hints win)))
	     (and hints (or (xlib:wm-size-hints-max-width hints)
			    (xlib:wm-size-hints-max-height hints))))
	   :maxsize)
      :normal))

;; Stolen from Eclipse
(defun xwin-send-configuration-notify (xwin x y w h bw)
  "Send a synthetic configure notify event to the given window (ICCCM 4.1.5)"
    (xlib:send-event xwin :configure-notify nil
		     :event-window xwin
		     :window xwin
		     :x x :y y
		     :width w
		     :height h
		     :border-width bw
		     :propagate-p nil))

(defun geometry-hints (win)
  "Return hints for max width and height and increment hints. These
hints have been modified to always be defined and never be greater
than the root window's width and height."
  (let* ((f (window-frame win))
	 (x (frame-x f))
	 (y (frame-y f))
	 (fwidth (- (frame-width f) (* 2 (xlib:drawable-border-width (window-parent win)))))
	 (fheight (- (frame-height f)
		     (* 2 (xlib:drawable-border-width (window-parent win)))))
	 (width fwidth)
	 (height fheight)
	 (hints (window-normal-hints win))
	 (hints-max-width (and hints (xlib:wm-size-hints-max-width hints)))
	 (hints-max-height (and hints (xlib:wm-size-hints-max-height hints)))
	 (hints-width (and hints (xlib:wm-size-hints-base-width hints)))
	 (hints-height (and hints (or (xlib:wm-size-hints-base-height hints))))
	 (hints-inc-x (and hints (xlib:wm-size-hints-width-inc hints)))
	 (hints-inc-y (and hints (xlib:wm-size-hints-height-inc hints)))
	 (hints-min-aspect (and hints (xlib:wm-size-hints-min-aspect hints)))
	 (hints-max-aspect (and hints (xlib:wm-size-hints-max-aspect hints)))
	 center)
    ;;    (dformat 4 "hints: ~s~%" hints)
    ;; determine what the width and height should be
    (cond
      ;; Adjust the defaults if the window is a transient_for window.
      ((eq (window-type win) :transient)
       (setf center t
	     width (min (window-width win) width)
	     height (min (window-height win) height)))
      ;; aspect hints are handled similar to max size hints
      ((and hints-min-aspect hints-max-aspect)
       (let ((ratio (/ width height)))
	 (cond ((< ratio hints-min-aspect)
		(setf height (ceiling width hints-min-aspect)))
	       ((> ratio hints-max-aspect)
		(setf width  (ceiling (* height hints-max-aspect)))))
	 (setf center t)))
      ;; Update our defaults if the window has the maxsize hints
      ((or hints-max-width hints-max-height)
       (when (and hints-max-width
		  (< hints-max-width width))
	 (setf width hints-max-width))
       (when (and hints-max-height
		  (< hints-max-height height))
	 (setf height hints-max-height))
       (setf center t))
      (t
       ;; if they have inc hints then start with the size and adjust
       ;; based on those increments until the window fits in the frame
       (when hints-inc-x
	 (let ((w (or hints-width (window-width win))))
	   (setf width (+ w (* hints-inc-x
			       (+ (floor (- fwidth w) hints-inc-x)))))))
       (when hints-inc-y
	 (let ((h (or hints-height (window-height win))))
	   (setf height (+ h (* hints-inc-y
				(+ (floor (- fheight h -1) hints-inc-y)))))))))
    ;; center if needed
    (when center
      (setf x (+ x (truncate (- fwidth width) 2))
	    y (+ y (truncate (- fheight height) 2))))
    ;; Now return our findings
    (values x y width height center)))

(defun set-window-geometry (win &key x y width height border-width)
  (macrolet ((update (xfn wfn v)
	       `(when ,v ;; (/= (,wfn win) ,v))
		  (setf (,xfn (window-xwin win)) ,v)
		  ,(when wfn `(setf (,wfn win) ,v)))))
    (xlib:with-state ((window-xwin win))
      (update xlib:drawable-x nil x)
      (update xlib:drawable-y nil y)
      (update xlib:drawable-width window-width width)
      (update xlib:drawable-height window-height height)
      (update xlib:drawable-border-width nil border-width)
      )))

(defun maximize-window (win)
  "Maximize the window."
  (multiple-value-bind (x y width height stick)
      (geometry-hints win)
    (dformat 4 "maximize window ~a x: ~d y: ~d width: ~d height: ~d stick: ~s~%" win x y width height stick)
    ;; Move the parent window
    (setf (xlib:drawable-x (window-parent win)) x
	  (xlib:drawable-y (window-parent win)) y)
    ;; This is the only place a window's geometry should change
    (set-window-geometry win :x 0 :y 0 :width width :height height :border-width 0)
    ;; the parent window should stick to the size of the window
    ;; unless it isn't being maximized to fill the frame.
    (if stick
	(setf (xlib:drawable-width (window-parent win)) (window-width win)
	      (xlib:drawable-height (window-parent win)) (window-height win))
	(let ((frame (window-frame win)))
	  (setf (xlib:drawable-width (window-parent win)) (- (frame-width frame)
							     (* 2 (xlib:drawable-border-width (window-parent win))))
		(xlib:drawable-height (window-parent win)) (- (frame-height frame)
							      (* 2 (xlib:drawable-border-width (window-parent win)))))))))

(defun find-free-window-number (group)
  "Return a free window number for GROUP."
  (find-free-number (mapcar 'window-number (group-windows group))))

(defun reparent-window (window)
  ;; apparently we need to grab the server so the client doesn't get
  ;; the mapnotify event before the reparent event. that's what fvwm
  ;; says.
  (xlib:with-server-grabbed (*display*)
    (let* ((screen (window-screen window))
	   (master-window (xlib:create-window
			   :parent (screen-root screen)
			   :x (xlib:drawable-x (window-xwin window)) :y (xlib:drawable-y (window-xwin window))
			   :width (window-width window)
			   :height (window-height window)
			   ;; normal windows geta black background
			   :background (get-bg-color-pixel screen)
			   :border (get-color-pixel screen *unfocus-color*)
			   :border-width (default-border-width-for-type (window-type window))
			   :event-mask *window-parent-events*)))
      (unless (eq (xlib:window-map-state (window-xwin window)) :unmapped)
	(incf (window-unmap-ignores window)))
      (xlib:reparent-window (window-xwin window) master-window 0 0)
      ;;     ;; we need to update these values since they get set to 0,0 on reparent
      ;;     (setf (window-x window) 0
      ;; 	  (window-y window) 0)
      (xlib:add-to-save-set (window-xwin window))
      (setf (window-parent window) master-window))))

(defun process-existing-windows (screen)
  "Windows present when stumpwm starts up must be absorbed by stumpwm."
  (let ((children (xlib:query-tree (screen-root screen))))
    (dolist (win children)
      (let ((map-state (xlib:window-map-state win))
	    (wm-state (xwin-state win)))
	;; Don't process override-redirect windows.
	(unless (or (eq (xlib:window-override-redirect win) :on)
		    (internal-window-p screen win))
	  (if (or (eql map-state :viewable)
		  (eql wm-state +iconic-state+))
	      (progn
		(dformat 1 "Processing ~S ~S~%" (xwin-name win) win)
		(process-mapped-window screen win)))))))
  ;; Once processing them, hide them all. Later one will be mapped and
  ;; focused. We can do this because on start up there is only 1 group.
  (mapcar 'hide-window (group-windows (screen-current-group screen))))

(defun xwin-grab-keys (win)
  (labels ((grabit (w key)
	     (let ((code (xlib:keysym->keycodes *display* (key-keysym key))))
	       ;; some keysyms aren't mapped to keycodes so just ignore them.
	       (when code
		 (xlib:grab-key w code
				:modifiers (x11-mods key) :owner-p t
				:sync-pointer-p nil :sync-keyboard-p t)
		 ;; Ignore numlock by also grabbing the keycombo with
		 ;; numlock on.
		 (when (modifiers-numlock *modifiers*)
		   (xlib:grab-key w code
				  :modifiers (x11-mods key t) :owner-p t
				  :sync-pointer-p nil :sync-keyboard-p t))))))
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (grabit win k))
	     *top-map*)))

(defun grab-keys-on-window (win)
  (xwin-grab-keys (window-xwin win)))

(defun xwin-ungrab-keys (win)
  (xlib:ungrab-key win :any :modifiers :any))

(defun ungrab-keys-on-window (win)
  (xwin-ungrab-keys (window-xwin win)))

(defun sync-keys ()
  "Any time *top-map* is modified this must be called."
  (loop for i in *screen-list*
     do (xwin-ungrab-keys (screen-focus-window i))
     do (loop for j in (screen-mapped-windows i)
	   do (xwin-ungrab-keys j))
     do (xlib:display-finish-output *display*)
     do (loop for j in (screen-mapped-windows i)
	   do (xwin-grab-keys j))
     do (xwin-grab-keys (screen-focus-window i)))
  (xlib:display-finish-output *display*))

(defgeneric group-add-window (group xwin)
  (:documentation "add window to the head of the group's windows list."))

(defmethod group-add-window ((group tile-group) xwin)
  ;; give it a number, put it in a frame
  (let ((window (make-window :xwin xwin
			     :width (xlib:drawable-width xwin) :height (xlib:drawable-height xwin)
			     :name (xwin-name xwin)
			     :class (xwin-class xwin)
			     :res (xwin-res-name xwin)
			     :type (xwin-type xwin)
			     :normal-hints (xlib:wm-normal-hints xwin)
			     :state +normal-state+
			     :group group
			     :plist (make-hash-table)
			     :number (find-free-window-number group)
			     :frame (tile-group-current-frame group)
			     :unmap-ignores 0)))
    (setf (frame-window (tile-group-current-frame group)) window)
    (push window (group-windows group))
    window))

(defun add-window (screen xwin)
  (push xwin (screen-mapped-windows screen))
  (group-add-window (screen-current-group screen) xwin))

(defun process-mapped-window (screen xwin)
  "Add the window to the screen's mapped window list and process it as
needed."
  (let ((window (add-window screen xwin)))
    (setf (xlib:window-event-mask (window-xwin window)) *window-events*)
    ;; windows always have border width 0. Their parents provide the
    ;; border.
    (set-window-geometry window :border-width 0)
    (reparent-window window)
    (xlib:map-window (window-parent window))
    (maximize-window window)
    (xlib:map-subwindows (window-parent window))
    (grab-keys-on-window window)
    ;; quite often the modeline displays the window list, so update it
    (when (screen-mode-line screen)
      (redraw-mode-line-for (screen-mode-line screen) screen))
    ;; Run the map window hook on it
    (run-hook-with-args *map-window-hook* window)
    window))

(defun find-withdrawn-window (xwin)
  "Return the window and screen for a withdrawn window."
  (dolist (i *screen-list*)
    (let ((w (find xwin (screen-withdrawn-windows i) :key 'window-xwin :test 'xlib:window-equal)))
      (when w
	(return-from find-withdrawn-window (values w i))))))

(defun restore-window (window)
  "Restore a withdrawn window"
  ;; put it in a valid group
  (let ((screen (window-screen window)))
    (unless (find (window-group window)
		  (screen-groups screen))
      (setf (window-group window) (screen-current-group screen)))
    ;; FIXME: somehow it feels like this could be merged with group-add-window
    (setf (window-number window) (find-free-window-number (window-group window))
	  (window-frame window) (tile-group-current-frame (window-group window))
	  (window-state window) +iconic-state+
	  (xwin-state (window-xwin window)) +iconic-state+
	  (screen-withdrawn-windows screen) (delete window (screen-withdrawn-windows screen)))
    (push window (group-windows (window-group window)))
    ;; give it focus
    (frame-raise-window (window-group window) (window-frame window) window)))

(defun withdraw-window (window)
  "Withdrawing a window means just putting it in a list til we get a destroy event."
  ;; This function cannot request info about WINDOW from the xserver as it may not exist anymore.
  (let ((f (window-frame window))
	(group (window-group window))
	(screen (window-screen window)))
    (dformat 1 "withdraw window ~a~%" screen)
    ;; Save it for later since it is only withdrawn, not destroyed.
    (push window (screen-withdrawn-windows screen))
    (setf (window-state window) +withdrawn-state+
	  (xwin-state (window-xwin window)) +withdrawn-state+)
    ;; Clean up the window's entry in the screen and group
    (setf (screen-mapped-windows screen)
	  (delete (window-xwin window) (screen-mapped-windows screen))
	  (group-windows group)
	  (delete window (group-windows group)))
    ;; remove it from it's frame structures
    (when (eq (frame-window f) window)
      (setf (frame-window f) (first (frame-windows group f))))
    (when (window-in-current-group-p window)
      ;; since the window doesn't exist, it doesn't have focus.
      (setf (screen-focus screen) nil))
    ;; If the current window was removed, then refocus the frame it
    ;; was in, since it has a new current window
    (when (eq (tile-group-current-frame group) f)
      (focus-frame (window-group window) f))
    ;; quite often the modeline displays the window list, so update it
    (when (screen-mode-line screen)
      (redraw-mode-line-for (screen-mode-line screen) screen))
    ;; Run the unmap hook on the window
    (run-hook-with-args *unmap-window-hook* window)))

(defun destroy-window (window)
  "The window has been destroyed. clean up our data structures."
  ;; This function cannot request info about WINDOW from the xserver
  (let ((screen (window-screen window)))
    (unless (eql (window-state window) +withdrawn-state+)
      (withdraw-window window))
    ;; now that the window is withdrawn, clean up the data structures
    (setf (screen-withdrawn-windows screen)
	  (delete window (screen-withdrawn-windows screen)))
    (dformat 1 "destroy window ~a~%" screen)
    (dformat 3 "destroying parent window~%")
    (xlib:destroy-window (window-parent window))
    (run-hook-with-args *destroy-window-hook* window)))

(defun move-window-to-head (group window)
  "Move window to the head of the group's window list."
  ;(assert (member window (screen-mapped-windows screen)))
  (setf (group-windows group) (delete window (group-windows group)))
  (push window (group-windows group)))

(defun no-focus (group last-win)
  "don't focus any window but still read keyboard events."
  (dformat 3 "no-focus~%")
  (let* ((screen (group-screen group)))
    (when (eq group (screen-current-group screen))
      (xlib:set-input-focus *display* (screen-focus-window screen) :POINTER-ROOT)
      (setf (screen-focus screen) nil)
      (move-screen-to-head screen))
    (when last-win
      (setf (xlib:window-border (window-parent last-win)) (get-color-pixel screen *unfocus-color*)))))

(defun maybe-hide-window (window new-window)
  "Hide WINDOW depending on what kind of window NEW-WINDOW is. if
NEW-WINDOW is nil then the window is being hidden."
  (when (or (null new-window)
	    (and (eql (window-frame window) (window-frame new-window))
		 (eq (window-type new-window) :normal)))
    (hide-window window)))

(defun focus-window (window)
  "Give the window focus. This means the window will be visible,
maximized, and given focus."
  (let* ((group (window-group window))
	 (screen (group-screen group))
	 (cw (screen-focus screen)))
    ;; If window to focus is already focused then our work is done.
    (unless (eq window cw)
      (raise-window window)
      (screen-set-focus screen window)
      ;;(send-client-message window :WM_PROTOCOLS +wm-take-focus+)
      (update-window-border window)
      (when cw
	(update-window-border cw))
      ;; Move the window to the head of the mapped-windows list
      (move-window-to-head group window)
      ;; If another window was focused, then call the unfocus hook for
      ;; it.
      (when cw
	;; iconize the previous window if it was in the same frame and
	;; is a :normal window
	(maybe-hide-window cw window)
	(run-hook-with-args *unfocus-window-hook* cw))
      (run-hook-with-args *focus-window-hook* window))))

(defun delete-window (window)
  "Send a delete event to the window."
  (dformat 3 "Delete window~%")
  (send-client-message window :WM_PROTOCOLS +wm-delete-window+))

(defun xwin-kill (window)
  "Kill the client associated with window."
  (dformat 3 "Kill client~%")
  (xlib:kill-client *display* (xlib:window-id window)))


;;; Message printing functions

(defun color-exists-p (color)
  (handler-case
      (loop for i in *screen-list*
	 always (xlib:lookup-color (xlib:screen-default-colormap (screen-number i)) color))
    (xlib:name-error () nil)))

(defun font-exists-p (font-name)
  ;; if we can list the font then it exists
  (plusp (length (xlib:list-font-names *display* font-name :max-fonts 1))))

(defun set-fg-color (color)
  (when (color-exists-p color)
    (dolist (i *screen-list*)
      (setf (screen-fg-color i) color))
    (update-colors-all-screens)
    t))

(defun set-bg-color (color)
  (when (color-exists-p color)
    (dolist (i *screen-list*)
      (setf (screen-bg-color i) color))
    (update-colors-all-screens)
    t))

(defun set-border-color (color)
  (when (color-exists-p color)
    (dolist (i *screen-list*)
      (setf (screen-border-color i) color))
    (update-colors-all-screens)
    t))

(defun set-font (font)
  (when (font-exists-p font)
    (dolist (i *screen-list*)
      (let ((fobj (xlib:open-font *display* (first (xlib:list-font-names *display* font :max-fonts 1)))))
	(xlib:close-font (screen-font i))
	(setf (screen-font i) fobj
	      (xlib:gcontext-font (screen-message-gc i)) fobj)))
    t))

(defun get-color-pixel (screen color)
  (xlib:alloc-color (xlib:screen-default-colormap (screen-number screen)) color))

(defun get-fg-color-pixel (screen)
  (get-color-pixel screen (screen-fg-color screen)))

(defun get-bg-color-pixel (screen)
  (get-color-pixel screen (screen-bg-color screen)))

(defun get-border-color-pixel (screen)
  (get-color-pixel screen (screen-border-color screen)))

(defun max-width (font l)
  "Return the width of the longest string in L using FONT."
  (loop for i in l
	maximize (xlib:text-width font i :translate #'translate-id)))

(defun setup-win-gravity (screen win gravity)
  "Position the x, y of the window according to its gravity."
  (let ((w (xlib:drawable-width win))
	(h (xlib:drawable-height win))
	(screen-width (xlib:drawable-width (screen-root screen)))
	(screen-height (xlib:drawable-height (screen-root screen))))
    (let ((x (case gravity
	       ((:top-left :bottom-left) 0)
	       (:center (truncate (- screen-width w (* (xlib:drawable-border-width win) 2)) 2))
	       (t (- screen-width w (* (xlib:drawable-border-width win) 2)))))
	  (y (case gravity
	       ((:bottom-right :bottom-left) (- screen-height h (* (xlib:drawable-border-width win) 2)))
	       (:center (truncate (- screen-height h (* (xlib:drawable-border-width win) 2)) 2))
	       (t 0))))
      (setf (xlib:drawable-y win) (max 0 y)
	    (xlib:drawable-x win) (max 0 x)))))

(defun setup-message-window (screen l)
  (let ((height (* (length l)
		   (+ (xlib:font-ascent (screen-font screen))
		      (xlib:font-descent (screen-font screen)))))
	(width (max-width (screen-font screen) l))
	(win (screen-message-window screen)))
    ;; Now that we know the dimensions, raise and resize it.
    (xlib:map-window (screen-message-window screen))
    (setf (xlib:drawable-height win) height
	  (xlib:drawable-width win) (+ width (* *message-window-padding* 2))
	  (xlib:window-priority win) :above)
    (setup-win-gravity screen win *message-window-gravity*)
    ;; Clear the window
    (xlib:clear-area win)))

(defun invert-rect (screen win x y width height)
  "invert the color in the rectangular area. Used for highlighting text."
  (let ((gcontext (xlib:create-gcontext :drawable win
					:foreground (get-fg-color-pixel screen)
					:function boole-xor)))
    (xlib:draw-rectangle win gcontext x y width height t)
    (setf (xlib:gcontext-foreground gcontext) (get-bg-color-pixel screen))
    (xlib:draw-rectangle win gcontext x y width height t)))


;;; Frame functions

(defun frame-raise-window (g f w &optional (focus t))
  "Raise the window w in frame f in screen s. if FOCUS is
T (default) then also focus the frame."
  ;; nothing to do when W is nil
  (let ((oldw (frame-window f)))
    (setf (frame-window f) w)
    (when focus
      (focus-frame g f))
    ;; The old one might need to be hidden
    (unless (and w (eq oldw w))
      (when oldw
	(maybe-hide-window oldw w))
      (when w
	(raise-window w)))))

(defun focus-frame (group f)
  (let ((w (frame-window f))
	(last (tile-group-current-frame group)))
    (setf (tile-group-current-frame group) f)
    ;; record the last frame to be used in the fother command.
    (unless (eq f last)
      (setf (tile-group-last-frame group) last))
    (if w
	(focus-window w)
      (no-focus group (frame-window last)))
    (run-hook-with-args *focus-frame-hook* f last)))

(defun frame-windows (group f)
  (remove-if-not (lambda (w) (eq (window-frame w) f))
		 (group-windows group)))

(defun frame-sort-windows (group f)
  (remove-if-not (lambda (w) (eq (window-frame w) f))
		 (sort-windows group)))

(defun make-initial-frame (x y w h)
  "Used to create an initial frame hash for a screen."
  (make-frame :number 0
	      :x x
	      :y y
	      :width w
	      :height h
	      :window nil))

(defun copy-frame-tree (group)
  "Return a copy of the frame tree."
  (labels ((copy (tree)
	     (cond ((null tree) tree)
		   ((typep tree 'frame)
		    (copy-structure tree))
		   (t
		    (mapcar 'copy-frame-tree tree)))))
    (copy (tile-group-frame-tree group))))

(defun group-frames (group)
  (tree-accum-fn (tile-group-frame-tree group) 'nconc 'list))

(defun find-free-frame-number (group)
  (find-free-number (mapcar (lambda (f) (frame-number f))
			    (group-frames group))))

(defun split-frame-h (group p)
  "Return 2 new frames. The first one stealing P's number and window"
  (let* ((w (truncate (/ (frame-width p) 2)))
	 (h (frame-height p))
	 (f1 (make-frame :number (frame-number p)
			 :x (frame-x p)
			 :y (frame-y p)
			 :width w
			 :height h
			 :window (frame-window p)))
	 (f2 (make-frame :number (find-free-frame-number group)
			 :x (+ (frame-x p) w)
			 :y (frame-y p)
			 :width w
			 :height h
			 :window nil)))
    (run-hook-with-args *new-frame-hook* f2)
    (values f1 f2)))

(defun split-frame-v (group p)
  "Return 2 new frames. The first one stealing P's number and window"
  (let* ((w (frame-width p))
	 (h (truncate (/ (frame-height p) 2)))
	 (f1 (make-frame :number (frame-number p)
			 :x (frame-x p)
			 :y (frame-y p)
			 :width w
			 :height h
			 :window (frame-window p)))
	 (f2 (make-frame :number (find-free-frame-number group)
			 :x (frame-x p)
			 :y (+ (frame-y p) h)
			 :width w
			 :height h
			 :window nil)))
    (run-hook-with-args *new-frame-hook* f2)
    (values f1 f2)))

(defun funcall-on-leaf (tree leaf fn)
  "Return a new tree with LEAF replaced with the result of calling FN on LEAF."
  (cond ((atom tree)
	 (if (eq leaf tree)
	     (funcall fn leaf)
	   tree))
	(t (list (funcall-on-leaf (first tree) leaf fn)
		 (funcall-on-leaf (second tree) leaf fn)))))

(defun funcall-on-node (tree fn match)
  "Call fn on the node where match returns t."
  (if (funcall match tree)
      (funcall fn tree)
    (cond ((atom tree) tree)
	  (t (list (funcall-on-node (first tree) fn match)
		   (funcall-on-node (second tree) fn match))))))

(defun replace-frame-in-tree (tree f f1 f2)
  (funcall-on-leaf tree f (lambda (f)
                            (declare (ignore f))
			    (list f1 f2))))

(defun sibling (tree leaf)
  "Return the sibling of LEAF in TREE."
  (cond ((atom tree) nil)
	((eq (first tree) leaf)
	 (second tree))
	((eq (second tree) leaf)
	 (first tree))
	(t (or (sibling (first tree) leaf)
	       (sibling (second tree) leaf)))))

(defun migrate-frame-windows (group src dest)
  "Migrate all windows in SRC frame to DEST frame."
  (mapc (lambda (w)
	  (when (eq (window-frame w) src)
	    (setf (window-frame w) dest)))
	(group-windows group)))

(defun tree-accum-fn (tree acc fn)
  "Run an accumulator function on fn applied to each leaf"
  (cond ((null tree) nil)
	((atom tree)
	 (funcall fn tree))
	(t (funcall acc
		    (tree-accum-fn (first tree) acc fn)
		    (tree-accum-fn (second tree) acc fn)))))

(defun tree-iterate (tree fn)
  "Call FN on every leaf in TREE"
  (cond ((null tree) nil)
	((atom tree)
	 (funcall fn tree))
	(t (tree-iterate (first tree) fn)
           (tree-iterate (second tree) fn))))

(defun tree-x (tree)
  (tree-accum-fn tree 'min 'frame-x))

(defun tree-y (tree)
  (tree-accum-fn tree 'min 'frame-y))

(defun tree-width (tree)
  (tree-accum-fn tree '+ 'frame-width))

(defun tree-height (tree)
  (tree-accum-fn tree '+ 'frame-height))

(defun tree-row-split (tree)
  "Return t if the children of tree are stacked vertically"
  (= (tree-y (first tree)) (tree-y (second tree))))

(defun tree-column-split (tree)
  "Return t if the children of tree are side-by-side"
  (= (tree-x (first tree)) (tree-x (second tree))))

(defun expand-frame (f amount dir)
  (ecase dir
    (left (decf (frame-x f) amount)
	  (incf (frame-width f) amount))
    (right (incf (frame-width f) amount))
    (top (decf (frame-y f) amount)
	 (incf (frame-height f) amount))
    (bottom (incf (frame-height f) amount))))

(defun expand-tree (tree amount dir)
  "expand the frames in tree by AMOUNT in DIR direction. DIR can be 'top 'bottom 'left 'right"
  (cond ((null tree) nil)
	((atom tree)
	 (expand-frame tree amount dir))
	(t (if (or (and (member dir '(left right))
			(tree-column-split tree))
		   (and (member dir '(top bottom))
			(tree-row-split tree)))
	       (progn
		 (expand-tree (first tree) amount dir)
		 (expand-tree (second tree) amount dir))
	     (let ((n (truncate amount 2)))
	       (multiple-value-bind (a b)
		   (if (find dir '(left top))
		       (values (first tree) (second tree))
		     (values (second tree) (first tree)))
		 ;; first expand it the full amount to take up the
		 ;; space. then shrink it in the other direction by
		 ;; half.
		 (expand-tree a amount dir)
		 (expand-tree a (- n) (ecase dir
					       (left 'right)
					       (right 'left)
					       (top 'bottom)
					       (bottom 'top)))
		 ;; the other side simple needs to be expanded half
		 ;; the amount.
		 (expand-tree b n dir)))))))

(defun join-subtrees (tree keep)
  "expand one of the children of tree to occupy the space of the other
child. KEEP decides which child to keep. It can be 'LEFT or
'RIGHT. Return the child that was kept."
  (multiple-value-bind (child other)
      (if (eql keep 'left)
	  (values (first tree) (second tree))
	(values (second tree) (first tree)))
    (if (tree-row-split tree)
	(expand-tree child
		     (tree-width other)
		     (if (eql keep 'left) 'right 'left))
      (expand-tree child
		   (tree-height other)
		   (if (eql keep 'left) 'bottom 'top)))
    child))

(defun remove-frame (tree leaf)
  "Return a new tree with LEAF and it's sibling merged into
one."
  (cond ((atom tree) tree)
	((and (atom (first tree))
	      (eq (first tree) leaf))
	 (join-subtrees tree 'right))
	((and (atom (second tree))
	      (eq (second tree) leaf))
	 (join-subtrees tree 'left))
	(t (list (remove-frame (first tree) leaf)
		 (remove-frame (second tree) leaf)))))

(defun sync-frame-windows (group frame)
  "synchronize windows attached to FRAME."
  (mapc (lambda (w)
	  (when (eq (window-frame w) frame)
	    (dformat 3 "maximizing ~S~%" w)
	    (maximize-window w)))
	(group-windows group)))

(defun sync-all-frame-windows (group)
  (let ((tree (tile-group-frame-tree group)))
    (tree-iterate tree
		  (lambda (f)
		    (sync-frame-windows group f)))))

(defun depth-first-search (tree elt &key (test #'eq))
  "If ELT is in TREE return the branches from ELT up to and including TREE"
  (if (atom tree)
      (funcall test tree elt)
    (labels ((find-path (acc)
			(let ((current (car acc)))
			  (cond ((atom current)
				 (when (funcall test elt current)
				   (throw 'found (cdr acc))))
				(t (find-path (cons (first current) acc))
				   (find-path (cons (second current) acc)))))))
      (catch 'found (find-path (list tree))))))

(defun spree-root-branch (tree pred t-frame)
  "Find the first parent branch of T-FRAME in TREE for which PRED no longer is T"
  (let ((path (depth-first-search tree t-frame)))
    ;; path is the path of branches traversed to reach T-FRAME
    (when path
      (loop for branch in path while (funcall pred branch)
	    finally (return branch)))))

(defun offset-frames (group x y)
  "move the screen's frames around."
  (let ((tree (tile-group-frame-tree group)))
    (tree-iterate tree (lambda (frame)
			 (incf (frame-x frame) x)
			 (incf (frame-y frame) y)))))

(defun resize-frame (group frame amount dim)
  "Resize FRAME by AMOUNT in DIM dimension, DIM can be either 'width or 'height"
  (let ((tree (tile-group-frame-tree group))
	(screen (group-screen group)))
    ;; if FRAME is taking up the whole DIM or if AMOUNT = 0, do nothing
    (unless (or (zerop amount)
                (case dim
                  (width  (< (screen-width screen) (+ (frame-width frame) amount)))
                  (height (< (screen-height screen) (+ (frame-height frame) amount)))))
      (let* ((split-pred (ecase dim
                           (width   #'tree-column-split)
                           (height  #'tree-row-split)))
             (a-branch (spree-root-branch tree
                                          (lambda (b)
                                            (funcall split-pred b))
                                          frame)))
        (multiple-value-bind (a b)
            (if (depth-first-search (first a-branch) frame)
                (values (first a-branch) (second a-branch))
		(values (second a-branch) (first a-branch)))
          (let ((dir (ecase dim
                       (width (if (< (tree-x a) (tree-x b))
				  'right 'left))
                       (height (if (< (tree-y a) (tree-y b))
				   'bottom 'top)))))
            (expand-tree a amount dir)
            (expand-tree b (- amount) (ecase dir
					(left 'right)
					(right 'left)
					(top 'bottom)
					(bottom 'top)))
            (tree-iterate a-branch
                          (lambda (leaf)
                            (sync-frame-windows group leaf)))))))))

(defun split-frame (group how-fn)
  (let* ((frame (tile-group-current-frame group)))
    (multiple-value-bind (f1 f2) (funcall how-fn frame)
      (setf (tile-group-frame-tree group)
	    (replace-frame-in-tree (tile-group-frame-tree group)
				   frame f1 f2))
      (migrate-frame-windows group frame f1)
      (if (eq (tile-group-current-frame group)
	      frame)
	  (setf (tile-group-current-frame group) f1))
      (setf (tile-group-last-frame group) f2)
      (sync-frame-windows group f1)
      (sync-frame-windows group f2))))

(defun draw-frame-outlines (group)
  "Draw an outline around all frames in SCREEN."
  (let* ((screen (group-screen group))
	 (gc (xlib:create-gcontext :drawable (screen-root screen)
				   :font (screen-font screen)
				   :foreground (get-fg-color-pixel screen)
				   :background (get-bg-color-pixel screen)
				   :line-style :dash)))
    (mapc (lambda (f)
	    (xlib:draw-line (screen-root screen) gc
			    (frame-x f) (frame-y f) (frame-width f) 0 t)
	    (xlib:draw-line (screen-root screen) gc
			    (frame-x f) (frame-y f) 0 (frame-height f) t))
	  (group-frames group))))

(defun clear-frame-outlines (group)
  "Clear the outlines drawn with DRAW-FRAME-OUTLINES."
  (xlib:clear-area (screen-root (group-screen group))))

(defun draw-frame-numbers (group)
  "Draw the number of each frame in its corner. Return the list of
windows used to draw the numbers in. The caller must destroy them."
  (let ((screen (group-screen group)))
    (mapcar (lambda (f)
	      (let ((w (xlib:create-window
			:parent (screen-root screen)
			:x (frame-x f) :y (frame-y f) :width 1 :height 1
			:background (get-fg-color-pixel screen)
			:border (get-border-color-pixel screen)
			:border-width 1
			:event-mask '())))
		(xlib:map-window w)
		(setf (xlib:window-priority w) :above)
		(echo-in-window w (screen-font screen)
				(get-fg-color-pixel screen)
				(get-bg-color-pixel screen)
				(string (get-frame-number-translation f)))
		(xlib:display-finish-output *display*)
		(dformat 3 "mapped ~S~%" (frame-number f))
		w))
	    (group-frames group))))


;;; Screen functions

(defun sort-screens ()
  "Return the list of screen sorted by ID."
  (sort1 *screen-list*
	 (lambda (a b)
	   (< (screen-id a)
	      (screen-id b)))))

(defun next-screen (&optional (list (sort-screens)))
  (let ((matches (member (current-screen) list)))
    (if (null (cdr matches))
	;; If the last one in the list is current, then
	;; use the first one.
	(car list)
	;; Otherwise, use the next one in the list.
	(cadr matches))))

(defun move-screen-to-head (screen)
  (setf *screen-list* (remove screen *screen-list*))
  (push screen *screen-list*))

(defun switch-to-screen (screen)
  (when (and screen
	     (not (eq screen (current-screen))))
    (if (screen-focus screen)
	(xlib:set-input-focus *display* (window-xwin (screen-focus screen)) :POINTER-ROOT)
	(xlib:set-input-focus *display* (screen-focus-window screen) :POINTER-ROOT))
    (move-screen-to-head screen)))

(defun screen-set-focus (screen window)
  (when (eq (window-group window)
	    (screen-current-group screen))
    (xlib:set-input-focus *display* (window-xwin window) :POINTER-ROOT)
    (setf (screen-focus screen) window)
    (move-screen-to-head screen)))

(defun screen-current-window (screen)
  (group-current-window (screen-current-group screen)))

;;; TODO: Will windows exist in multiple groups, one day?
(defun find-window (xwin)
  (dformat 3 "find-window!~%")
  (dolist (i *screen-list*)
    (dolist (g (screen-groups i))
      (let ((w (find xwin (group-windows g) :key 'window-xwin :test 'xlib:window-equal)))
	(when w
	  (return-from find-window w))))))

(defun screen-root (screen)
  (xlib:screen-root (screen-number screen)))

(defun update-colors-for-screen (screen)
  (setf (xlib:gcontext-foreground (screen-message-gc screen)) (get-fg-color-pixel screen)
	(xlib:gcontext-background (screen-message-gc screen)) (get-bg-color-pixel screen))
  (dolist (i (list (screen-message-window screen)
		   (screen-frame-window screen)
		   (screen-input-window screen)))
    (setf (xlib:window-border i) (get-border-color-pixel screen)
	  (xlib:window-background i) (get-bg-color-pixel screen))))

(defun update-colors-all-screens ()
  "After setting the fg, bg, or border colors. call this to sync any existing windows."
  (mapc 'update-colors-for-screen *screen-list*))

(defun internal-window-p (screen win)
  "Return t if win is a window used by stumpwm"
  (or (xlib:window-equal (screen-message-window screen) win)
      (xlib:window-equal (screen-input-window screen) win)
      (xlib:window-equal (screen-frame-window screen) win)
      (xlib:window-equal (screen-focus-window screen) win)))

(defun unmap-message-window (screen)
  "Unmap the screen's message window, if it is mapped."
  (unless (eq (xlib:window-map-state (screen-message-window screen)) :unmapped)
    (xlib:unmap-window (screen-message-window screen))))

(defun unmap-all-message-windows ()
  (mapc #'unmap-message-window *screen-list*))

(defun unmap-frame-indicator (screen)
  (unless (eq (xlib:window-map-state (screen-frame-window screen)) :unmapped)
    (xlib:unmap-window (screen-frame-window screen))))

(defun unmap-all-frame-indicators ()
  (mapc #'unmap-frame-indicator *screen-list*))

(defun show-frame-indicator (group &optional force-draw)
  (let* ((screen (group-screen group))
	 (w (screen-frame-window screen))
	 (s "Current Frame")
	 (height (font-height (screen-font screen)))
	 (width (xlib:text-width (screen-font screen) s))
	 (cf (tile-group-current-frame group)))
    ;; first thing, make sure its not being displayed somewhere else
    (unmap-all-frame-indicators)
    ;; don't bother drawing it if there's only one frame.
    (when (or force-draw
	      (consp (tile-group-frame-tree group)))
      (xlib:map-window w)
      (setf (xlib:drawable-x w) (+ (frame-x cf) (truncate (- (frame-width cf) width) 2))
	    (xlib:drawable-y w) (+ (frame-y cf) (truncate (- (frame-height cf) height) 2))
	    (xlib:window-priority w) :above)
      (echo-in-window w (screen-font screen)
		      (get-fg-color-pixel screen)
		      (get-bg-color-pixel screen)
		      s)
      (xlib:display-finish-output *display*)
      (reset-timeout-for-frame-indicator))))

(defun echo-in-window (win font fg bg string)
  (let* ((height (font-height font))
	 (gcontext (xlib:create-gcontext :drawable win
					 :font font
					 :foreground fg
					 :background bg))
	 (width (xlib:text-width font string)))
    (setf (xlib:drawable-height win) height
	  (xlib:drawable-width win) width)
    (xlib:clear-area win)
    (xlib:draw-image-glyphs win gcontext 0 (xlib:font-ascent font) string)))

(defun push-last-message (screen strings highlights)
  ;; only push unique messages
  (unless *record-last-msg-override*
    (push strings (screen-last-msg screen))
    (push highlights (screen-last-msg-highlights screen))
    ;; crop for size
    (when (>= (length (screen-last-msg screen)) *max-last-message-size*)
      (setf (screen-last-msg screen) (butlast (screen-last-msg screen)
					      (- (length (screen-last-msg screen))
						 *max-last-message-size*))
	    (screen-last-msg-highlights screen) (butlast (screen-last-msg-highlights screen)
							 (- (length (screen-last-msg screen))
							    *max-last-message-size*))))))

(defun echo-nth-last-message (screen n)
  (let ((*record-last-msg-override* t))
    (apply 'echo-string-list screen (nth n (screen-last-msg screen)) (nth n (screen-last-msg-highlights screen)))))

(defun echo-string-list (screen strings &rest highlights)
  "Draw each string in l in the screen's message window. HIGHLIGHT is
the nth entry to highlight."
  (let* ((height (+ (xlib:font-descent (screen-font screen))
		    (xlib:font-ascent (screen-font screen))))
	 (gcontext (screen-message-gc screen))
	 (message-win (screen-message-window screen)))
    (setup-message-window screen strings)
    (loop for s in strings
	  ;; We need this so we can track the row for each element
	  for i from 0 to (length strings)
	  do (xlib:draw-image-glyphs message-win gcontext
				     *message-window-padding*
				     (+ (* i height)
					(xlib:font-ascent (screen-font screen)))
				     s
				     :translate #'translate-id
				     :size 16)
	  when (find i highlights :test 'eql)
	  do (invert-rect screen message-win
			  0 (* i height)
			  (xlib:drawable-width message-win)
			  height)))
  (xlib:display-finish-output *display*)
  (push-last-message screen strings highlights)
  ;; Set a timer to hide the message after a number of seconds
  (unless *supress-echo-timeout*
    (reset-timeout))
  (apply 'run-hook-with-args *message-hook* strings))

(defun echo-string (screen msg)
  "Print msg to SCREEN's message window."
  (echo-string-list screen (split-string msg (string #\Newline))))

(defun current-screen ()
  "Return the current screen."
  (car *screen-list*))

(defun init-screen (screen-number id host)
  "Given a screen number, returns a screen structure with initialized members"
  ;; Listen for the window manager events on the root window
  (setf (xlib:window-event-mask (xlib:screen-root screen-number))
	'(:substructure-redirect
	  :substructure-notify
	  :property-change))
  (xlib:display-finish-output *display*)
  ;; Initialize the screen structure
  (let* ((screen (make-screen))
	 (fg (xlib:alloc-color (xlib:screen-default-colormap screen-number) +default-foreground-color+))
	 (bg (xlib:alloc-color (xlib:screen-default-colormap screen-number) +default-background-color+))
	 (border (xlib:alloc-color (xlib:screen-default-colormap screen-number) +default-border-color+))
	 (input-window (xlib:create-window :parent (xlib:screen-root screen-number)
					   :x 0 :y 0 :width 20 :height 20
					   :background bg
					   :border border
					   :border-width 1
					   :colormap (xlib:screen-default-colormap
						      screen-number)
					   :event-mask '(:key-press)))
	 (focus-window (xlib:create-window :parent (xlib:screen-root screen-number)
					   :x 0 :y 0 :width 1 :height 1))
	 (frame-window (xlib:create-window :parent (xlib:screen-root screen-number)
					   :x 0 :y 0 :width 1 :height 1
					   :background fg
					   :border border
					   :border-width 1
					   :colormap (xlib:screen-default-colormap
						      screen-number)
					   :event-mask '()))
	 (message-window (xlib:create-window :parent (xlib:screen-root screen-number)
					     :x 0 :y 0 :width 1 :height 1
					     :background bg
					     :border border
					     :border-width 1
					     :colormap (xlib:screen-default-colormap
							screen-number)
					     :event-mask '()))
	 (initial-frame (make-initial-frame 0 0
					    (xlib:screen-width screen-number)
					    (xlib:screen-height screen-number)))
	 (font (xlib:open-font *display* +default-font-name+))
	 (group (make-tile-group
		 :frame-tree initial-frame
		 :current-frame initial-frame
		 :screen screen
		 :number 1
		 :name "Default")))
    ;; Create our screen structure
    ;; The focus window is mapped at all times
    (xlib:map-window focus-window)
    (xwin-grab-keys focus-window)
    (setf (screen-number screen) screen-number
	  (screen-id screen) id
	  (screen-host screen) host
	  (screen-groups screen) (list group)
	  (screen-current-group screen) group
	  (screen-font screen) font
	  (screen-fg-color screen) +default-foreground-color+
	  (screen-bg-color screen) +default-background-color+
	  (screen-border-color screen) +default-border-color+
	  (screen-message-window screen) message-window
	  (screen-input-window screen) input-window
	  (screen-frame-window screen) frame-window
	  (screen-focus-window screen) focus-window
	  (screen-message-gc screen) (xlib:create-gcontext
				      :drawable message-window
				      :font font
				      :foreground (xlib:alloc-color (xlib:screen-default-colormap screen-number) +default-foreground-color+)
				      :background (xlib:alloc-color (xlib:screen-default-colormap screen-number) +default-background-color+))
	  (screen-marked-gc screen) (xlib:create-gcontext
				     ;; We just use this as placeholder. it'll actually be used
				     ;; on parent windows but according to the docs the
				     ;; drawables just have to have the same depth and something
				     ;; else to work. So this should.
				     :drawable message-window
				     :subwindow-mode :include-inferiors
				     :foreground  (xlib:alloc-color (xlib:screen-default-colormap screen-number) +default-foreground-color+)
				     :background (xlib:alloc-color (xlib:screen-default-colormap screen-number) +default-background-color+)))
    screen))


;;; keyboard helper functions

(defun key-to-keycode+state (key)
  (let ((code (xlib:keysym->keycodes *display* (key-keysym key))))
    (cond ((eq (xlib:keycode->keysym *display* code 0) (key-keysym key))
	   (values code (x11-mods key)))
	  ((eq (xlib:keycode->keysym *display* code 1) (key-keysym key))
	   (values code (apply 'xlib:make-state-mask
			       (cons :shift (xlib:make-state-keys (x11-mods key))))))
	  (t
	   ;; just warn them and go ahead as scheduled
	   (warn "Don't know how to encode ~s" key)
	   (values code (x11-mods key))))))

(defun send-fake-key (win key)
  "Send a fake key event to win. ch is the character and mods is a
list of modifier symbols."
  (multiple-value-bind (code state) (key-to-keycode+state key)
    (xlib:send-event (window-xwin win) :key-press (xlib:make-event-mask :key-press)
		     :display *display*
		     :root (screen-root (window-screen win))
		     :window (window-xwin win) :event-window (window-xwin win)
		     :code code
		     :state state)))

(defun send-fake-click (win button)
  "Send a fake key event to win. ch is the character and mods is a
list of modifier symbols."
  ;; I don't know why this doesn't work. Sadly CLX doesn't have the
  ;; XTest extension like xlib does. With it this would be 2 lines.
  (multiple-value-bind (x y) (xlib:query-pointer (window-xwin win))
    (multiple-value-bind (rx ry) (xlib:query-pointer (screen-root (window-screen win)))
      (xlib:send-event (window-xwin win) :button-press (xlib:make-event-mask :button-press)
		       :display *display*
		       :root (screen-root (window-screen win))
		       :window (window-xwin win) :event-window (window-xwin win)
		       :code button
		       :state 0
		       :x x :y y :root-x rx :root-y ry
		       :same-screen-p t)
      (xlib:send-event (window-xwin win) :button-release (xlib:make-event-mask :button-release)
		       :display *display*
		       :root (screen-root (window-screen win))
		       :window (window-xwin win) :event-window (window-xwin win)
		       :code button
		       :state #x100
		       :x x :y y :root-x rx :root-y ry
		       :same-screen-p t))))


;;; Pointer helper functions

(defun grab-pointer (screen)
  "Grab the pointer and set the pointer shape."
  (let* ((white (xlib:make-color :red 1.0 :green 1.0 :blue 1.0))
	 (black (xlib:make-color :red 0.0 :green 0.0 :blue 0.0))
	 (cursor-font (xlib:open-font *display* "cursor"))
	 (cursor (xlib:create-glyph-cursor :source-font cursor-font
					   :source-char 64
					   :mask-font cursor-font
					   :mask-char 65
					   :foreground black
					   :background white)))
    (xlib:grab-pointer (screen-root screen) nil :owner-p nil
		       :cursor cursor)))

(defun ungrab-pointer ()
  "Remove the grab on the cursor and restore the cursor shape."
  (xlib:ungrab-pointer *display*)
  (xlib:display-finish-output *display*))

(defun grab-keyboard (screen)
  (xlib:grab-keyboard (screen-root screen) :owner-p nil
		      :sync-keyboard-p nil :sync-pointer-p nil))

(defun ungrab-keyboard ()
  (xlib:ungrab-keyboard *display*))

(defun warp-pointer (screen x y)
  "Move the pointer to the specified location."
  (let ((root (screen-root screen)))
    (xlib:warp-pointer root x y)))

(defun warp-pointer-relative (dx dy)
  "Move the pointer by DX and DY relative to the current location."
  (xlib:warp-pointer-relative *display* dx dy))


;; Event handler functions

(defparameter *event-fn-table* (make-hash-table)
  "A hash of event types to functions")

(defmacro define-stump-event-handler (event keys &body body)
  (let ((fn-name (gensym))
	(event-slots (gensym)))
  `(labels ((,fn-name (&rest ,event-slots &key ,@keys &allow-other-keys)
	      (declare (ignore ,event-slots))
	      ,@body))
     (setf (gethash ,event *event-fn-table*) #',fn-name))))

;(define-stump-event-handler :map-notify (event-window window override-redirect-p)
;  )

(defun handle-unmanaged-window (xwin x y width height border-width value-mask)
  "Call this function for windows that stumpwm isn't
managing. Basically just give the window what it wants."
  (labels ((has-x (mask) (= 1 (logand mask 1)))
	   (has-y (mask) (= 2 (logand mask 2)))
	   (has-w (mask) (= 4 (logand mask 4)))
	   (has-h (mask) (= 8 (logand mask 8)))
	   (has-bw (mask) (= 16 (logand mask 16)))
	   ;; (has-stackmode (mask) (= 64 (logand mask 64)))
	   )
    (xlib:with-state (xwin)
      (when (has-x value-mask)
	(setf (xlib:drawable-x xwin) x))
      (when (has-y value-mask)
	(setf (xlib:drawable-y xwin) y))
      (when (has-h value-mask)
	(setf (xlib:drawable-height xwin) height))
      (when (has-w value-mask)
	(setf (xlib:drawable-width xwin) width))
      (when (has-bw value-mask)
      	(setf (xlib:drawable-border-width xwin) border-width)))))

(defun handle-managed-window (window width height stack-mode value-mask)
  "This is a managed window so deal with it appropriately."
  ;; Grant the stack-mode change (if it's mapped)
  (set-window-geometry window :width width :height height)
  (maximize-window window)
  (when (and (window-in-current-group-p window)
	     ;; stack-mode change?
	     (= 64 (logand value-mask 64)))
    (case stack-mode
      (:above
       (frame-raise-window (window-group window) (window-frame window) window)))))

(define-stump-event-handler :configure-request (stack-mode #|parent|# window #|above-sibling|# x y width height border-width value-mask)
  ;; Grant the configure request but then maximize the window after the granting.
  (dformat 3 "CONFIGURE REQUEST ~S ~S ~S~%" value-mask height (xlib:drawable-height window))
  (let ((win (find-window window)))
    (if win
	(handle-managed-window win width height stack-mode value-mask)
	(handle-unmanaged-window window x y width height border-width value-mask))))

(define-stump-event-handler :map-request (parent send-event-p window)
  (unless send-event-p
    ;; This assumes parent is a root window and it should be.
    (dformat 3 "map request: ~a ~a ~a~%" window parent (find-window window))
    (let ((screen (find-screen parent))
	  (win (find-window window))
	  (wwin (find-withdrawn-window window)))
      ;; only absorb it if it's not already managed (it could be iconic)
      (cond 
	(win (dformat 1 "map request for mapped window ~a~%" win))
	(wwin (restore-window wwin))
	(t
	 (let ((window (process-mapped-window screen window)))
	   ;; Give it focus
	   (frame-raise-window (window-group window) (window-frame window) window)))))))

(define-stump-event-handler :unmap-notify (send-event-p event-window window #|configure-p|#)
  ;; There are two kinds of unmap notify events: the straight up
  ;; ones where event-window and window are the same, and
  ;; substructure unmap events when the event-window is the parent
  ;; of window. So use event-window to find the screen.
  (dformat 2 "UNMAP: ~s ~s ~a~%" send-event-p (not (xlib:window-equal event-window window)) (find-window window))
  (unless (and (not send-event-p)
	       (not (xlib:window-equal event-window window)))
    (let ((window (find-window window)))
      ;; if we can't find the window then there's nothing we need to
      ;; do.
      (when window
	(if (plusp (window-unmap-ignores window))
	    (progn
	      (dformat 3 "decrement ignores! ~d~%" (window-unmap-ignores window))
	      (decf (window-unmap-ignores window)))
	    (withdraw-window window))))))

;;(define-stump-event-handler :create-notify (#|window parent x y width height border-width|# override-redirect-p))
  ;; (unless (or override-redirect-p
;; 	      (internal-window-p (window-screen window) window))
;;    (process-new-window (window-screen window) window))
;    (run-hook-with-args *new-window-hook* window)))


(define-stump-event-handler :destroy-notify (send-event-p event-window window)
  (unless (or send-event-p
	      (xlib:window-equal event-window window))
    ;; Ignore structure destroy notifies and only
    ;; use substructure destroy notifiers. This way
    ;; event-window is the window's parent.
    (let ((win (or (find-window window)
		   (find-withdrawn-window window))))
      (when win
	(destroy-window win)))))

(defun read-from-keymap (kmap)
  "Read a sequence of keys from the user, guided by the keymap,
KMAP and return the binding or nil if the user hit an unbound sequence."
  (let* ((code-state (loop for k = (read-key)
			while (is-modifier (xlib:keycode->keysym *display* (car k) 0))
			finally (return k)))
	 (code (car code-state))
	 (state (cdr code-state)))
    (handle-keymap kmap code state nil)))

(defun handle-keymap (kmap code state key-seq)
  "Find the command mapped to the (code state) and return it."
  ;; a symbol is assumed to have a hashtable as a value.
  (dformat 1 "Awaiting key ~a~%" kmap)
  (when (and (symbolp kmap)
	     (boundp kmap)
	     (hash-table-p (symbol-value kmap)))
    (setf kmap (symbol-value kmap)))
  (check-type kmap hash-table)
  (let* ((key (code-state->key code state))
	 (cmd (lookup-key kmap key))
	 (key-seq (cons key key-seq)))
    (dformat 1 "key-press: ~S ~S ~S~%" key state cmd)
    (if cmd
	(cond
	  ((or (hash-table-p cmd)
	       (and (symbolp cmd)
		    (boundp cmd)
		    (hash-table-p (symbol-value cmd))))
	   (let* ((code-state (do ((k (read-key) (read-key)))
				  ((not (is-modifier (xlib:keycode->keysym *display* (car k) 0))) k)))
		  (code (car code-state))
		  (state (cdr code-state)))
	     (handle-keymap cmd code state key-seq)))
	  (t (values cmd key-seq)))
	(values nil key-seq))))

(define-stump-event-handler :key-press (code state #|window|#)
  ;; modifiers can sneak in with a race condition. so avoid that.
  (unless (is-modifier (xlib:keycode->keysym *display* code 0))
    (labels ((get-cmd (screen code state)
	       (unwind-protect
		    (progn
		      (grab-pointer screen)
		      (grab-keyboard screen)
		      (handle-keymap *top-map* code state nil))
		 (ungrab-pointer)
		 (ungrab-keyboard)
		 ;; this force output is crucial. Without it weird
		 ;; things happen if an error happens later on.
		 (xlib:display-finish-output *display*))))
      (multiple-value-bind (cmd key-seq) (get-cmd (current-screen) code state)
	(unmap-message-window (current-screen))
	(if cmd
	    (interactive-command cmd)
	    (echo-string (current-screen) (format nil "~{~a ~}not bound." (mapcar 'print-key (nreverse key-seq)))))))))

(defun bytes-to-window (bytes)
  "A sick hack to assemble 4 bytes into a 32 bit number. This is
because ratpoison sends the rp_command_request window in 8 byte
chunks."
  (+ (first bytes)
     (ash (second bytes) 8)
     (ash (third bytes) 16)
     (ash (fourth bytes) 24)))

(defun handle-rp-commands (root)
  "Handle a ratpoison style command request."
  (labels ((one-cmd ()
	     (multiple-value-bind (win type format bytes-after) (xlib:get-property root :rp_command_request :end 4 :delete-p t)
	       (declare (ignore type format))
	       (setf win (xlib::lookup-window *display* (bytes-to-window win)))
	       (when (xlib:window-p win)
		 (let* ((data (xlib:get-property win :rp_command))
			(interactive-p (car data))
			(cmd (map 'string 'code-char (nbutlast (cdr data)))))
		   (declare (ignore interactive-p))
		   (interactive-command cmd)
		   (xlib:change-property win :rp_command_result (map 'list 'char-code "0TODO") :string 8)
		   (xlib:display-finish-output *display*)))
	       bytes-after)))
    (loop while (> (one-cmd) 0))))

(defun update-window-properties (window atom)
  (case atom
    (:wm_name
     (setf (window-name window) (xwin-name (window-xwin window))))
    (:wm_normal_hints
     (setf (window-normal-hints window) (xlib:wm-normal-hints (window-xwin window))
	   (window-type window) (xwin-type (window-xwin window)))
     (dformat 4 "new hints: ~s~%" (window-normal-hints window))
     (maximize-window window))
    (:wm_hints)
    (:wm_class
     (setf (window-class window) (xwin-class (window-xwin window))
	   (window-res window) (xwin-res-name (window-xwin window))))
    (:wm_transient_for
     (setf (window-type window) (xwin-type (window-xwin window)))
     (maximize-window window))))

(define-stump-event-handler :property-notify (window atom state)
  (dformat 2 "property notify ~s ~s ~s~%" window atom state)
  (case atom
    (:rp_command_request
     ;; we will only find the screen if window is a root window, which
     ;; is the only place we listen for ratpoison commands.
     (let* ((screen (find-screen window)))
       (when (and (eq state :new-value)
		  screen)
	 (handle-rp-commands window))))
     (t
      (let ((window (find-window window)))
	(when window
	  (update-window-properties window atom))))))

(define-stump-event-handler :mapping-notify (request start count)
  ;; We could be a bit more intelligent about when to update the
  ;; modifier map, but I don't think it really matters.
  (xlib:mapping-notify *display* request start count)
  (update-modifier-map)
  (sync-keys))

(define-stump-event-handler :selection-request (requestor property selection target time)
  (send-selection requestor property selection target time))

(define-stump-event-handler :selection-clear ()
  (setf *x-selection* nil))

(define-stump-event-handler :exposure (window x y width height count)
  (declare (ignore x y width height))
  (let ((screen (find-if (lambda (s)
			   (and (screen-mode-line s)
				(xlib:window-equal window (mode-line-window (screen-mode-line s)))))
			 *screen-list*)))
    (when (and screen
	       (zerop count))
      (redraw-mode-line-for (screen-mode-line screen) screen))))

(define-stump-event-handler :reparent-notify (window parent)
  (let ((win (find-window window)))
    (when (and win
	       (not (xlib:window-equal parent (window-parent win))))
      ;; reparent it back
      (unless (eq (xlib:window-map-state (window-xwin win)) :unmapped)
	(incf (window-unmap-ignores win)))
      (xlib:reparent-window (window-xwin win) (window-parent win) 0 0))))


;; Handling event :KEY-PRESS
;; (:DISPLAY #<XLIB:DISPLAY :0 (The X.Org Foundation R60700000)> :EVENT-KEY :KEY-PRESS :EVENT-CODE 2 :SEND-EVENT-P NIL :CODE 45 :SEQUENCE 1419 :TIME 98761213 :ROOT #<XLIB:WINDOW :0 96> :WINDOW #<XLIB:WINDOW :0 6291484> :EVENT-WINDOW #<XLIB:WINDOW :0 6291484> :CHILD
;;  #<XLIB:WINDOW :0 6291485> :ROOT-X 754 :ROOT-Y 223 :X 753 :Y 222 :STATE 4 :SAME-SCREEN-P T)
;; H

(defun handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  (dformat 1 ">>> ~S~%" event-key)
  (let ((eventfn (gethash event-key *event-fn-table*)))
    (when eventfn
      (handler-case
	  (progn
	    (apply eventfn event-slots)
	    (xlib:display-finish-output *display*))
	((or xlib:window-error xlib:drawable-error) (c)
	  ;; Asynchronous errors are handled in the error
	  ;; handler. Synchronous errors like trying to get the window
	  ;; hints on a deleted window are caught and ignored here. We
	  ;; do this inside the event handler so that the event is
	  ;; handled. If we catch it higher up the event will not be
	  ;; flushed from the queue and we'll get ourselves into an
	  ;; infinite loop.
	  (dformat 4 "ignore synchronous ~a~%" c))))
    (dformat 2 "<<< ~S~%" event-key)
    t))

;;; Selection

(defun export-selection ()
  (let* ((screen (current-screen))
	 (selwin (screen-focus-window (current-screen)))
	 (root (screen-root screen)))
    (xlib:set-selection-owner *display* :primary selwin)
    (unless (eq (xlib:selection-owner *display* :primary) selwin)
      (error "Can't set selection owner"))
    ;; also set the cut buffer for completeness
    (xlib:change-property root :cut-buffer0 *x-selection* :string 8 :transform #'xlib:char->card8
			  :mode :replace)))

(defun set-x-selection (text)
  (setf *x-selection* text)
  (export-selection))

(defun send-selection (requestor property selection target time)
  (dformat 1 "send-selection ~s ~s ~s ~s ~s~%" requestor property selection target time)
  (cond
    ;; they're requesting what targets are available
    ((eq target :targets)
     (xlib:change-property requestor property target (list :targets :string) 8 :mode :replace))
    ;; send them a string
    ((find target '(:string ))
     (xlib:change-property requestor property *x-selection* :string 8 :mode :replace :transform #'xlib:char->card8))
    ;; we don't know how to handle anything else
    (t
     (setf property nil)))
  (xlib:send-event requestor :selection-notify nil
		   :display *display*
		   :window requestor
		   :selection selection
		   :property property
		   :target target
		   :time time)
  (xlib:display-finish-output *display*))

(defun get-x-selection (&optional timeout)
  "Return the x selection no matter what client own it."
  (labels ((wait-for-selection (&rest event-slots &key display event-key &allow-other-keys)
	     (declare (ignore display))
	     (when (eq event-key :selection-notify)
	       (destructuring-bind (&key window property &allow-other-keys) event-slots
	         (if property
		     (xlib:get-property window property :type :string :result-type 'string :transform #'xlib:card8->char :delete-p t)
		     "")))))
    (if *x-selection*
	*x-selection*
	(progn
	  (xlib:convert-selection :primary :string (screen-input-window (current-screen)) :stumpwm-selection)
	  ;; Note: this may spend longer than timeout in this loop but it will eventually return.
	  (let ((time (get-internal-real-time)))
	    (loop for ret = (xlib:process-event *display* :handler #'wait-for-selection :timeout timeout :discard-p nil)
	       when (or ret
			(> (/ (- time (get-internal-real-time)) internal-time-units-per-second)
			   timeout))
	       ;; make sure we return a string
	       return (or ret "")))))))

;;; Top map push/popping

(defvar *top-map-list* nil)

(defun push-top-map (new-top)
  (push *top-map* *top-map-list*)
  (setf *top-map* new-top)
  (sync-keys))

(defun pop-top-map ()
  (when *top-map-list*
    (setf *top-map* (pop *top-map-list*))
    (sync-keys)
    t))

(defmacro save-frame-excursion (&body body)
  "Execute body and then restore the current frame."
  (let ((oframe (gensym "OFRAME"))
	(ogroup (gensym "OGROUP")))
    `(let ((,oframe (tile-group-current-frame (current-group)))
	   (,ogroup (current-group)))
       (unwind-protect (progn ,@body)
	 (focus-frame ,ogroup ,oframe)))))

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

;; Screen helper functions

(defun screen-height (screen)
  (let ((root (xlib:screen-root (screen-number screen))))
    (xlib:drawable-height root)))

(defun screen-width (screen)
  (let ((root (xlib:screen-root (screen-number screen))))
    (xlib:drawable-width root)))

(defun find-screen (root)
  "Return the screen containing the root window."
  (find-if (lambda (s)
	     (eql (xlib:screen-root (screen-number s))
		  root))
	   *screen-list*))


;;; Window functions

(defun default-window-format (screen w)
  "The default function called to format a window for display in the window list."
  (format nil "~D~C~A"
	  (window-number screen w)
	  (cond ((xlib:window-equal w (screen-current-window screen))
		 #\*)
		((and (xlib:window-p (second (screen-mapped-windows screen)))
		      (xlib:window-equal w (second (screen-mapped-windows screen))))
		 #\+)
		(t #\-))
	  (window-name w)))

(defun window-master (window)
  "Find the window's master window (the one it's been reparented to)."
  (multiple-value-bind (children parent) (xlib:query-tree window)
   (declare (ignore children))
   (if (xlib:window-equal (xlib:drawable-root window) parent)
       window 
     (window-master parent))))


(defun window-screen (w)
  "Return the screen associated with window w."
  (find-screen (xlib:drawable-root w)))

(defun window-name (win)
  (concatenate 'string (mapcar #'code-char (xlib:get-property win :WM_NAME))))

(defun window-number (screen win)
  (gethash :number (gethash win (screen-window-hash screen))))

(defun window-frame (screen win)
  (gethash :frame (gethash win (screen-window-hash screen))))

(defun set-window-frame (screen win frame)
  (setf (gethash :frame (gethash win (screen-window-hash screen)))
	frame))

(defsetf window-frame set-window-frame)

(defun sort-windows (screen)
  "Return a copy of the screen's window list sorted by number."
  (sort1 (screen-mapped-windows screen)
	 (lambda (a b)
	   (< (window-number screen a)
	      (window-number screen b)))))

(defun set-window-state (win state)
  "Set the state (iconic, normal, withdrawn) of a window."
  (xlib:change-property win
			:WM_STATE
			(list state)
			:WM_STATE
			32))

(defun window-state (win)
  "Get the state (iconic, normal, withdraw of a window."
  (first (xlib:get-property win :WM_STATE)))

(defsetf window-state set-window-state)

;(defun hide-window (window)
;  (xlib:unmap-window window)
;  (xlib:set

;; Stolen from Eclipse
(defun send-configuration-notify (window)
  "Send a synthetic configure notify event to the given window (ICCCM 4.1.5)"
  (multiple-value-bind (x y)
      (xlib:translate-coordinates window 0 0 (xlib:drawable-root window))
    (xlib:send-event window
		     :configure-notify
		     (xlib:make-event-mask :structure-notify)
		     :event-window window :window window
		     :x x :y y
		     :override-redirect-p nil
		     :border-width (xlib:drawable-border-width window)
		     :width (xlib:drawable-width window)
		     :height (xlib:drawable-height window)
		     :propagate-p nil)))
  
(defun maximize-window (win)
  "Maximize the window."
  (let* ((screen (window-screen win))
	 (hints (geometry-hints screen win))
	 (x (first hints))
	 (y (second hints))
	 (width (third hints))
	 (height (fourth hints))
	 (inc-x (fifth hints))
	 (inc-y (sixth hints)))
    ;; Move the window
    (setf (xlib:drawable-x win) x
	  (xlib:drawable-y win) y)
    ;; Resize the window
    (setf (xlib:drawable-width win)
	  (+ (xlib:drawable-width win)
	     (* inc-x (truncate (/ (- width (xlib:drawable-width win)) inc-x))))
	  (xlib:drawable-height win)
	  (+ (xlib:drawable-height win)
	     (* inc-y (truncate (/ (- height (xlib:drawable-height win)) inc-y)))))
    (xlib:display-force-output *display*)))

(defun find-free-window-number (screen)
  "Return a free window number for SCREEN."
  (find-free-number (mapcar-hash (lambda (val) (gethash :number val))
				 (screen-window-hash screen))))

(defun process-new-window (win)
  "When a new window is created (or when we are scanning initial
windows), this function dresses the window up and gets it ready to be
managed."
  ;; Listen for events
  (setf (xlib:window-event-mask win) '(:structure-notify
				       :property-change
				       :colormap-change
				       :focus-change))
  (set-window-state win 'normal))

(defun process-existing-windows (screen)
  "Windows present when stumpwm starts up must be absorbed by stumpwm."
  (let ((children (xlib:query-tree (xlib:screen-root (screen-number screen)))))
    (dolist (win children)
      (let ((map-state (xlib:window-map-state win))
	    (wm-state (window-state win)))
	;; Don't process override-redirect windows.
	(unless (or (eq (xlib:window-override-redirect win) :on)
		    (internal-window-p screen win))
	  (if (or (eql map-state :viewable)
		  (eql wm-state +iconic-state+))
	      (progn
		(pprint (list 'processing (window-name win) win))
		(process-new-window win)
		;; Pretend it's been mapped
		(absorb-mapped-window screen win))))))))
;;     ;; Once we've processed them, we need to give one of them focus.
;;     ;; FIXME: what if there aren't any windows?
;;     (when (screen-mapped-windows screen)
;;       (focus-window (first (screen-mapped-windows screen))))))

(defun geometry-hints (screen win)
  "Return hints for max width and height and increment hints. These
hints have been modified to always be defined and never be greater
than the root window's width and height."
  (let* ((f (frame-data screen (window-frame screen win)))
	 (x (frame-x f))
	 (y (frame-y f))
	 (width (- (frame-width f) (* 2 (xlib:drawable-border-width win))))
	 (height (- (frame-height f)
		    (* 2 (xlib:drawable-border-width win))))
	 (inc-x 1)
	 (inc-y 1)
	 (hints (xlib:wm-normal-hints win))
	 (hints-width (xlib:wm-size-hints-max-width hints))
	 (hints-height (xlib:wm-size-hints-max-height hints))
	 (hints-inc-x (xlib:wm-size-hints-width-inc hints))
	 (hints-inc-y (xlib:wm-size-hints-height-inc hints)))
    ;; Adjust the defaults if the window is a transient_for window.
    (when (xlib:get-property win :WM_TRANSIENT_FOR)
      (setf x (truncate (- (/ width 2)
			   (/ (xlib:drawable-width win) 2)))
	    y (truncate (- (/ height 2)
			   (/ (xlib:drawable-height win) 2)))
	    width (xlib:drawable-width win)
	    height (xlib:drawable-height win)))
    ;; Update our defaults if the window has the hints
    (when (and hints-width
	       (< hints-width width))
      (setf width hints-width))
    (when (and hints-height
	       (< hints-height height))
      (setf height hints-height))
    (when hints-inc-x
      (setf inc-x hints-inc-x))
    (when hints-inc-y
      (setf inc-y hints-inc-y))
    ;; Now return our findings as a list
    ;; FIXME: use values
    (list x y width height inc-x inc-y)))

(defun grab-keys-on-window (win)
  (xlib:grab-key win (xlib:keysym->keycodes *display* *prefix-key*)
		 :modifiers *prefix-modifiers* :owner-p t
		 :sync-pointer-p nil :sync-keyboard-p nil))

(defun ungrab-keys-on-window (win)
  (xlib:ungrab-key win :any :modifiers '(:any)))

(defun add-window (screen window)
  "add window to the head of the mapped-windows list."
  ;(assert (not (member window (screen-mapped-windows screen))))
  (push window (screen-mapped-windows screen))
  ;; Create the window-table entry, adding it's number
  (let ((num (find-free-window-number screen)))
    (setf (gethash window (screen-window-hash screen)) (make-hash-table))
    (setf (gethash :number (gethash window (screen-window-hash screen))) num)
    (setf (window-frame screen window) (screen-current-frame screen))
    (setf (frame-window (frame-data screen (screen-current-frame screen)))
	  window)))
    

(defun reparent-window (screen window)
  (let ((master-window (xlib:create-window
			:parent (xlib:screen-root (screen-number screen))
			:x (xlib:drawable-x window)
			:y (xlib:drawable-y window)
			:width (xlib:drawable-width window)
			:height (xlib:drawable-height window)
			:background (xlib:screen-white-pixel (screen-number screen))
			:border-width 5
			:event-mask (xlib:make-event-mask
				     :substructure-notify))))
    (xlib:reparent-window window master-window 2 2)
    (xlib:map-window master-window)
    (xlib:map-subwindows master-window)))

(defun absorb-mapped-window (screen window)
  "Add the window to the screen's mapped window list and process it as
needed."
  (add-window screen window)
  (maximize-window window)
  ;(reparent-window screen window)
  (xlib:map-window window)
  (grab-keys-on-window window)
  ;; Run the map window hook on it
  (run-hook-with-args *map-window-hook* window))

(defun remove-window (screen window)
  "Remove the window from the list of mapped windows and, possibly,
give the last accessed window focus."
  ;; Remove the window from the list of mapped windows.
  (when (member window (screen-mapped-windows screen))
    (setf (screen-mapped-windows screen)
	  (delete window (screen-mapped-windows screen)))
    (let ((f (frame-data screen (window-frame screen window))))
      ;; Clean up the window's entry in screen-window-hash
      (remhash window (screen-window-hash screen))
      ;; remove it from it's frame structures
      (when (xlib:window-equal (frame-window f) window)
	(setf (frame-window f) (first (frame-windows screen f))))
      ;; Run the unmap hook on the window
      (run-hook-with-args *unmap-window-hook* window)
      ;; If the current window was removed, then refocus the frame it
      ;; was in, since it has a new current window
      (when (= (screen-current-frame screen)
	       (frame-number f))
	(focus-frame screen (frame-number f))))))
  
(defun move-window-to-head (screen window)
  "Move window to the head of the mapped-windows list."
  ;(assert (member window (screen-mapped-windows screen)))
  (setf (screen-mapped-windows screen) (delete window (screen-mapped-windows screen)))
  (push window (screen-mapped-windows screen)))

(defun no-focus (screen)
  "don't focus any window but still read keyboard events."
  (pprint 'no-focus)
  (xlib:set-input-focus *display* (screen-focus-window screen) :POINTER-ROOT))

(defun focus-window (window)
  "Give the window focus. This means the window will be visible,
maximized, and given focus."
  (let ((screen (window-screen window)))
    ;(master-window (window-master window)))
    ;(setf (xlib:window-priority master-window) :top-if)
    (setf (xlib:window-priority window) :top-if)
    (xlib:set-input-focus *display* window :POINTER-ROOT)
    ;; Move the window to the head of the mapped-windows list
    (move-window-to-head screen window)
    ;; If another window was focused, then call the unfocus hook for
    ;; it.
    (when (screen-current-window screen)
      (run-hook-with-args *unfocus-window-hook* (screen-current-window screen)))
      ;(hide-window (screen-current-window screen)))
;;     (when (not (xlib:window-equal (frame-window (frame-data screen
;; 							    (screen-current-frame screen)))
;; 				  window))
;;       (setf (frame-window (frame-data screen (screen-current-frame screen))) window)
;;       ;; We only need to maximize the windo if it used to be in a
;;       ;; different frame.
;;       (if (not (eq (window-frame screen window)
;; 		   (screen-current-frame screen)))
;; 	  (maximize-window window)))
    ;(unhide-window window)
    (run-hook-with-args *focus-window-hook* window)))

(defun delete-window (window)
  "Send a delete event to the window."
  (pprint '(delete window))
  (xlib:send-event window
		   :client-message nil
		   :window window
		   :type :WM_PROTOCOLS
		   :format 32
		   :data (list +wm-delete-window+)))


;;; Message printing functions 

(defun create-message-window-gcontext (screen)
  "Create a graphic context suitable for printing characters."
  (xlib:create-gcontext :drawable (screen-message-window screen)
			:font (screen-font screen)
			:foreground
			(xlib:screen-white-pixel (screen-number screen))
			:background
			(xlib:screen-black-pixel (screen-number screen))))

(defun max-width (font l)
  "Return the width of the longest string in L using FONT."
  (loop for i in l
	maximize (xlib:text-width font i)))

(defun setup-message-window (screen l)
  (let ((height (* (length l)
		   (+ (xlib:font-ascent (screen-font screen))
		      (xlib:font-descent (screen-font screen)))))
	(width (max-width (screen-font screen) l))
	(screen-width (xlib:drawable-width (xlib:screen-root (screen-number screen))))
	(win (screen-message-window screen)))
    ;; Now that we know the dimensions, raise and resize it.
    (xlib:map-window (screen-message-window screen))
    (setf (xlib:drawable-y win) 0
	  (xlib:drawable-height win) height
	  (xlib:drawable-x win) (- screen-width width
				   (* (xlib:drawable-border-width win) 2)
				   (* *message-window-padding* 2))
	  (xlib:drawable-width win) (+ width (* *message-window-padding* 2))
	  (xlib:window-priority win) :above)
    ;; Clear the window
    (xlib:clear-area win)))

(defun invert-rect (screen win x y width height)
  "invert the color in the rectangular area. Used for highlighting text."
  (let ((gcontext (xlib:create-gcontext :drawable win
					:foreground
					(xlib:screen-white-pixel (screen-number screen))
					:function boole-xor)))
    (xlib:draw-rectangle win gcontext x y width height t)))


;;; Frame functions

(defun frame-raise-window (s f w)
  "Raise the window w in frame f in screen s."
  (assert (= (window-frame s w) f))
  (setf (frame-window (frame-data s f)) w)
  (focus-frame s f))
  

(defun focus-frame (screen f)
  (let ((w (frame-window (frame-data screen f))))
    (setf (screen-current-frame screen) f)
    (pprint (frame-data screen f))
    (if w
	(focus-window w)
      (no-focus screen))))

(defun frame-data (screen f)
  "Return the data associated with frame F."
  (gethash f (screen-frame-hash screen)))

(defun set-frame-data (screen fnum fdata)
  (setf (gethash fnum (screen-frame-hash screen)) fdata))

(defun rm-frame-data (screen fnum)
  (remhash fnum (screen-frame-hash screen)))

(defsetf frame-data set-frame-data)

(defun frame-windows (screen f)
  (remove-if-not (lambda (w) (= (window-frame screen w)
				(frame-number f)))
		 (screen-mapped-windows screen)))

(defun frame-sort-windows (screen f)
  (remove-if-not (lambda (w) (= (window-frame screen w)
				(frame-number f)))
		 (sort-windows screen)))

(defun make-frame-hash (w h)
  "Used to create an initial frame hash for a screen."
  (let ((frame (make-frame :number 0
		     :x 0
		     :y 0
		     :width w
		     :height h
		     :window nil))
	(hash (make-hash-table)))
    ;;Create initial frame hashtable
    (setf (gethash 0 hash) frame)
    hash))

(defun find-free-frame-number (screen)
  (find-free-number (mapcar-hash (lambda (f) (frame-number f))
				 (screen-frame-hash screen))))

(defun split-frame-h (screen p)
  "Return 2 new frames. The first one stealing P's number and window"
  (let* ((w (truncate (/ (frame-width p) 2)))
	 (h (frame-height p))
	 (f1 (make-frame :number (frame-number p)
			 :x (frame-x p)
			 :y (frame-y p)
			 :width w
			 :height h
			 :window (frame-window p)))
	 (f2 (make-frame :number (find-free-frame-number screen)
			 :x (+ (frame-x p) w)
			 :y (frame-y p)
			 :width w
			 :height h
			 :window nil)))
    (values f1 f2)))

(defun split-frame-v (screen p)
  "Return 2 new frames. The first one stealing P's number and window"
  (let* ((w (frame-width p))
	 (h (truncate (/ (frame-height p) 2)))
	 (f1 (make-frame :number (frame-number p)
			 :x (frame-x p)
			 :y (frame-y p)
			 :width w
			 :height h
			 :window (frame-window p)))
	 (f2 (make-frame :number (find-free-frame-number screen)
			 :x (frame-x p)
			 :y (+ (frame-x p) h)
			 :width w
			 :height h
			 :window nil)))
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

(defun migrate-frame-windows (screen src dest)
  "Migrate all windows in SRC frame to DEST frame."
  (mapc (lambda (w)
	  (when (eq (window-frame screen w) src)
	    (setf (window-frame screen w) dest)))
	(screen-mapped-windows screen)))

(defun tree-accum-fn (tree acc fn)
  "Run an accumulator function on fn applied to each leaf"
  (cond ((null tree) nil)
	((atom tree)
	 (funcall fn tree))
	(t (funcall acc
		    (tree-accum-fn (first tree) acc fn)
		    (tree-accum-fn (second tree) acc fn)))))

(defun tree-x (screen tree)
  (tree-accum-fn tree #'min (lambda (n)
			      (let ((f (frame-data screen n)))
				(frame-x f)))))
(defun tree-y (screen tree)
  (tree-accum-fn tree #'min (lambda (n)
			      (let ((f (frame-data screen n)))
				(frame-y f)))))

(defun tree-width (screen tree)
  (tree-accum-fn tree #'+ (lambda (n)
			    (let ((f (frame-data screen n)))
			      (frame-width f)))))

(defun tree-height (screen tree)
  (tree-accum-fn tree #'+ (lambda (n)
			    (let ((f (frame-data screen n)))
			      (frame-height f)))))

(defun tree-row-split (screen tree)
  "Return t if the children of tree are stacked vertically"
  (= (tree-y screen (first tree)) (tree-y screen (second tree))))

(defun tree-column-split (screen tree)
  "Return t if the children of tree are side-by-side"
  (= (tree-x screen (first tree)) (tree-x screen (second tree))))

(defun expand-frame (f amount dir)
  (case dir
    ('left (decf (frame-x f) amount)
	   (incf (frame-width f) amount))
    ('right (incf (frame-width f) amount))    
    ('top (decf (frame-y f) amount)
	  (incf (frame-height f) amount))
    ('bottom (incf (frame-height f) amount))))

(defun expand-tree (screen tree amount dir)
  "expand the frames in tree by AMOUNT in DIR direction. DIR can be 'up 'down 'left 'right"
  (cond ((null tree) nil)
	((atom tree)
	 (let ((f (frame-data screen tree)))
	   (expand-frame f amount dir)))
	 (t (if (or (and (member dir '(left right))
			 (tree-row-split screen tree))
		    (and (member dir '(top bottom))
			 (tree-column-split screen tree)))
		    (progn
		      (expand-tree screen (first tree) amount dir)
		      (expand-tree screen (second tree) amount dir))
		  (let ((n (truncate (/ amount 2))))
		    (expand-tree screen (first tree) n dir)
		    (expand-tree screen (second tree) (- amount n) dir))))))

(defun join-subtrees (screen tree keep)
  "expand one of the children of tree to occupy the space of the other
child. KEEP decides which child to keep. It can be 'LEFT or
'RIGHT. Return the child that was kept."
  (let ((child (if (eql keep 'left)
		   (first tree)
		 (second tree)))
	dir amount)
    (if (tree-row-split screen tree)
	(progn
	  (setf amount (tree-width screen child))
	  (if (eql keep 'left)
	      (setf dir 'right)
	    (setf dir 'left)))
      (progn
	(setf amount (tree-height screen child))
	(if (eql keep 'left)
	    (setf dir 'bottom)
	  (setf dir 'top))))
    (expand-tree screen child amount dir)
    child))

(defun remove-frame (screen tree leaf)
  "Return a new tree with LEAF and it's sibling merged into
one. Alters frame-data for the frames affected."
  (cond ((atom tree) tree)
	((and (atom (first tree))
	      (eq (first tree) leaf))
	 (prog1
	     (join-subtrees screen tree 'right)
	   (rm-frame-data screen (first tree))))
	((and (atom (second tree))
	      (eq (second tree) leaf))
	 (prog1
	     (join-subtrees screen tree 'left)
	   (rm-frame-data screen (second tree))))
	(t (list (remove-frame screen (first tree) leaf)
		 (remove-frame screen (second tree) leaf)))))

(defun sync-frame-windows (screen frame)
  "synchronize windows attached to FRAME."
  (mapc (lambda (w)
	  (when (eq (window-frame screen w) frame)
	    (maximize-window w)))
	(screen-mapped-windows screen)))

(defun split-frame (screen how-fn)
  (let* ((frame (frame-data screen (screen-current-frame screen))))
    (multiple-value-bind (f1 f2) (funcall how-fn frame)
      (setf (gethash (frame-number f1) (screen-frame-hash screen)) f1
	    (gethash (frame-number f2) (screen-frame-hash screen)) f2
	    (screen-frame-tree screen) (replace-frame-in-tree (screen-frame-tree screen)
							      (frame-number frame)
							      (frame-number f1)
							      (frame-number f2)))
      (sync-frame-windows screen (frame-number f1))
      (sync-frame-windows screen (frame-number f2)))))
    
(defun draw-frame-numbers (screen)
  "Draw the number of each frame in its corner. Return the list of
windows used to draw the numbers in. The caller must destroy them."
  (mapcar-hash (lambda (f)
		 (let ((w (xlib:create-window
			   :parent (xlib:screen-root (screen-number screen))
			   :x (frame-x f) :y (frame-y f) :width 1 :height 1
			   :background (xlib:screen-white-pixel (screen-number screen))
			   :border (xlib:screen-white-pixel (screen-number screen))
			   :border-width 1
			   :event-mask '())))
		   (xlib:map-window w)
		   (setf (xlib:window-priority w) :above)
		   (echo-in-window w (screen-font screen)
				   (xlib:screen-white-pixel (screen-number screen))
				   (xlib:screen-black-pixel (screen-number screen))
				   (format nil "~A" (frame-number f)))
		   (xlib:display-force-output *display*)
		   (pprint (list 'mapped (frame-number f)))
		   w))
	       (screen-frame-hash screen)))
	    

;;; Screen functions

(defun internal-window-p (screen win)
  "Return t if win is a window used by stumpwm"
  (or (xlib:window-equal (screen-focus-window screen) win)))

(defun screen-current-window (screen)
  (frame-window (frame-data screen (screen-current-frame screen))))

(defun unmap-message-window (screen)
  "Unmap the screen's message window, if it is mapped."
  (unless (eq (xlib:window-map-state (screen-message-window screen)) :unmapped)
    (xlib:unmap-window (screen-message-window screen))))

(defun unmap-all-message-windows ()
  (mapc #'unmap-message-window *screen-list*))

(defun echo-in-window (win font fg bg string)
  (let* ((height (+ (xlib:font-descent font)
		    (xlib:font-ascent font)))
	 (gcontext (xlib:create-gcontext :drawable win
					 :font font
					 :foreground fg
					 :background bg))
	 (width (xlib:text-width font string)))
    (setf (xlib:drawable-height win) height
	  (xlib:drawable-width win) width)
    (xlib:clear-area win)
    (xlib:draw-image-glyphs win gcontext 0 (xlib:font-ascent font) string)))
	 
(defun echo-string-list (screen strings &optional highlight)
  "draw each string in l in the screen's message window. HIGHLIGHT is
the nth entry to highlight."
  (let* ((height (+ (xlib:font-descent (screen-font screen))
		    (xlib:font-ascent (screen-font screen))))
	 (gcontext (create-message-window-gcontext screen))
	 (message-win (screen-message-window screen)))
    (setup-message-window screen strings)
    (loop for s in strings
	  ;; We need this so we can track the row for each element
	  for i from 0 to (length strings)
	  do (xlib:draw-image-glyphs message-win gcontext
				     *message-window-padding*
				     (+ (* i height)
					(xlib:font-ascent (screen-font screen)))
				     s)
	  when (and highlight
		    (= highlight i))
	  do (invert-rect screen message-win
			  0 (* i height)
			  (xlib:drawable-width message-win)
			  height)))
  ;; Set a timer to hide the message after a number of seconds
  (reset-timeout))

(defun echo-string (screen msg)
  "Print msg to SCREEN's message window."
  (echo-string-list screen (list msg)))

(defun current-screen ()
  "Return the current screen. The current screen is the screen whose
window has focus. If no window has focus it is the screen that last had
focus of a window."
  (let* ((win (xlib:input-focus *display*))
	 (screen (window-screen win)))
    ;; We MUST be able to figure out the current screen by this method
    (assert screen)
    ;; Return the current screen
    screen))

(defun init-screen (screen-number)
  "Given a screen number, returns a screen structure with initialized members"
  ;; Listen for the window manager events on the root window
  (setf (xlib:window-event-mask (xlib:screen-root screen-number))
	'(:substructure-redirect
	  :substructure-notify
	  :property-change))
  (xlib:display-force-output *display*)
  ;; Grab the prefix key for the root window
  (grab-keys-on-window (xlib:screen-root screen-number))
  ;; Initialize the screen structure
  (let* ((white (xlib:screen-white-pixel screen-number))
	 (black (xlib:screen-black-pixel screen-number))
	 (input-window (xlib:create-window :parent (xlib:screen-root screen-number)
					   :x 0 :y 0 :width 20 :height 20
					   :background black
					   :border white
					   :border-width 1
					   :colormap (xlib:screen-default-colormap
						      screen-number)
					   :event-mask '(:key-press)))
	 (focus-window (xlib:create-window :parent (xlib:screen-root screen-number)
					   :x 0 :y 0 :width 1 :height 1))
	 (frame-window (xlib:create-window :parent (xlib:screen-root screen-number)
					   :x 0 :y 0 :width 1 :height 1
					   :background white
					   :border white
					   :border-width 0
					   :colormap (xlib:screen-default-colormap
						      screen-number)
					   :event-mask '()))
	 (message-window (xlib:create-window :parent (xlib:screen-root screen-number)
					     :x 0 :y 0 :width 1 :height 1
					     :background black
					     :border white
					     :border-width 1
					     :colormap (xlib:screen-default-colormap
							screen-number)
					     :event-mask '())))
    ;; Create our screen structure
    ;; The focus window is mapped at all times
    (xlib:map-window focus-window)
    (grab-keys-on-window focus-window)
    (make-screen :number screen-number
		 :frame-tree 0
		 :frame-hash (make-frame-hash (xlib:screen-width screen-number)
					      (xlib:screen-height screen-number))
		 :font (xlib:open-font *display* *font-name*)
		 :current-frame 0
		 :frame-hash (make-hash-table)
		 :window-hash (make-hash-table)
		 :message-window message-window
		 :input-window input-window
		 :frame-window frame-window
		 :focus-window focus-window)))

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
    (xlib:grab-pointer (xlib:screen-root (screen-number screen)) nil :owner-p nil
		       :cursor cursor)))

(defun ungrab-pointer ()
  "Remove the grab on the cursor and restore the cursor shape."
  (xlib:ungrab-pointer *display*))

(defun grab-keyboard (screen)
  (xlib:grab-keyboard (xlib:screen-root (screen-number screen)) :owner-p nil
		      :sync-keyboard-p nil :sync-pointer-p nil))

(defun ungrab-keyboard ()
  (xlib:ungrab-keyboard *display*))

(defun warp-pointer (screen x y)
  "Move the pointer to the specified location."
  (let ((root (xlib:screen-root (screen-number screen))))
    (xlib:warp-pointer root x y)))


;; Event handler functions

(defparameter *event-fn-table* (make-hash-table)
  "A hash of event types to functions")

(defmacro define-stump-event-handler (event keys &body body)
  (let ((fn-name (gensym)))
  `(labels ((,fn-name (&rest event-slots &key ,@keys &allow-other-keys)
		      (declare (ignorable event-slots))
		      ,@body))
     (setf (gethash ,event *event-fn-table*) #',fn-name))))


;(define-stump-event-handler :map-notify (event-window window override-redirect-p)
;  )

(define-stump-event-handler :configure-request (stack-mode parent window above-sibling x y width height border-width value-mask)
  ;; Grant the configure request but then maximize the window after the granting.
  (declare (ignorable above-sibling))
  (declare (ignorable parent))
  (declare (ignorable stack-mode))
  (pprint value-mask)
  (labels ((has-x (mask) (= 1 (logand mask 1)))
	   (has-y (mask) (= 2 (logand mask 2)))
	   (has-w (mask) (= 4 (logand mask 4)))
	   (has-h (mask) (= 8 (logand mask 8)))
	   (has-bw (mask) (= 16 (logand mask 16)))
    	   (has-stackmode (mask) (= 64 (logand mask 64))))
    (let ((screen (window-screen window)))
      (xlib:with-state (window)
		       (pprint value-mask)
		       (when (has-x value-mask)
			 (pprint 'x)
			 (setf (xlib:drawable-x window) x))
		       (when (has-y value-mask)
			 (pprint 'y)
			 (setf (xlib:drawable-y window) y))
		       (when (has-h value-mask)
			 (pprint 'h)
			 (setf (xlib:drawable-height window) height))
		       (when (has-w value-mask)
			 (pprint 'w)
			 (setf (xlib:drawable-width window) width))
		       (when (has-bw value-mask)
			 (pprint 'bw)
			 (setf (xlib:drawable-border-width window) border-width)))
      ;; TODO: are we ICCCM compliant?
      ;; Make sure that goes to the client
      (xlib:display-force-output *display*)
      ;; After honouring the request, maximize it
      (when (member window (screen-mapped-windows screen))
	(maximize-window window)
	;; The ICCCM says with have to send a fake configure-notify if
	;; the window is moved but not resized.
	(unless (or (logbitp 2 value-mask) (logbitp 3 value-mask))
	  (send-configuration-notify window))
	;; Finally, grant the stack-mode change (if it's mapped)
	(when (has-stackmode value-mask)
	  (case stack-mode
	    (:above
	     (let ((f (window-frame screen window)))
	       (frame-raise-window screen f window)))))))))

(define-stump-event-handler :map-request (parent window)
  (declare (ignorable parent))
  (let ((screen (window-screen window)))
    (process-new-window window)
    (absorb-mapped-window screen window)
    ;; Give it focus
    (frame-raise-window screen (window-frame screen window) window)))

(define-stump-event-handler :unmap-notify (send-event-p event-window window configure-p)
  (declare (ignorable configure-p))
  (unless (or send-event-p
	      (xlib:window-equal window event-window))
    ;; There are two kinds of unmap notify events: the straight up
    ;; ones where event-window and window are the same, and
    ;; substructure unmap events when the event-window is the parent
    ;; of window. So use event-window to find the screen.
    (let ((screen (window-screen event-window)))
      (remove-window screen window))))


(define-stump-event-handler :create-notify (parent window x y width height border-width override-redirect-p)
  (declare (ignorable border-width))
  (declare (ignorable width))
  (declare (ignorable height))
  (declare (ignorable x))
  (declare (ignorable y))
  (declare (ignorable parent))
  (unless override-redirect-p))
;    (process-new-window window)
;    (run-hook-with-args *new-window-hook* window)))


(define-stump-event-handler :destroy-notify (send-event-p event-window window)
  (unless (or send-event-p
	      (xlib:window-equal event-window window))
    ;; Ignore structure destroy notifies and only
    ;; use substructure destroy notifiers. This way
    ;; event-window is the window's parent.
    (let ((screen (window-screen event-window)))
      ;; In some cases, we get a destroy notify before an unmap
      ;; notify, so simulate an unmap notify (for now).
      (remove-window screen window)
      ;; Destroy the master window
      ;(xlib:destroy-window event-window)
      (run-hook-with-args *destroy-window-hook* window))))

(defun handle-command-key (screen code state)
  "Find the command mapped to the (code state) and executed it."
  (let* ((key (keycode->character code (xlib:make-state-keys state)))
	 (fn (gethash key *key-bindings*)))
    (pprint (list key state))
    ;(pprint (cook-keycode code state))
    (pprint fn)
    (if (null fn)
	(pprint '(no match))
      (progn
	(pprint '(found it))
	(funcall fn screen)))))

(define-stump-event-handler :key-press (code state window root)
  (declare (ignorable window))
  ;; FIXME: maybe we should verify that code and state are what we
  ;; expect them to be (C-t).
  (declare (ignorable code))
  (declare (ignorable state))
  (let ((screen (find-screen root)))
    (unmap-message-window screen)
    ;; grab the keyboard
    (grab-pointer screen)
    (grab-keyboard screen)
    (pprint '(awaiting command key))
    ;; Listen for key
    (let ((key (do ((k (read-key) (read-key)))
		   ((not (is-modifier (xlib:keycode->keysym *display* (car k) 0))) k))))
      (pprint '(handling command))
      ;; We've read our key, so we can release the keyboard.
      (ungrab-pointer)
      (ungrab-keyboard)
      (xlib:display-force-output *display*)
      (handle-command-key screen (car key) (cdr key)))))

(defun handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignorable display))
  (pprint (list 'handling 'event event-key))
  (let ((eventfn (gethash event-key *event-fn-table*)))
    (when eventfn
      (apply eventfn event-slots))
    t))
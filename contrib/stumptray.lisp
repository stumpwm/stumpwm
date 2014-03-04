;;;; System Tray for stumpwm.

;;;; Note: To run this you need to patch stumpwm to add
;;;; stumpwm:*event-processing-hook*
;;;; TODO Do something about the errors that happen when resuming from
;;;; hibernation

;;;; TODO Modify the modeline command or add a wrapper to start the
;;;; tray together with the modeline

(asdf:compute-source-registry)
(declaim (optimize (speed 0) (debug 3) (safety 3)))
(require :xembed)
(require :alexandria)

(defpackage :stumptray
  (:use #:cl #:alexandria)
  (:export *tray-viwin-background*
           *tray-hiwin-background*

           add-mode-line-hooks
           remove-mode-line-hooks
           ))

(in-package :stumptray)

(defstruct (tray)
  win 		  ;; Main window
  sowin 	  ;; Selection Owner Window (required by the tray protocol)
  fpwin 	  ;; Focus proxy window (required by the xembed protocol)
  viwin 	  ;; Visible Icons container
  hiwin 	  ;; Hidden icons container 
  curwin  	  ;; Icon cursor window
  curpos          ;; Icon cursor position (icon index)
  icon-height     ;; Icon height
  show-hiwin-p    ;; Whether hidden icons are visible or not
  vicons	  ;; visible icon list
  hicons	  ;; hidden icon list
  vicons-ordering ;; Visible icons ordering information
  hicons-ordering ;; Hidden icons ordering information
  event-processing-fn ;; Hook function added to stumpwm:*event-processing-hook* to process tray events
  display
  screen
  mode-line)

;;; Event masks

;; main window
(defparameter +WIN-EVENT-MASK+
  (xlib:make-event-mask :property-change :key-press :key-release))
;; Focus proxy window
(defparameter +FPWIN-EVENT-MASK+
  (xlib:make-event-mask :property-change :key-press :key-release))
;; Selection owner window 
(defparameter +SOWIN-EVENT-MASK+
  (xlib:make-event-mask))
;; visible icons container
(defparameter +VIWIN-EVENT-MASK+
  (xlib:make-event-mask :property-change))
;; Hidden icons container
(defparameter +HIWIN-EVENT-MASK+
  (xlib:make-event-mask :property-change))

;;; Some functions to locate trays and stuff
;;; The FDO protocol specifies a single treay for each screen
(defvar *screen-tray-table*
  (make-hash-table))
(defvar *tray-head-selection-fn* #'first
  "A funcion that takes a list of heads - all relative to the same
screen - and returns the head that will contain the tray. Selects the
first on the list by default.")

(defmacro screen-tray (screen)
  `(gethash ,screen *screen-tray-table*))

(defun screen-mode-line (screen)
  "Returns the mode-line that will contain the tray for SCREEN."
  (stumpwm::head-mode-line 
   (funcall *tray-head-selection-fn* 
	    (stumpwm::screen-heads screen))))

(defun current-tray ()
  (screen-tray (stumpwm:current-screen)))

;;;; Tray appearance
;;; Dimensions The tray height depends on the modeline height. In
;;; stumpwm there is a modeline for every X screen.
(defvar *tray-cursor-thickness* 2)
(defvar *tray-cursor-icon-distance* 1)

(defun screen-tray-height (screen)
  "Calculates the total tray height based on the height of the
SCREEN's modeline."
  (xlib:drawable-height
   (stumpwm::mode-line-window (screen-mode-line screen))))
(defun screen-tray-icon-height (screen)
  "Calculates the height of the icons embedded in the tray based on
the height of the SCREEN's modeline."
  (- (screen-tray-height screen)
     *tray-cursor-thickness*
     *tray-cursor-icon-distance*))

(defun tray-height (tray)
  "Returns the TRAY height."
  (screen-tray-height (tray-screen tray)))

(defun tray-icon-height (tray)
  "Returns the height of the icons embedded in the TRAY."
  (screen-tray-icon-height (tray-screen tray)))

(defun tray-width (tray)
  "Calculates the total width of the TRAY."
  (+ (xlib:drawable-width (tray-viwin tray))
     (if (tray-show-hiwin-p tray)
	 (xlib:drawable-width (tray-hiwin tray))
	 0)))
(defun tray-position-right (tray)
  "Calculates the position of the TRAY in the parent window coordinates.
The tray is aligned right."
  (- (xlib:drawable-width (xembed:window-parent (tray-win tray)))
     (tray-width tray)))

(defvar *tray-position-function* #'tray-position-right
  "The function used to calculate the tray position in the parent
window coordinates.")

;; Colors
(defparameter *tray-win-background* (nth 7 stumpwm:*colors*)
  "Tray main container window background color.")
(defparameter *tray-viwin-background* stumpwm:*mode-line-background-color*
  "Tray visible icons container window color.")
(defparameter *tray-hiwin-background* stumpwm:*mode-line-border-color*
  "Tray hidden icons container window color.")
(defparameter *tray-cursor-color* (nth 2 stumpwm:*colors*)
  "Tray icon selection cursor color.")

;;; Sorting and hiding
(defvar *tray-hidden-classes* nil
  "A list of window classes that will be hidden in the tray.")
   
;;; Tray creation and destruction
(defun create-tray (screen mode-line &key (x 0) (y 0))
  "Creates a tray object given the SCREEN and the MODE-LINE window. The
  tray object needs further initialization, see `tray-init'."
  (let* ((parent (stumpwm::mode-line-window mode-line))
         (root-window (xlib:drawable-root parent))
	 (depth (xlib:drawable-depth root-window))
	 (icon-height (screen-tray-icon-height screen))
	 (tray-height (screen-tray-height screen))
	 (win (xlib:create-window :parent parent
                                  :x x
                                  :y y
                                  :depth depth
                                  :width icon-height
                                  :height tray-height
                                  :background (xlib:alloc-color (xlib:window-colormap root-window)
                                                               (stumpwm:lookup-color 
                                                                (stumpwm:current-screen) *tray-win-background*)) 
                                  :event-mask +WIN-EVENT-MASK+)))
    (flet ((create-1x1-invisible-window (event-mask)
	     (xlib:create-window :parent win
                                 :x -1 :y -1 :width 1 :height 1
                                 :event-mask event-mask))
	   (create-visible-win (bgcolor event-mask x y)
	     (xlib:create-window :parent win :depth depth
				 :x x :y y :width icon-height :height tray-height
                                 :event-mask event-mask
				 :background (xlib:alloc-color (xlib:window-colormap root-window)
                                                               (stumpwm:lookup-color 
                                                                (stumpwm:current-screen) bgcolor)))))
      (let* ((fpwin (create-1x1-invisible-window +FPWIN-EVENT-MASK+))
             (sowin (create-1x1-invisible-window +SOWIN-EVENT-MASK+))
             (viwin (create-visible-win *tray-viwin-background* +VIWIN-EVENT-MASK+ icon-height 0))
             (hiwin (create-visible-win *tray-hiwin-background* +HIWIN-EVENT-MASK+ 0 0))
             (curwin (create-visible-win *tray-cursor-color* (xlib:make-event-mask) 0 0)))
        (make-tray :win win :sowin sowin :fpwin fpwin
                   :viwin viwin :hiwin hiwin :curwin curwin
                   :curpos nil :icon-height icon-height
                   :display (xlib:drawable-display root-window) :screen screen 
                   :mode-line mode-line)))))

;;TODO: We should also remove the hook somewhere
(defun destroy-tray (tray)
  "Destroys and de-initializes a tray object."
  (stumpwm:remove-hook stumpwm:*event-processing-hook* (tray-event-processing-fn tray))
  (dolist (socket (tray-vicons tray))
    (ignore-errors (xembed:destroy-socket socket)))
  (dolist (socket (tray-hicons tray))
    (ignore-errors (xembed:destroy-socket socket)))
  (xlib:destroy-window (tray-win tray))
  (setf (screen-tray (tray-screen tray)) nil))

;;; Tray mapping
(defun map-tray (tray) 
  "Maps the tray and its subwindows according to the tray state."
  (mapcar #'xlib:map-window
	  (list (tray-win tray)
		(tray-fpwin tray)
		(tray-sowin tray)
		(tray-viwin tray)))
  (xembed:update-timestamp (tray-fpwin tray))
  (cond ((tray-curpos tray)
	 (xlib:map-window (tray-curwin tray)))
	(t (xlib:unmap-window (tray-curwin tray))))
  (if (tray-show-hiwin-p tray)
      (xlib:map-window (tray-hiwin tray))
      (xlib:unmap-window (tray-hiwin tray))))

;;;; Icon window management
;;; Helpers 
(defun window-wm-class (window)
  "Returns the WM class of WINDOW."
  (second (multiple-value-list (xlib:get-wm-class window))))

(defun client-wm-class (icon-socket)
  "Returns the class of the window embedded in ICON-SOCKET."
  (window-wm-class (xembed:client icon-socket)))

(defun icon-visibility (icon &optional (socket-p t))
  "Returns T if ICON should be visible, NIL otherwise.
The window passed to this function can be the icon window itself or
its embedder socket if SOCKET-P is T."
  (not (member (window-wm-class (if socket-p
				   (xembed:client icon)
				   icon))
	      *tray-hidden-classes* :test #'equalp)))
    
(defun tray-icons (tray visibility)
  "Returns the appropriate TRAY icon list depending on VISIBILTY."
  (if visibility
    (tray-vicons tray)
    (tray-hicons tray)))

(defmethod (setf tray-icons) (value tray visibility)
  (if visibility
    (setf (tray-vicons tray) value)
    (setf (tray-hicons tray) value)))

(defun tray-ordering (tray visibility)
  "Returns the appropriate TRAY ordering data depending on VISIBILITY."
  (if visibility
    (tray-vicons-ordering tray)
    (tray-hicons-ordering tray)))

(defmethod (setf tray-ordering) (value tray visibility)
  (if visibility
    (setf (tray-vicons-ordering tray) value)
    (setf (tray-hicons-ordering tray) value)))

(defun tray-icon-container (tray visibility)
  "Returns the appropriate TRAY icon container window depending on VISIBILITY."
  (if visibility
      (tray-viwin tray)
      (tray-hiwin tray)))

(defun icon-at-pos (tray pos)
  "Returns the icon socket at position POS in the TRAY. The position
is intended left to right, starting from the first hidden icons and
ending to the last visible icon."
  (elt (append (tray-hicons tray) (tray-vicons tray)) pos))
	 
  
;;; XXX improve this function and use icon-at-pos
(defun icon-at-cursor (tray)
  "Returns the selected icon socket (the one over the cursor)."
  (let* ((vcard (length (tray-vicons tray)))
	 (hcard (length (tray-hicons tray)))
	 (pos (mod (tray-curpos tray) (+ hcard vcard))))
    (setf (tray-curpos tray) pos)
    (cond ((zerop (+ hcard vcard)) nil)
	  ((>= pos hcard)
	   (elt (tray-vicons tray) (- pos hcard)))
	  (t (elt (tray-hicons tray) pos)))))

(defun icon->pos (tray icon)
  "Returns the position of ICON socket on TRAY, as intended in
`icon-at-pos'."
  (position icon (append (tray-hicons tray) (tray-vicons tray))
	    :test #'xlib:window-equal))

;;; Xembed sockets
(defun make-icon-socket (tray parent)
  "Creates and returns an xembed socket"
  (let ((root (xlib:drawable-root parent))
	(icon-height (tray-icon-height tray)))
    (xembed:create-socket nil :parent parent :depth (xlib:drawable-depth root)
			  :background :PARENT-RELATIVE
			  :x 0 :y 0
			  :width icon-height :height icon-height)))

(defun initialize-icon-socket (icon-socket)
  "Maps and activates the socket."
  (xlib:map-window icon-socket)
  (xlib:map-subwindows icon-socket)
  (xembed:socket-activate icon-socket))

;;; Adding icons 
(defun tray-update-icon-data (tray socket visibility)
  "Adds the socket to the appropriate icon list and updates the
ordering data."
  (push socket (tray-icons tray visibility))
  (pushnew (client-wm-class socket)
	   (tray-ordering tray visibility)))

(defun add-icon (tray icon-id)
  "Adds an icon with window id ICON-ID to TRAY, and starts the XEMBED
protocol."
  (let* ((icon (xlib::lookup-window (tray-display tray) icon-id))
	 (icon-visibility (icon-visibility icon nil))
	 (socket (make-icon-socket tray (tray-icon-container tray icon-visibility))))
    (xembed:embed socket icon t 0 0)
    (initialize-icon-socket socket)
    (tray-update-icon-data tray socket icon-visibility)))

;;; Icon tiling
;;; This is necessary as some icons may want to resize themselves, and
;;; sometimes need to be repositioned These functions actually work on
;;; sockets

(defun tile-icons (icons) 
  "Repositions the ICONS one next to the other."
  (let ((x 0))
    (dolist (icon icons)
      (setf (xlib:drawable-x icon) x)
      (incf x (xlib:drawable-width icon)))))

(defun tray-tile-icons (tray)
  "Repositions the icons embedded in TRAY one next to the other."
  (tile-icons (tray-hicons tray))
  (tile-icons (tray-vicons tray)))

;;; Icon sorting
;;; These functions take a ORDERING parameter, an ordered list of window classes.

(defun icon-rank (ordering icon-socket)
  "Ranks the icon in ICON-SOCKET for icon sorting based on ORDERING."
  (position (client-wm-class icon-socket) ordering :test #'equalp))

(defun icon-comp (ordering icon1 icon2)
  "Compares the ranks of ICON1 and ICON2 based on ORDERING."
  (let ((ir1 (icon-rank ordering icon1))
	(ir2 (icon-rank ordering icon2)))
    (cond ((equalp ir1 ir2) 0)
	  ((null ir1) 1)
	  ((null ir2) -1)
	  ((< ir1 ir2) -1)
	  (t 1))))

(defun icon< (ordering icon1 icon2)
  "T if ICON1 should appear before ICON2 based on ORDERING. NIL otherwise."
  (< (icon-comp ordering icon1 icon2) 0))

(defun icon> (ordering icon1 icon2)
  "T if ICON1 should appear after ICON2 based on ORDERING. NIL otherwise."
  (> (icon-comp ordering icon1 icon2) 0))

(defun tray-sort-icons (tray)
  "Sorts the icons embedded in TRAY."
  (setf (tray-vicons tray)
	(sort (tray-vicons tray) (curry #'icon< (tray-vicons-ordering tray))))
  (setf (tray-hicons tray)
	(sort (tray-hicons tray) (curry #'icon> (tray-hicons-ordering tray)))))

;;; Tray geometry updating
(defun icon-container-width (tray visibility)
  "Returns the width of the appropriate TRAY icon container
based on VISIBILITY."
  (max (tray-icon-height tray)
       (reduce #'+ (tray-icons tray visibility) :key #'xlib:drawable-width)))

(defun update-icon-containers-geometry (tray)
  "Appropriately resizes the TRAY's icon containers (visible and hidden icons)."
  (let* ((viwin (tray-viwin tray))
	 (hiwin (tray-hiwin tray))
	 (hiwin-width (icon-container-width tray nil))
	 (viwin-width (icon-container-width tray t)))
    (cond ((tray-show-hiwin-p tray)
	   (setf (xlib:drawable-width hiwin) hiwin-width)
	   (setf (xlib:drawable-x viwin) hiwin-width)
	   (setf (xlib:drawable-width viwin) viwin-width))
	  (t (setf (xlib:drawable-x viwin) 0)
	     (setf (xlib:drawable-width viwin) viwin-width)))))

(defun update-cursor-geometry (tray)
  "Appropriately resizes the TRAY's, setting its width to the width of
the icon over it."
  (when (numberp (tray-curpos tray))
    (let ((icon (icon-at-cursor tray))
	  (curwin (tray-curwin tray)))
      (when icon
	(multiple-value-bind (dx)
	    (xlib:translate-coordinates (xembed:window-parent icon)
					(xlib:drawable-x icon)
					(xlib:drawable-y icon)
					(tray-win tray))
	  (setf (xlib:drawable-x curwin) dx)
	  (setf (xlib:drawable-y curwin) (+ *tray-cursor-icon-distance*
				       (tray-icon-height tray)))
	  (setf (xlib:drawable-height curwin) *tray-cursor-thickness*)
	  (setf (xlib:drawable-width curwin) (xlib:drawable-width icon)))))))

(defun update-main-window-geometry (tray)
  "Appropriately resizes the TRAY's main window, based on its icon
containers sizes."
  (let ((win (tray-win tray)))
    (setf (xlib:drawable-width win)
	  (tray-width tray))
    (setf (xlib:drawable-height win)
	  (tray-height tray))
    (setf (xlib:drawable-x win)
	  (funcall *tray-position-function* tray))))

(defun tray-update-geometry (tray)
  "Updates the TRAY's geometry (main windows and subwindows)."
  (update-icon-containers-geometry tray)
  (update-cursor-geometry tray)
  (update-main-window-geometry tray))

(defun tray-update (tray &optional (map-p t))
  "Sorts the icons embedded in TRAY, tiles them and updates the
geometry of its windows. Maps the tray windows - or unmaps them, based
on the TRAY state - if MAP-P is T."
  (tray-sort-icons tray)
  (tray-tile-icons tray)
  (tray-update-geometry tray)
  (when map-p
    (map-tray tray)))

;;; Icon hiding
(defun toggle-icon-hiding (tray icon-socket)
  "Hides the icon embedded in ICON-SOCKET."
  (let* ((visibility (icon-visibility icon-socket)))
    ;; Reparent icon in the appropriate container
    (xlib:reparent-window icon-socket (tray-icon-container tray (not visibility)) 0 0)
    ;; Update the *hidden-classes* variable
    (if visibility
	(push (client-wm-class icon-socket) *tray-hidden-classes*)
	(setf *tray-hidden-classes* (remove (client-wm-class icon-socket)
					    *tray-hidden-classes* :test #'equalp)))
    ;; 
    (setf (tray-icons tray visibility)
	  (remove icon-socket (tray-icons tray visibility)
		  :test #'xlib:window-equal))
    (tray-update-icon-data tray icon-socket (not visibility))
    (tray-update tray) 
    (setf (tray-curpos tray) (icon->pos tray icon-socket))))

(defun set-hiwin-visibility (tray visibility)
  "Show or hides the hidden icons embedded in TRAY, if VISIBILITY is
respectively T or NIL."
  (setf (tray-show-hiwin-p tray) visibility)
  (setf (tray-curpos tray) (and visibility (tray-curpos tray)))
  (tray-update tray))

(defun show-hiwin (tray)
  "Shows the hidden icons in TRAY."
  (set-hiwin-visibility tray t))

(defun hide-hiwin (tray)
  "Hides the hidden icons in TRAY."
  (set-hiwin-visibility tray nil))

;;; Icon moving
(defun move-before (list elt neighbor &key (test #'eql))
  "Returns a copy of LIST where ELT appears right before NEIGHBOR
instead of its current position in the list."
  (let* ((list-w/o-elt (remove elt list :test test))
	 (npos (position neighbor list-w/o-elt :test test)))
    (assert npos)
    (append (subseq list-w/o-elt 0 npos)
	    (cons elt (subseq list-w/o-elt npos)))))

(defun move-after (list elt neighbor &key (test #'eql))
  "Returns a copy of LIST where ELT appears right after NEIGHBOR
instead of its current position in the list."
  (let* ((list-w/o-elt (remove elt list :test test))
	 (npos (position neighbor list-w/o-elt :test test)))
    (assert npos)
    (append (subseq list-w/o-elt 0 (1+ npos))
	    (cons elt (subseq list-w/o-elt (1+ npos))))))

	
(defun move-icon (tray icon-socket neighbor-socket move-fn)
  "Moves the icon embedded in ICON-SOCKET in tray using MOVE-FN."
  (if (icon-visibility icon-socket) 
      (setf (tray-vicons-ordering tray)
	    (funcall move-fn (tray-vicons-ordering tray)
		     (client-wm-class icon-socket) (client-wm-class neighbor-socket)
		     :test #'string=))
      (setf (tray-hicons-ordering tray)
	    (funcall move-fn (tray-hicons-ordering tray)
		     (client-wm-class icon-socket) (client-wm-class neighbor-socket)
		     :test #'string=)))
  (tray-update tray)
  (setf (tray-curpos tray) (icon->pos tray icon-socket)))

(defun move-icon-left (tray)
  "Moves the icon embedded in ICON-SOCKET to the left."
  (let ((icon-at-left (ignore-errors (icon-at-pos tray (1- (tray-curpos tray))))))
    (when icon-at-left
      (move-icon tray (icon-at-cursor tray) icon-at-left #'move-before))))

(defun move-icon-right (tray)
  "Moves the icon embedded in ICON-SOCKET to the right."
  (let ((icon-at-right (ignore-errors (icon-at-pos tray (1+ (tray-curpos tray))))))
    (when icon-at-right
      (move-icon tray (icon-at-cursor tray) icon-at-right #'move-after))))

;;; Icon removal
(defun remove-icon (tray socket)
  "Removes the icon embedded in SOCKET from TRAY."
  (setf (tray-vicons tray)
	(remove socket (tray-vicons tray) :test #'xlib:window-equal))
  (setf (tray-hicons tray)
	(remove socket (tray-hicons tray) :test #'xlib:window-equal))
  (xembed:destroy-socket socket)
  (tray-update tray))

;;; Icon scaling to tray size
(defun scale-icon-width (tray-icon-height width height)
  "Scales the icon keeping its aspect ratio so that its height is TRAY-ICON-HEIGHT."
  (let ((aspect-ratio (if (or (zerop height) (zerop width)) 
			   1 ; some icons are initially mapped with zero width or height, assume square
			   (/ width height))))
    (ceiling (* tray-icon-height (max 1 aspect-ratio))))) ;; assume 1 as minimum aspect ratio

;;;; Xembed requirements
(defun xembed-tray-init (tray)
  "Initializes the TRAY windows as needed by the XEMBED protocol."
  (pushnew :WM_TAKE_FOCUS (xlib:wm-protocols (tray-win tray)))
  (pushnew :WM_TAKE_FOCUS (xlib:wm-protocols (tray-fpwin tray))))

;;;; FDO Tray requirements
(defun fdo-tray-selection-name (tray)
  "Returns the selection atom name for TRAY as specified by the FDO
System Tray protocol."
  (let* ((stumpwm-screen (tray-screen tray))
	 (screen (slot-value stumpwm-screen 'stumpwm::number))
	(display (tray-display tray)))
    (intern 
     (format nil "_NET_SYSTEM_TRAY_S~a" (xlib::screen-position screen display))
     'keyword)))

(defun fdo-tray-init-properties (tray)
  "Sets the selection owner window property as specified by the FDO
System tray protocol."
  (xlib:change-property (tray-sowin tray)
			:_NET_SYSTEM_TRAY_ORIENTATION #(0)
			:_NET_SYSTEM_TRAY_ORIENTATION 32))

(defun fdo-tray-set-selection-owner (tray)
  "Sets the selection owner of the manager selection as specified by
the FDO System Tray protocol."
  (setf (xlib:selection-owner (tray-display tray)
			      (fdo-tray-selection-name tray))
	(tray-sowin tray)))

(defun fdo-tray-send-manager-notification (tray)
  (let ((root-window (xlib:drawable-root (tray-win tray)))
	(atom-id (xlib:intern-atom (tray-display tray) (fdo-tray-selection-name tray))))
    (xlib:send-event root-window :client-message (xlib:make-event-mask :structure-notify)
		     :window root-window
		     :type :MANAGER
		     :format 32
		     :data (vector xembed:*timestamp* atom-id (xlib:window-id (tray-sowin tray)) 0 0)
		     :propagate-p nil)))

(defun fdo-tray-init (tray)
  "Initializes the TRAY windows as needed by the FDO System Tray
protocol."
  (fdo-tray-init-properties tray)
  (fdo-tray-set-selection-owner tray)
  (fdo-tray-send-manager-notification tray))

(defparameter +FDO-TRAY-OPCODES-ALIST+
  '((:SYSTEM-TRAY-REQUEST-DOCK . 0)
    (:SYSTEM-TRAY-BEGIN-MESSAGE . 1)
    (:SYSTEM-TRAY-CANCEL-MESSAGE . 2)))

(defun fdo-tray-encode-opcode (type)
  (cdr (assoc type +FDO-TRAY-OPCODES-ALIST+)))

(defun fdo-tray-decode-opcode (type)
  (car (rassoc type +FDO-TRAY-OPCODES-ALIST+)))

;;; Event handlers

;; Implements the fdo systemtray specification
(defun fdo-tray-make-event-handler (tray)
  (xembed:handler-vector
   ((:client-message) (window type data)
    (when (eq type :_NET_SYSTEM_TRAY_OPCODE) ;FIXME check destination window
      (destructuring-bind (timestamp message data1 data2 data3)
	  (coerce data 'list)
	(declare (ignorable data2 data3))
	(xembed:update-timestamp (tray-fpwin tray) timestamp)
	(let ((opcode (fdo-tray-decode-opcode message)))
	  (xembed:dformat 0 "TRAY-MESSAGE[~S](~S)~%" window opcode)
	  (case opcode
	    (:SYSTEM-TRAY-REQUEST-DOCK 
	     (add-icon tray data1)
	     (tray-update tray))
	    (:SYSTEM-TRAY-BEGIN-MESSAGE t)
	    (:SYSTEM-TRAY-CANCEL-MESSAGE t))))))))

;;; Xembed event handler
(defun make-tray-xembed-event-handler (tray)
  (let ((hnd (xembed:socket-list-handler-vector (lambda () (append (tray-vicons tray)
								   (tray-hicons tray))))))
    (xembed:combine-handlers (xembed:handler-vector
			      ((:client-message) (type data)
			       (when (and (eq type :_XEMBED))
				 (let ((opcode (xembed:decode-xembed-message-type (elt data 1))))
				   (case opcode
				     (:xembed-protocol-finished
				      (let ((socket (xlib::lookup-window (tray-display tray)
									 (elt data 3))))
					(remove-icon tray socket)))))))
			      ((:configure-notify) (event-window window width height)
			       (let ((iheight (tray-icon-height tray)))
				 (when (and (member event-window (append (tray-vicons tray)
									(tray-hicons tray))
						    :test #'xlib:window-equal)
					    (xlib:window-equal (xembed:client event-window) window))
				   (xembed:dformat 2 "CONFIGURE ~S~%" (list width height))
				   (xembed:socket-resize event-window
							 (scale-icon-width iheight width height)
							 iheight)
				   (tray-update tray)))
			       t))
			     hnd)))


(defun make-tray-handler (tray)
  "Builds and returns an event handler vector for TRAY, that can be
passed to `xlib:process-event'."
  (reduce #'xembed:combine-handlers
	  (list (make-tray-xembed-event-handler tray)
		(fdo-tray-make-event-handler tray))))

(defun tray-init (tray)
  "Initializes the TRAY object."
  (fdo-tray-init tray)
  (xembed-tray-init tray)
  (tray-update tray nil))

(defun new-mode-line-hook (mode-line)
  "If *tray-autoshow*, then creates tray window"
  (let ((stumpwm-screen (stumpwm::mode-line-screen mode-line)))
    (unless (screen-tray stumpwm-screen)
      (let* ((tray (create-tray stumpwm-screen mode-line))
             (hnd (make-tray-handler tray)))
        (setf (screen-tray stumpwm-screen) tray)
        (tray-init tray)
        (map-tray tray)
        (let ((event-handler (lambda ()
                               (loop while (ignore-errors
                                             (xlib:process-event stumpwm::*display* :timeout 0 :handler hnd))))))
          (setf (tray-event-processing-fn tray) event-handler)
          (stumpwm::add-hook stumpwm:*event-processing-hook* event-handler))))))

(defun destroy-mode-line-hook (mode-line)
  "Destroys tray, when mode-line is destroyed"
  (let* ((stumpwm-screen (stumpwm::mode-line-screen mode-line))
         (tray (screen-tray stumpwm-screen)))
    (when (and
           tray
           (xlib:window-equal
            (stumpwm::mode-line-window (tray-mode-line tray))
            (stumpwm::mode-line-window mode-line)))
      (destroy-tray tray))))

(defun add-mode-line-hooks ()
  (stumpwm:add-hook stumpwm:*new-mode-line-hook* #'new-mode-line-hook)
  (stumpwm:add-hook stumpwm:*destroy-mode-line-hook* #'destroy-mode-line-hook)
  nil)

(defun remove-mode-line-hooks ()
  (stumpwm:add-hook stumpwm:*new-mode-line-hook* #'new-mode-line-hook)
  (stumpwm:add-hook stumpwm:*destroy-mode-line-hook* #'destroy-mode-line-hook)
  nil)

(stumpwm:defcommand stumptray () ()
  "Enable tray for current screen"
  (if (current-tray)
      (destroy-tray (current-tray))
      (let* ((stumpwm-screen (stumpwm:current-screen))
             (tray (create-tray stumpwm-screen (screen-mode-line stumpwm-screen)))
             (hnd (make-tray-handler tray)))
        (setf (screen-tray stumpwm-screen) tray)
        (tray-init tray)
        (map-tray tray)
        (let ((event-handler (lambda ()
                               (loop while (ignore-errors
                                             (xlib:process-event stumpwm::*display* :timeout 0 :handler hnd))))))
          (setf (tray-event-processing-fn tray) event-handler)
          (stumpwm:add-hook stumpwm:*event-processing-hook*
                            event-handler)))))


(stumpwm:defcommand stumptray-toggle-hidden-icons-visibility () ()
  "Toggle icon visibility"
  (cond ((tray-show-hiwin-p (current-tray))
         (hide-hiwin (current-tray)))
        (t
	 (show-hiwin (current-tray))))
  (tray-update (current-tray)))

(stumpwm:defcommand systray-selection-right () ()
  "Selection right"
  (let* ((tray (current-tray))
	 (pos (tray-curpos tray)))
    (show-hiwin tray)
    (setf (tray-curpos tray) (1+ (or pos -1)))
    (tray-update tray)))

(stumpwm:defcommand systray-selection-left () ()
  "Selection left"
  (let* ((tray (current-tray))
	 (pos (tray-curpos tray)))
    (show-hiwin tray)
    (setf (tray-curpos tray) (1- (or pos 0)))
    (tray-update tray)))

(stumpwm:defcommand systray-toggle-icon-hiding () ()
  "Toggle icon hiding"
  (let ((tray (current-tray)))
    (toggle-icon-hiding tray (icon-at-cursor tray))
    (tray-update tray)))

(stumpwm:defcommand systray-move-icon-left () ()
  "Move icon left"
  (let ((tray (current-tray)))
    (show-hiwin tray)
    (move-icon-left tray)
    (tray-update tray)))

(stumpwm:defcommand systray-move-icon-right () ()
  "Move icon right"
  (let ((tray (current-tray)))
    (show-hiwin tray)
    (move-icon-right tray)
    (tray-update tray)))

(defun tray-window-list (tray)
  (append (tray-viwin tray)
	  (tray-hiwin tray)
	  (tray-sowin tray)
	  (tray-curwin tray)
	  (tray-win tray)
	  (tray-fpwin tray)))

(defun tray-socket-list (tray)
  (append (tray-vicons tray)
	  (tray-hicons tray)))

(defun tray-client-list (tray)
  (mapcar #'xembed:client (tray-socket-list tray)))
	  

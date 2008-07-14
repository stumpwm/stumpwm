;; Copyright (C) 2003-2008 Shawn Betts
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
;; Event handling.
;;
;; Code:

(in-package #:stumpwm)

;;; Event handler functions

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

(defun handle-mode-line-window (xwin x y width height)
  (declare (ignore width))
  (let ((ml (find-mode-line-window xwin)))
    (when ml
      (setf (xlib:drawable-height xwin) height)
      (update-mode-line-position ml x y)
      (resize-mode-line ml)
      (sync-mode-line ml))))

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

(defun update-configuration (win)
  ;; Send a synthetic configure-notify event so that the window
  ;; knows where it is onscreen.
  (xwin-send-configuration-notify (window-xwin win)
                                  (xlib:drawable-x (window-parent win))
                                  (xlib:drawable-y (window-parent win))
                                  (window-width win) (window-height win) 0))

(defun handle-managed-window (window width height stack-mode value-mask)
  "This is a managed window so deal with it appropriately."
  ;; Grant the stack-mode change (if it's mapped)
  (set-window-geometry window :width width :height height)
  (maximize-window window)
  (when (window-urgent-p window)
    (window-clear-urgency window))
  (when (and (window-in-current-group-p window)
             ;; stack-mode change?
             (= 64 (logand value-mask 64)))
    (case stack-mode
      (:above
       (maybe-raise-window window))))
  (update-configuration window))

(defun handle-window-move (win x y relative-to &optional (value-mask -1))
  (when *honor-window-moves*
    (dformat 3 "Window requested new position ~D,~D relative to ~S~%" x y relative-to)
    (labels ((has-x (mask) (= 1 (logand mask 1)))
             (has-y (mask) (= 2 (logand mask 2))))
      (when (or (eq relative-to :root) (has-x value-mask) (has-y value-mask))
        (let* ((group (window-group win))
               (pos  (if (eq relative-to :parent)
                         (list
                          (+ (xlib:drawable-x (window-parent win)) x)
                          (+ (xlib:drawable-y (window-parent win)) y))
                         (list x y)))
               (frame (apply #'find-frame group pos)))
          (when frame
            (pull-window win frame)))))))

(define-stump-event-handler :configure-request (stack-mode #|parent|# window #|above-sibling|# x y width height border-width value-mask)
  ;; Grant the configure request but then maximize the window after the granting.
  (dformat 3 "CONFIGURE REQUEST ~@{~S ~}~%" stack-mode window x y width height border-width value-mask)
  (let ((win (find-window window)))
    (cond
      (win
       (handle-window-move win x y :parent value-mask)
       (handle-managed-window win width height stack-mode value-mask))
      ((handle-mode-line-window window x y width height))
      (t (handle-unmanaged-window window x y width height border-width value-mask)))))

(define-stump-event-handler :configure-notify (stack-mode #|parent|# window #|above-sibling|# x y width height border-width value-mask)
  (dformat 4 "CONFIGURE NOTIFY ~@{~S ~}~%" stack-mode window x y width height border-width value-mask)
  (let ((screen (find-screen window)))
    (when screen
      (let ((old-heads (copy-list (screen-heads screen))))
        (setf (screen-heads screen) nil)
        (let ((new-heads (make-screen-heads screen (screen-root screen))))
          (setf (screen-heads screen) old-heads)
          (cond
            ((equalp old-heads new-heads)
             (dformat 3 "Bogus configure-notify on root window of ~S~%" screen) t)
            (t
             (dformat 1 "Updating Xinerama configuration for ~S.~%" screen)
             (if new-heads
                 (progn
                   (scale-screen screen new-heads)
                   (mapc 'sync-all-frame-windows (screen-groups screen))
                   (update-mode-lines screen))
                 (dformat 1 "Invalid configuration! ~S~%" new-heads)))))))))

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
        ((eq (xwin-type window) :dock)
         (when wwin
           (setf screen (window-screen wwin)))
         (dformat 1 "window is dock-type. attempting to place in mode-line.")
         (place-mode-line-window screen window)
         ;; Some panels are broken and only set the dock type after they map and withdraw.
         (when wwin
           (setf (screen-withdrawn-windows screen) (delete wwin (screen-withdrawn-windows screen))))
         t)
        (wwin (restore-window wwin))
        ((xlib:get-property window :_KDE_NET_WM_SYSTEM_TRAY_WINDOW_FOR)
         ;; Do nothing if this is a systray window (the system tray
         ;; will handle it, if there is one, and, if there isn't the
         ;; user doesn't want this popping up as a managed window
         ;; anyway.
         t)
        (t
         (let ((window (process-mapped-window screen window)))
           ;; Give it focus
           (if (deny-request-p window *deny-map-request*)
               (unless *suppress-deny-messages*
                 (if (eq (window-group window) (current-group))
                     (echo-string (window-screen window) (format nil "'~a' denied map request" (window-name window)))
                     (echo-string (window-screen window) (format nil "'~a' denied map request in group ~a" (window-name window) (group-name (window-group window))))))
               (frame-raise-window (window-group window) (window-frame window) window
                                   (if (eq (window-frame window)
                                           (tile-group-current-frame (window-group window)))
                                       t nil)))))))))

(define-stump-event-handler :unmap-notify (send-event-p event-window window #|configure-p|#)
  ;; There are two kinds of unmap notify events: the straight up
  ;; ones where event-window and window are the same, and
  ;; substructure unmap events when the event-window is the parent
  ;; of window.
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
;;          (internal-window-p (window-screen window) window))
;;    (process-new-window (window-screen window) window))
;;    (run-hook-with-args *new-window-hook* window)))

(define-stump-event-handler :destroy-notify (send-event-p event-window window)
  (unless (or send-event-p
              (xlib:window-equal event-window window))
    ;; Ignore structure destroy notifies and only
    ;; use substructure destroy notifiers. This way
    ;; event-window is the window's parent.
    (let ((win (or (find-window window)
                   (find-withdrawn-window window))))
      (if win
          (destroy-window win)
          (progn
            (let ((ml (find-mode-line-window window)))
              (when ml (destroy-mode-line-window ml))))))))

(defun read-from-keymap (kmap &optional update-fn)
  "Read a sequence of keys from the user, guided by the keymap,
KMAP and return the binding or nil if the user hit an unbound sequence.

The Caller is responsible for setting up the input focus."
  (let* ((code-state (read-key-no-modifiers))
         (code (car code-state))
         (state (cdr code-state)))
    (handle-keymap kmap code state nil nil update-fn)))

(defun handle-keymap (kmap code state key-seq grab update-fn)
  "Find the command mapped to the (code state) and return it."
  ;; a symbol is assumed to have a hashtable as a value.
  (dformat 1 "Awaiting key ~a~%" kmap)
  (let ((keymap '()))
    (when (and (symbolp kmap)
               (boundp kmap)
               (hash-table-p (symbol-value kmap)))
      (setf
       keymap kmap
       kmap (symbol-value kmap)))
    (check-type kmap hash-table)
    (let* ((key (code-state->key code state))
           (cmd (lookup-key kmap key))
           (key-seq (cons key key-seq)))
      (dformat 1 "key-press: ~S ~S ~S~%" key state cmd)
      (run-hook-with-args *key-press-hook* key key-seq cmd)
      (when update-fn
        (funcall update-fn key-seq))
      (if cmd
          (cond
            ((or (hash-table-p cmd)
                 (and (symbolp cmd)
                      (boundp cmd)
                      (hash-table-p (symbol-value cmd))))
             (when grab
               (grab-pointer (current-screen)))
             (let* ((code-state (read-key-no-modifiers))
                    (code (car code-state))
                    (state (cdr code-state)))
               (unwind-protect
                    (handle-keymap cmd code state key-seq nil update-fn)
                 (when grab (ungrab-pointer)))))
            (t (values cmd key-seq)))
          (if (find key (list (kbd "?")
                              (kbd "C-h"))
                    :test 'equalp)
              (progn (display-keybinding keymap) (values t key-seq))
              (values nil key-seq))))))

(define-stump-event-handler :key-press (code state #|window|#)
  (labels ((get-cmd (code state)
             (with-focus (screen-key-window (current-screen))
               (handle-keymap *top-map* code state nil t nil))))
    (unwind-protect
         ;; modifiers can sneak in with a race condition. so avoid that.
         (unless (is-modifier code)
           (multiple-value-bind (cmd key-seq) (get-cmd code state)
             (cond
               ((eq cmd t))
               (cmd
                (unmap-message-window (current-screen))
                (interactive-command cmd) t)
               (t (message "~{~a ~}not bound." (mapcar 'print-key (nreverse key-seq))))))))))

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

(defun handle-stumpwm-commands (root)
  "Handle a StumpWM style command request."
  (let* ((win root)
         (screen (find-screen root))
         (data (xlib:get-property win :stumpwm_command :delete-p t))
         (cmd (bytes-to-string data)))
    (let ((msgs (screen-last-msg screen))
          (hlts (screen-last-msg-highlights screen))
          (*executing-stumpwm-command* t))
      (setf (screen-last-msg screen) '()
            (screen-last-msg-highlights screen) '())
      (interactive-command cmd)
      (xlib:change-property win :stumpwm_command_result
                            (string-to-bytes (format nil "~{~{~a~%~}~}" (nreverse (screen-last-msg screen))))
                            :string 8)
      (setf (screen-last-msg screen) msgs
            (screen-last-msg-highlights screen) hlts))
    (xlib:display-finish-output *display*)))

(defun maybe-set-urgency (window)
  (when (and (window-urgent-p window)
             (not (find window (screen-urgent-windows (window-screen window)))))
    (when (register-urgent-window window)
      (run-hook-with-args *urgent-window-hook* window))))

(defun safe-atom-name (n)
  "Return the name of the atom with atom-id N or nil if there isn't one."
  (handler-case
      (xlib:atom-name *display* n)
    (xlib:atom-error ()
      nil)))

(defun safe-bytes-to-atoms (list)
  "Return a list of atoms from list. Any number that cannot be
converted to an atom is removed."
  (loop for p in list
        when (typep p '(unsigned-byte 29))
        collect (safe-atom-name p)))

(defun update-window-properties (window atom)
  (case atom
    (:wm_name
     (setf (window-title window) (xwin-name (window-xwin window)))
     ;; Let the mode line know about the new name.
     (update-all-mode-lines))
    (:wm_normal_hints
     (setf (window-normal-hints window) (xlib:wm-normal-hints (window-xwin window))
           (window-type window) (xwin-type (window-xwin window)))
     (dformat 4 "new hints: ~s~%" (window-normal-hints window))
     (maximize-window window))
    (:wm_hints
     (maybe-set-urgency window))
    (:wm_class
     (setf (window-class window) (xwin-class (window-xwin window))
           (window-res window) (xwin-res-name (window-xwin window))))
    (:wm_window_role
     (setf (window-role window) (xwin-role (window-xwin window))))
    (:wm_transient_for
     (setf (window-type window) (xwin-type (window-xwin window)))
     (maximize-window window))
    (:_NET_WM_STATE
     ;; Some clients put really big numbers in the list causing
     ;; atom-name to fail, so filter out anything that can't be
     ;; converted into an atom.
     (dolist (p (safe-bytes-to-atoms
                 (xlib:get-property (window-xwin window) :_NET_WM_STATE)))
       (case p
         (:_NET_WM_STATE_FULLSCREEN
          ;; Client is broken and sets this property itself instead of sending a
          ;; client request to the root window. Try to make do.
          ;; FIXME: what about when properties are REMOVED?
          (update-fullscreen window 1)))))))

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
    (:stumpwm_command
     ;; RP commands are too weird and problematic, KISS.
     (let* ((screen (find-screen window)))
       (when (and (eq state :new-value)
                  screen)
         (handle-stumpwm-commands window))))
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

(defun find-message-window-screen (win)
  "Return the screen, if any, that message window WIN belongs."
  (dolist (screen *screen-list*)
    (when (xlib:window-equal (screen-message-window screen) win)
      (return screen))))

(defun draw-cross (screen window x y width height)
  (xlib:draw-line window
                  (screen-frame-outline-gc screen)
                  x y
                  width height
                  t)
  (xlib:draw-line window
                  (screen-frame-outline-gc screen)
                  x (+ y height)
                  (+ x width) y))

(define-stump-event-handler :exposure (window x y width height count)
  (let (screen ml)
    (when (zerop count)
      (cond
        ((setf screen (find-screen window))
         ;; root exposed
         (show-frame-outline (screen-current-group screen) nil))
        ((setf screen (find-message-window-screen window))
         ;; message window exposed
         (if (plusp (screen-ignore-msg-expose screen))
             (decf (screen-ignore-msg-expose screen))
             (redraw-current-message screen)))
        ((setf ml (find-mode-line-window window))
         (setf screen (mode-line-screen ml))
         (redraw-mode-line ml t)))
      ;; Show the area.
      (when (and *debug-expose-events* screen)
        (draw-cross screen window x y width height)))))

(define-stump-event-handler :reparent-notify (window parent)
  (let ((win (find-window window)))
    (when (and win
               (not (xlib:window-equal parent (window-parent win))))
      ;; reparent it back
      (unless (eq (xlib:window-map-state (window-xwin win)) :unmapped)
        (incf (window-unmap-ignores win)))
      (xlib:reparent-window (window-xwin win) (window-parent win) 0 0))))

;;; Fullscreen functions

(defun activate-fullscreen (window)
  (dformat 2 "client requests to go fullscreen~%")
  (add-wm-state (window-xwin window) :_NET_WM_STATE_FULLSCREEN)
  (setf (window-fullscreen window) t)
  (maximize-window window)
  (focus-window window))

(defun deactivate-fullscreen (window)
  (setf (window-fullscreen window) nil)
  (dformat 2 "client requests to leave fullscreen~%")
  (remove-wm-state (window-xwin window) :_NET_WM_STATE_FULLSCREEN)
  (setf (xlib:drawable-border-width (window-parent window)) (default-border-width-for-type window))
  (maximize-window window)
  (update-window-border window)
  (update-mode-lines (current-screen)))

(defun update-fullscreen (window action)
  (let ((fullscreen-p (window-fullscreen window)))
    (case action
      (0                                ; _NET_WM_STATE_REMOVE
       (when fullscreen-p
         (deactivate-fullscreen window)))
      (1                                ; _NET_WM_STATE_ADD
       (unless fullscreen-p
         (activate-fullscreen window)))
      (2                                ; _NET_WM_STATE_TOGGLE
       (if fullscreen-p
           (deactivate-fullscreen window)
           (activate-fullscreen window))))))

(defun maybe-raise-window (window)
  (if (deny-request-p window *deny-raise-request*)
      (unless (or *suppress-deny-messages*
                  ;; don't mention windows that are already visible
                  (eq (frame-window (window-frame window)) window))
        (if (eq (window-group window) (current-group))
            (echo-string (window-screen window) (format nil "'~a' denied raise request" (window-name window)))
            (echo-string (window-screen window) (format nil "'~a' denied raise request in group ~a" (window-name window) (group-name (window-group window))))))
      (focus-all window)))

(define-stump-event-handler :client-message (window type #|format|# data)
  (dformat 2 "client message: ~s ~s~%" type data)
  (case type
    (:_NET_CURRENT_DESKTOP              ;switch desktop
     (let* ((screen (find-screen window))
            (n (elt data 0))
            (group (and screen
                        (< n (length (screen-groups screen)))
                        (elt (sort-groups screen) n))))
       (when group
         (switch-to-group group))))
    (:_NET_WM_DESKTOP                   ;move window to desktop
     (let* ((our-window (find-window window))
            (screen (when our-window
                      (window-screen our-window)))
            (n (elt data 0))
            (group (and screen
                        (< n (length (screen-groups screen)))
                        (elt (sort-groups screen) n))))
       (when (and our-window group)
         (move-window-to-group our-window group))))
    (:_NET_ACTIVE_WINDOW
     (let ((our-window (find-window window))
           (source (elt data 0)))
       (when our-window
         (if (= source 2)               ;request is from a pager
             (focus-all our-window)
             (maybe-raise-window our-window)))))
    (:_NET_CLOSE_WINDOW
     (let ((our-window (find-window window)))
       (when our-window
         (delete-window our-window))))
    (:_NET_WM_STATE
     (let ((our-window (find-window window)))
       (when our-window
         (let ((action (elt data 0))
               (p1 (elt data 1))
               (p2 (elt data 2)))
           (dolist (p (list p1 p2))
             ;; Sometimes the number cannot be converted to an atom, so skip them.
             (unless (or (= p 0)
                         (not (typep p '(unsigned-byte 29))))
               (case (safe-atom-name p)
                 (:_NET_WM_STATE_DEMANDS_ATTENTION
                  (case action
                    (1
                     (add-wm-state window :_NET_WM_STATE_DEMANDS_ATTENTION))
                    (2
                     (unless (find-wm-state window :_NET_WM_STATE_DEMANDS_ATTENTION)
                       (add-wm-state window :_NET_WM_STATE_DEMANDS_ATTENTION))))
                  (maybe-set-urgency our-window))
               (:_NET_WM_STATE_FULLSCREEN
                (update-fullscreen our-window action)))))))))
  (:_NET_MOVERESIZE_WINDOW
   (let ((our-window (find-window window)))
     (when our-window
       (let ((x (elt data 1))
             (y (elt data 2)))
         (dformat 3 "!!! Data: ~S~%" data)
         (handle-window-move our-window x y :relative :root)))))
  (t
   (dformat 2 "ignored message~%"))))

(define-stump-event-handler :focus-out (window mode kind)
  (dformat 5 "~@{~s ~}~%" window mode kind))

;;; Mouse focus

(defun focus-all (win)
  "Focus the window, frame, group and screen belonging to WIN. Raise
the window in it's frame."
  (when (and win (window-frame win))
    (unmap-message-window (window-screen win))
    (switch-to-screen (window-screen win))
    (let ((frame (window-frame win))
          (group (window-group win)))
      (switch-to-group group)
      (frame-raise-window group frame win))))

(define-stump-event-handler :enter-notify (window mode)
  (when (and window (eq mode :normal) (eq *mouse-focus-policy* :sloppy))
    (let ((win (find-window window)))
      (when (and win (find win (top-windows)))
        (focus-all win)))))

(define-stump-event-handler :button-press (window code x y child time)
  ;; Pass click to client
  (xlib:allow-events *display* :replay-pointer time)
  (let (screen ml win)
    (cond
      ((and (setf screen (find-screen window)) (not child))
       (when (and (eq *mouse-focus-policy* :click)
                  *root-click-focuses-frame*)
         (let* ((group (screen-current-group screen))
                (frame (find-frame group x y)))
           (when frame
             (focus-frame group frame))))
       (run-hook-with-args *root-click-hook* screen code x y))
      ((setf ml (find-mode-line-window window))
       (run-hook-with-args *mode-line-click-hook* ml code x y))
      ((setf win (find-window-by-parent window (visible-windows)))
       (when (eq *mouse-focus-policy* :click)
         (focus-all win))))))

;; Handling event :KEY-PRESS
;; (:DISPLAY #<XLIB:DISPLAY :0 (The X.Org Foundation R60700000)> :EVENT-KEY :KEY-PRESS :EVENT-CODE 2 :SEND-EVENT-P NIL :CODE 45 :SEQUENCE 1419 :TIME 98761213 :ROOT #<XLIB:WINDOW :0 96> :WINDOW #<XLIB:WINDOW :0 6291484> :EVENT-WINDOW #<XLIB:WINDOW :0 6291484> :CHILD
;;  #<XLIB:WINDOW :0 6291485> :ROOT-X 754 :ROOT-Y 223 :X 753 :Y 222 :STATE 4 :SAME-SCREEN-P T)
;; H

(defun handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  (dformat 1 ">>> ~S~%" event-key)
  (let ((eventfn (gethash event-key *event-fn-table*))
        (win (getf event-slots :window)))
    (when eventfn
      ;; XXX: In both the clisp and sbcl clx libraries, sometimes what
      ;; should be a window will be a pixmap instead. In this case, we
      ;; need to manually translate it to a window to avoid breakage
      ;; in stumpwm. So far the only slot that seems to be affected is
      ;; the :window slot for configure-request and reparent-notify
      ;; events. It appears as though the hash table of XIDs and clx
      ;; structures gets out of sync with X or perhaps X assigns a
      ;; duplicate ID for a pixmap and a window.
      (when (and win (not (xlib:window-p win)))
        (dformat 10 "Pixmap Workaround! ~s should be a window!~%" win)
        (setf (getf event-slots :window) (make-xlib-window win)))
      (handler-case
          (progn
            ;; This is not the stumpwm top level, but if the restart
            ;; is in the top level then it seems the event being
            ;; processed isn't popped off the stack and is immediately
            ;; reprocessed after restarting to the top level. So fake
            ;; it, and put the restart here.
            (with-simple-restart (top-level "Return to stumpwm's top level")
              (let ((*interactivep* t))
		 (apply eventfn event-slots)))
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

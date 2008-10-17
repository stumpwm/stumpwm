(defpackage :test-wm (:use :cl))
(in-package :test-wm)

(defparameter *current-test-num* 0)

(defparameter *tests* nil)

(defmacro define-test ((dpy screen) &body body)
  (let ((name (intern (format nil "TEST-~d" *current-test-num*))))
    `(progn
      (defun ,name (,dpy ,screen)
        (format t "Starting test ~d~%" ,*current-test-num*)
        ,@body
        (format t "Done.~%"))
      (push ',name *tests*)
      (incf *current-test-num*))))

(define-test (dpy screen)
    (let ((w (xlib:create-window :parent (xlib:screen-root screen)
                                 :x 10 :y 10 :width 100 :height 100 :border-width 1)))
      (xlib:map-window w)
      (xlib:display-finish-output dpy)
      (xlib:destroy-window w)
      (xlib:display-finish-output dpy)))

(define-test (dpy screen)
    (let ((w (xlib:create-window :parent (xlib:screen-root screen)
                                 :x 10 :y 10 :width 100 :height 100 :border-width 1)))
      (xlib:map-window w)
      (xlib:display-finish-output dpy)
      (sleep 1)
      (setf (xlib:window-priority w) :above)
      ;;       (setf (xlib:drawable-border-width w) 3)
      (xlib:display-finish-output dpy)
      (xlib:destroy-window w)
      (xlib:display-finish-output dpy)))

(define-test (dpy screen)
    (let ((windows (loop for i from 0 to 100
                         collect (let ((w (xlib:create-window :parent (xlib:screen-root screen)
                                                              :x 10 :y 10 :width 100 :height 100 :border-width 1)))
                                   (xlib:map-window w)
                                   (xlib:display-finish-output dpy)
                                   (setf (xlib:window-priority w) :above)
                                   w))))
      (xlib:display-finish-output dpy)
      (loop for i in windows do
            (xlib:unmap-window i))
      (xlib:display-finish-output dpy)
      (sleep 3)
      (loop for i in windows do
            (xlib:destroy-window i))))

;; (define-test (dpy screen)
;;     (let ((windows (loop for i from 0 to 100
;;                    collect (let ((w (xlib:create-window :parent (xlib:screen-root screen)
;;                                                         :x 10 :y 10 :width 100 :height 100 :border-width 1)))
;;                              (xlib:map-window w)
;;                              (xlib:display-finish-output dpy)
;;                              (setf (xlib:window-priority w) :above)
;;                              w))))
;;       (xlib:display-finish-output dpy)
;;       (loop for i in windows do
;;         (xlib:unmap-window i))
;;       (xlib:display-finish-output dpy)
;;       (sleep 3)
;;       (loop for i in windows do
;;         (xlib:destroy-window i))))

(define-test (dpy screen)
    (let ((w (xlib:create-window :parent (xlib:screen-root screen)
                                 :x 10 :y 10 :width 100 :height 100 :border-width 1)))
      (xlib:map-window w)
      (setf (xlib:window-priority w) :above)
      (xlib:display-finish-output dpy)
      (xlib:unmap-window w)
      (setf (xlib:drawable-x w) 5)
      (xlib:display-finish-output dpy)))

(define-test (dpy screen)
    ;; create a window and set its role after being mapped
    (let ((w (xlib:create-window :parent (xlib:screen-root screen)
                                 :x 10 :y 10 :width 100 :height 100 :border-width 1)))
      (xlib:map-window w)
      (xlib:display-finish-output dpy)
      (sleep 1)
      (xlib:change-property w
                            :WM_WINDOW_ROLE
                            (map 'list 'char-code "rad dude")
                            :string
                            8)
      (xlib:display-finish-output dpy)
      (sleep 10)))

(defun break-display-xid-cache ()
  (labels ((make-win (dpy)
             (xlib:create-window :parent (xlib:screen-root (first (xlib:display-roots dpy))) :x 0 :y 0 :width 50 :height 50))
           (make-pixmap (window)
             (xlib:create-pixmap :width (random 100) :height (random 100) :depth 8 :drawable window))
           (first-pass (dpy)
             ;; Open a fresh connection. Create a window and a pixmap.
             (let* ((dpy2 (xlib:open-default-display))
                    (window (make-win dpy2))
                    (pixmap (make-pixmap window)))
               ;; make the pixmap the window's icon pixmap hint. 
               (setf (xlib:wm-hints window) (xlib:make-wm-hints :icon-pixmap pixmap))
               (format t "Window ID: ~s pixmap ID: ~s~%" (xlib:window-id window) (xlib:pixmap-id pixmap))
               (xlib:map-window window)
               (xlib:display-finish-output dpy2)
               (sleep 1)
               ;; On the old connection, list the root window children
               ;; and the icon pixmap hint to cache their XIDs.
               (loop for w in (xlib:query-tree (xlib:screen-root (first (xlib:display-roots dpy))))
                  for hints = (xlib:wm-hints w)
                  when hints
                  do (format t "top level window id: ~s | icon pixmap hint: ~s~%" (xlib:window-id w) (xlib:wm-hints-icon-pixmap hints)))
               (xlib:close-display dpy2)))
           (second-pass (dpy)
             ;; Open a fresh connection and create 2 windows.
             (let* ((dpy2 (xlib:open-default-display))
                    (window1 (make-win dpy2))
                    (window2 (make-win dpy2)))
               (format t "Window#1 ID: ~s Window#2 ID: ~s~%" (xlib:window-id window1) (xlib:window-id window2))
               (xlib:display-finish-output dpy2)
               ;; On the old connection, list the root window children
               ;; and note the second window is erroneously a pixmap
               ;; due to too agressive caching in clx.
               (loop for w in (xlib:query-tree (xlib:screen-root (first (xlib:display-roots dpy))))
                  do (format t "window: ~s~%" w))
               (xlib:close-display dpy2))))
    (let ((dpy (xlib:open-default-display)))
      (first-pass dpy)
      (second-pass dpy)
      (xlib:close-display dpy))))

(defun test-wm-class (map-p)
  "Test the robustness of CLX's wm-class function. If MAP-P is T then
map the window. Useful if you want to test the running window
manager."
  (labels ((test-it (w &rest strings)
             (xlib:change-property w :WM_CLASS
                                   (apply #'concatenate '(vector xlib:card8)
                                          strings)
                                   :string 8)
             (print (multiple-value-list (xlib:get-wm-class w)))
             ;; give the wm a chance to try out the value
             (when map-p
               (sleep 1)))
           (convert (s)
             (map '(vector xlib:card8) #'xlib:char->card8 s)))
    (let* ((dpy (xlib:open-default-display))
           (screen (first (xlib:display-roots dpy)))
           (root (xlib:screen-root screen))
           (win (xlib:create-window :parent root :x 0 :y 0 :width 100 :height 100 :background (xlib:screen-white-pixel screen))))
      (unwind-protect
           (when map-p
             (xlib:map-window win))
           (progn
             (test-it win
                      (convert "string 1")
                      #(0)
                      (convert "string 2")
                      #(0))
             (test-it win
                      (convert "Manifold X")
                      #(0)
                      (convert "Powercoupling Y")
                      #(0)
                      (convert "Magistrate Z")
                      #(0))
             (test-it win
                      #(0))
             (test-it win)
             (test-it win
                      #(0)
                      (convert "checkity checkfoo"))
             (test-it win
                      (convert "ohh bother")
                      #(0)
                      (convert "Magic Fudge"))
             (test-it win
                      (convert "You Gellin?")
                      #(0))
             (test-it win
                      (convert "Blinky The Cloon")))
        (xlib:close-display dpy))
      (values))))

(defun get-wm-hints ()
  "simias reports that on sbcl the wm-hints property is all screwed up
when he runs an x server on 32 or 64bit freebsd and runs any x client
on a fedora 32bit, connecting through an ssh tunnel. clisp works fine.

so run this function on clisp and sbcl and compare the numbers. This
assumes you're running a reparenting wm."
  (let ((dpy (xlib:open-default-display)))
    (write-line "you gotta have some windows open for this to work.")
    (dolist (top (xlib:query-tree (xlib:screen-root (first (xlib:display-roots dpy)))))
      (dolist (w (xlib:query-tree top))
        (format t "~s ~s: ~s~%" w (xlib:wm-name w) (xlib:get-property w :WM_HINTS :type :WM_HINTS :result-type 'vector))))
    (xlib:close-display dpy)))

(defun parse-display-string (display)
  "Parse an X11 DISPLAY string and return the host and display from it."
  (let* ((colon (position #\: display))
         (host (subseq display 0 colon))
         (rest (subseq display (1+ colon)))
         (dot (position #\. rest))
         (num (parse-integer (subseq rest 0 dot))))
    (values host num)))

(defvar *dpy* nil)

(defun test-wm (display-str)
  (multiple-value-bind (host display) (parse-display-string display-str)
    (setf *dpy* (xlib:open-display host :display display :protocol nil))
    (let* ((dpy *dpy*)
           (screen (first (xlib:display-roots dpy))))
      (unwind-protect
           (progn
             ;;              (dolist (i *tests*)
             ;;                (funcall i dpy screen))
             (funcall (car *tests*) dpy screen)
             )
        (xlib:close-display *dpy*)))))

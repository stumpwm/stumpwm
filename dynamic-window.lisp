(in-package :stumpwm)

(defvar *expose-n-max* 26 
  "Maximum number of windows to display in the expose")

(defvar *expose-auto-tile-fn* 'expose-tile
  "Function to call to tile current windows.")

(defun frame-area (frames)
  "Calculate the area of each frame and store the result in a list."
  (mapcar (lambda (f)
            (* (frame-width f) (frame-height f)))
          frames))

(defun recursive-tile (n group)
  "Find the largest (by area) frame in the group, split it in half
vertically or horizontally depending on which is dimension is larger.
Repeat until there is only one window left."
  (unless (<= n 1)
    (let* ((frames (group-frames group))
           (areas (frame-area frames))
           (idx (position (reduce #'max areas) areas))
           (frame (nth idx frames))
           (w (frame-width frame))
           (h (frame-height frame)))
      (focus-frame group frame)
      (if (< w h)
          (vsplit)
          (hsplit)))
    (recursive-tile (- n 1) group)))

(defun expose-tile (win &optional (group (current-group)))
  "First make only one frame, then recursively split the frame until
there are no more hidden windows. Tiling is done by splitting in the
direction that is widest, and choosing the frame that has the largest
area."
  (declare (ignore win))
  (let* ((windows (group-windows group))
         (num-win (length windows)))
    ;; Only try to make this the only frame if it isn't already.
    (unless (only-one-frame-p)
      (only))
    (recursive-tile (min *expose-n-max* num-win) group)))

(defgeneric invoke-expose (group))

(defmethod invoke-expose ((group tile-group))
  (funcall *expose-auto-tile-fn* nil
           (current-group (current-screen)))
  ;; have the user select a window
  (unless (only-one-frame-p)
    (run-commands "fselect"))
  ;; maximize that window
  (only))

(defmethod invoke-expose ((group float-group))
  (let ((curwin (group-current-window group))
        (wincount (length (group-windows group)))
        (width (head-width (group-current-head group)))
        (height (head-height (group-current-head group)))
        (old-win-positions nil))
    (labels ((collect-x-y (wincount)
               (let ((factors (loop for i from 1 to (1+ wincount)
                                    when (= (mod wincount i) 0)
                                      collect i)))
                 (cond ((or (= (length factors) 1) ; prime number
                            (= (length factors) 2)) 
                        (collect-x-y (1+ wincount)))
                       (t (nthcdr (floor (length factors) 2) factors))))))
      (unless (= wincount 1)
        (destructuring-bind (ycount xcount &rest ignore)
            (let ((f (collect-x-y wincount)))
              (let* ((x (car f))
                     (y (ceiling wincount x)))
                (list y x)))
          (declare (ignore ignore))
          (block done
            (let ((w (floor width xcount))
                  (h (floor height ycount))
                  (y 0)
                  (x 0)
                  (wins (group-windows group)))
              (dotimes (i ycount)
                (setf y (* i h))
                (dotimes (j xcount)
                  (setf x (* j w))
                  (let ((win (pop wins)))
                    (if win 
                        (progn
                          (push (list win :x (window-x win)
                                          :y (window-y win)
                                          :width (window-width win)
                                          :height (window-height win))
                                old-win-positions)
                          (float-window-move-resize win :x x
                                                        :y y
                                                        :width w
                                                        :height h))
                        (return-from done nil)))))))
          (let* ((ws (draw-window-numbers group))
                 (focuswin
                   (or (unwind-protect
                            (multiple-value-bind (has-click ch x y)
                                (read-one-char-or-click group)
                              (if has-click
                                  (let ((winner))
                                    (dolist (f (group-windows group))
                                      (when (and (> x (window-x f))
                                                 (> y (window-y f)))
                                        (if winner
                                            (when (or (> (window-x f)
                                                         (window-x winner))
                                                      (> (window-y f)
                                                         (window-y winner)))
                                              (setf winner f))
                                            (setf winner f))))
                                    (ungrab-pointer)
                                    winner)
                                  (when ch
                                    (let ((num
                                            (read-from-string (string ch) nil nil)))
                                      (dformat 3 "read ~S ~S~%" ch num)
                                      (find num (group-windows group)
                                            :test #'=
                                            :key 'window-number)))))
                         (mapc #'xlib:destroy-window ws)
                         (mapc (lambda (spec)
                                 (apply #'float-window-move-resize spec))
                               old-win-positions)
                         (xlib:display-finish-output *display*))
                       curwin)))
            (when focuswin
              (focus-all focuswin))))))))

(defcommand expose () ()
  "Automagically lay out all windows in a grid and let the user select one, making
that window the focused window. Set the variable `*expose-auto-tile-fn*' to
another tiling function if a different layout is desired for tile groups. Set
`*expose-n-max*' to the maximum number of windows to be displayed for choosing
when in a tile group."
  (invoke-expose (current-group (current-screen))))

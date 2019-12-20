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


(defcommand expose () ()
  "Automagically tile all windows and let the user select one, make
that window the focus. Set the variable `*expose-auto-tile-fn*' to another
tiling function if a different layout is desired. Set `*expose-n-max*' to the
maximum number of windows to be displayed for choosing."
  (funcall *expose-auto-tile-fn* nil
           (current-group (current-screen)))
  ;; have the user select a window
  (unless (only-one-frame-p)
    (run-commands "fselect"))
  ;; maximize that window
  (only))

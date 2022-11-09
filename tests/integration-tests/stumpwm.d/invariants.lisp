(in-package #:stumpwm)

;; Utilities

(defun collect-strings (&rest x)
  (remove-if-not #'stringp (flatten x)))

(defmacro implies (first &rest rest)
  (if rest
      `(if ,first (implies ,@rest) t)
      first))

(defun join-strings (l)
  (when l (format nil "~{~A~^~&~}" l)))

;; Invariants

(defvar *complain-at* nil)
(defun complain (&rest args)
  (format nil "~{~A~^ ~}~& at: ~{~{~A~^ ~}~^~&  -> ~}" args (reverse *complain-at*)))
(defmacro with-complain-context ((&rest context) &body body)
  `(let ((*complain-at* (cons (list ,@context) *complain-at*))) ,@body))

(defun get-invariant-violations-for-float-group (float-group)
  (declare (ignore float-group))
  nil ;; todo
  )

(defun get-invariant-violations-for-dynamic-group (dynamic-group)
  (declare (ignore dynamic-group))
  nil ;; todo
  )

(defun get-invariant-violations-for-tile-group-and-tree (tile-group tree)
  (with-complain-context ("tile group" tile-group "for tree" tree)
    (let ((x (tree-x tree))
          (y (tree-y tree))
          (width (tree-width tree))
          (height (tree-height tree))
          (branch (not (atom tree))))
      (list
       (unless (<= *min-frame-width* width)
         (complain "Width" width "is smaller than *min-frame-width*" *min-frame-width*))
       (unless (<= *min-frame-height* height)
         (complain "Height" height "is smaller than *min-frame-height*" *min-frame-height*))
       (unless (implies branch
                        (xor (tree-row-split tree)
                             (tree-column-split tree)))
         (complain "Inconsistent split"
                   :tree-row-split (tree-row-split tree)
                   :tree-column-split (tree-column-split tree)))
       (unless (implies branch
                        (tree-row-split tree)
                        (and (loop for i in tree always (= x (tree-x i)))
                             (loop for i in tree always (= width (tree-width i)))))
         (complain "Unequal x or width in row split"))
       (unless (implies branch
                        (tree-column-split tree)
                        (and (loop for i in tree always (= y (tree-y i)))
                             (loop for i in tree always (= height (tree-height i)))))
         (complain "Unequal y or height in column split"))
       (unless (implies branch
                        (= 2 (length tree)))
         (complain "Branch with other than 2 children"))
       (unless (implies branch
                        (tree-row-split tree)
                        (let ((f1 (first tree))
                              (f2 (second tree)))
                          (= (+ (tree-y f1) (tree-height f1)) (tree-y f2))))
         (complain "Bottom of first does not match top of second in row split"))
       (unless (implies branch
                        (tree-column-split tree)
                        (let ((f1 (first tree))
                              (f2 (second tree)))
                          (= (+ (tree-x f1) (tree-width f1)) (tree-x f2))))
         (complain "Right of first does not match left of second in column split"))
       (when branch
         (mapcar (lambda (child)
                   (get-invariant-violations-for-tile-group-and-tree tile-group child))
                 tree))))))

(defun get-invariant-violations-for-tile-group-and-head (tile-group head)
  (let ((tree (tile-group-frame-head tile-group head)))
    (get-invariant-violations-for-tile-group-and-tree tile-group tree)))

(defun get-invariant-violations-for-tile-group (tile-group)
  (with-complain-context ("tile group" tile-group)
    (let* ((frame-tree (tile-group-frame-tree tile-group))
           (frames (flatten frame-tree))
           ;; (last-frame (tile-group-last-frame tile-group))
           (current-frame (tile-group-current-frame tile-group))
           (screen (group-screen tile-group))
           (heads (screen-heads screen)))
      (list
       (unless (= (length frame-tree) (length heads))
         (complain "frame-tree length" (length frame-tree) "does not equal number of heads" (length heads)))
       (mapcar (lambda (head) (get-invariant-violations-for-tile-group-and-head tile-group head)) heads)
       ;; last-frame can be stale. When fixed, add this invariant.
       ;; (unless (or (eq last-frame nil)
       ;;             (and (typep last-frame 'frame)
       ;;                  (member last-frame frames)))
       ;;   (complain "Weird last-frame" :last-frame last-frame :frames frames))
       (unless (and (typep current-frame 'frame)
                    (member current-frame frames))
         (complain "Weird current-frame"))
       (unless (setp (mapcar #'frame-number frames))
         (complain "Frame numbers not unique within group"))
       ))))

(defun get-invariant-violations-for-group (group)
  (with-complain-context ("group" group)
    (let ((type-gtdf (list (typep group 'group)
                           (typep group 'tile-group)
                           (typep group 'dynamic-group)
                           (typep group 'float-group))))
      (cond
        ((equal type-gtdf '(t t nil nil))
         (get-invariant-violations-for-tile-group group))
        ((equal type-gtdf '(t nil t nil))
         (get-invariant-violations-for-dynamic-group group))
        ((equal type-gtdf '(t nil nil t))
         (get-invariant-violations-for-float-group group))
        (t (complain "Weird group typing"))))))

(defun get-invariant-violations-for-color (color)
  (declare (ignore color))
  nil ;; todo
  )

(defun get-invariant-violations-for-screen (screen)
  (with-complain-context ("screen" screen)
    (let ((groups (screen-groups screen))
          (current-group (screen-current-group screen)))
      (list
       (unless (member current-group groups)
         (complain "Weird current-group"))
       (unless (setp (mapcar #'group-number groups))
         (complain "Group numbers not unique within screen"))
       (mapcar (lambda (group) (unless (eq screen (group-screen group))
                                 (complain "Inconsistent screen for group")))
               groups)
       (mapcar (lambda (color-accessor)
                 (with-complain-context ("color fetched by" color-accessor)
                   (get-invariant-violations-for-color (funcall color-accessor screen))))
               '(screen-border-color
                 screen-fg-color
                 screen-bg-color
                 screen-win-bg-color
                 screen-focus-color
                 screen-unfocus-color
                 screen-float-focus-color
                 screen-float-unfocus-color))
       (mapcar #'get-invariant-violations-for-group groups)))))

(defun get-invariant-violations ()
  (collect-strings
   (with-complain-context ("root")
     (let ((screens *screen-list*))
       (list
        (unless (< 0 *resize-increment*)
          ;; This is used in ../testcases/test-check-invariants.sh to induce a violation
          (complain "Invalid *resize-increment*" *resize-increment*))
        (unless (member (current-screen) screens)
          (complain "Weird current-screen"))
        (unless (<= 1 (length screens))
          (complain "No screen"))
        (mapcar #'get-invariant-violations-for-screen screens))))))

;; Commands

(defcommand check-invariants () ()
            (let ((v (join-strings (get-invariant-violations))))
              (when v
                (format t "~&~A~&" v)
                (echo v))))

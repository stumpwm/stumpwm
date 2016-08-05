(in-package :stumpwm)

(export '(io-channel-ioport io-channel-events io-channel-handle
          *default-io-loop* io-loop io-loop-add io-loop-remove io-loop-update
          callback-channel callback-channel-stream callback-channel-events))

(defgeneric io-channel-ioport (io-loop channel))
(defgeneric io-channel-events (channel))
(defgeneric io-channel-handle (channel event &key &allow-other-keys))

(defgeneric io-loop-add (io-loop channel))
(defgeneric io-loop-remove (io-loop channel))
(defgeneric io-loop-update (io-loop channel))
(defgeneric io-loop (io-loop &key &allow-other-keys))

(defvar *default-io-loop* nil)

#+sbcl
(progn
  (defclass sbcl-io-loop ()
    ((channels :initform '())))

  (defmethod io-loop-add ((info sbcl-io-loop) channel)
    (with-slots (channels) info
      (when (find channel channels)
        (error "I/O channel is already registered"))
      (push channel channels)))

  (defmethod io-loop-remove ((info sbcl-io-loop) channel)
    (with-slots (channels) info
      (when (not (find channel channels))
        (error "I/O channel is not currently registered"))
      (setf channels (delete channel channels))))

  (defmethod io-loop-update ((info sbcl-io-loop) channel)
    (declare (ignore info channel)))

  (defmethod io-loop ((info sbcl-io-loop) &key description)
    (with-simple-restart (quit-ioloop "Quit I/O loop~A"
                                      (if description
                                          (format nil " (~A)" description)
                                          ""))
      (block io-loop
        (loop
           (with-simple-restart (restart-ioloop "Restart at I/O loop~A"
                                                (if description
                                                    (format nil " (~A)" description)
                                                    ""))
             (let ((ch-map (make-hash-table :test 'eql)) (rfds 0) (wfds 0) (maxfd 0)
                   (timeouts '())
                   (loop-ch '()))
               (let ((remove '()))
                 (dolist (channel (slot-value info 'channels))
                   (let ((fd (io-channel-ioport info channel)))
                     (let ((events (io-channel-events channel)))
                       (if events
                           (dolist (event (io-channel-events channel))
                             (multiple-value-bind (event data)
                                 (if (consp event) (values (car event) (cdr event)) (values event nil))
                               (case event
                                 (:read
                                  (setf maxfd (max maxfd fd)
                                        rfds (logior rfds (ash 1 fd)))
                                  (push (cons :read channel) (gethash fd ch-map '())))
                                 (:write
                                  (setf maxfd (max maxfd fd)
                                        wfds (logior wfds (ash 1 fd)))
                                  (push (cons :write channel) (gethash fd ch-map '())))
                                 (:timeout
                                  (let ((to (car data)))
                                    (push (cons to channel) timeouts)))
                                 (:loop
                                  (push channel loop-ch)))))
                           (push channel remove)))))
                 (dolist (channel remove)
                   (io-loop-remove info channel))
                 (unless (slot-value info 'channels)
                   (return-from io-loop)))
               (dolist (channel loop-ch)
                 (io-channel-handle channel :loop))
               (setf timeouts (sort timeouts '< :key 'car))
               (multiple-value-bind (to-sec to-usec)
                   (if timeouts
                       (let ((left (max (round (* (/ (- (car (first timeouts)) (get-internal-real-time))
                                                     internal-time-units-per-second)
                                                  1000000))
                                        0)))
                         (floor left 1000000))
                       (values nil nil))
                 (multiple-value-bind (result rfds wfds efds)
                     (sb-unix:unix-select (1+ maxfd)
                                          rfds wfds rfds to-sec to-usec)
                   (cond ((null result)
                          (let ((errno rfds))
                            (cond ((eql errno sb-unix:eintr)
                                   nil)
                                  (t (error "Unexpected ~S error: ~A" 'sb-unix:unix-select (sb-int:strerror errno))))))
                         ((> result 0)
                          (let ((afds (logior rfds wfds efds)))
                            (maphash (lambda (fd evs)
                                       (when (not (= (logand afds (ash 1 fd)) 0))
                                         (let ((r (not (= (logand rfds (ash 1 fd)) 0)))
                                               (w (not (= (logand wfds (ash 1 fd)) 0)))
                                               (e (not (= (logand efds (ash 1 fd)) 0))))
                                           (dolist (ev evs)
                                             (cond ((and (eq (car ev) :read)
                                                         (or r e))
                                                    (io-channel-handle (cdr ev) :read))
                                                   ((and (eq (car ev) :write)
                                                         w)
                                                    (io-channel-handle (cdr ev) :write)))))))
                                     ch-map))))))
               (when timeouts
                 (block timeouts
                   (let ((now (get-internal-real-time)))
                     (dolist (to timeouts)
                       (if (<= (car to) now)
                           (io-channel-handle (cdr to) :timeout)
                           (return-from timeouts))))))))))))

  (defmethod io-channel-ioport (io-loop (channel sb-sys:fd-stream))
    (declare (ignore io-loop))
    (sb-sys:fd-stream-fd channel))
  (defmethod io-channel-ioport ((io-loop sbcl-io-loop) (channel xlib:display))
    (io-channel-ioport io-loop (xlib::display-input-stream channel))))

(defclass xlib-io-loop ()
  ((display-ch :initform nil)
   (display :initform nil)
   (timers :initform '())))

(defmethod io-loop-add ((info xlib-io-loop) channel)
  (let ((fd (io-channel-ioport info channel)))
    (cond
      ((and (listp fd) (eq (first fd) :display))
       (with-slots (display-ch display) info
         (when display-ch
           (error "Dummy I/O loop implementation only supports one XLIB display"))
         (setf display-ch channel
               display (second fd))))
      ((null fd)
       (with-slots (timers) info
         (when (find channel timers)
           (error "Timer channel is already registered"))
         (push channel timers)))
      (t (error "Non-display, non-pure-timeout channels not supported by dummy I/O loop")))))

(defmethod io-loop-remove ((info xlib-io-loop) channel)
  (with-slots (display display-ch timers) info
    (cond ((eq display-ch channel)
           (setf display-ch nil
                 display nil))
          ((find channel timers)
           (setf timers (delete channel timers)))
          (t (error "I/O channel is not currently registered")))))

(defmethod io-loop-update ((info xlib-io-loop) channel)
  (declare (ignore info channel)))

(defmethod io-loop ((info xlib-io-loop) &key description)
  (with-simple-restart (quit-ioloop "Quit I/O loop~A"
                                    (if description
                                        (format nil " (~A)" description)
                                        ""))
    (labels ((channel-timeout (ch)
               (let ((evs (io-channel-events ch)))
                 (second (find :timeout evs :key (lambda (ev) (and (listp ev) (car ev)))))))
             (next-timeout ()
               (let ((timers (remove nil (mapcar #'channel-timeout (slot-value info 'timers)))))
                 (and timers
                      (max (/ (- (apply 'min timers)
                                 (get-internal-real-time))
                              internal-time-units-per-second)
                           0)))))
      (block io-loop
        (loop
           (with-simple-restart (restart-ioloop "Restart at I/O loop~A"
                                                (if description
                                                    (format nil " (~A)" description)
                                                    ""))
             (with-slots (display-ch display timers) info
               (let ((rem-ch '()))
                 (dolist (channel timers)
                   (let ((evs (io-channel-events channel)))
                     (cond ((null evs)
                            (push channel rem-ch))
                           ((find :loop evs)
                            (io-channel-handle channel :loop)))))
                 (dolist (channel rem-ch)
                   (io-loop-remove info channel)))
               (let ((timeout (next-timeout)))
                 (cond (display-ch
                        (io-channel-handle display-ch :loop)
                        (let ((nevents (xlib:event-listen display (and timeout (ceiling timeout)))))
                          (when nevents
                            (io-channel-handle display-ch :read))))
                       (timeout
                        (sleep timeout))
                       (t (return-from io-loop))))
               (when timers
                 (let ((now (get-internal-real-time)))
                   (dolist (channel timers)
                     (when (< (channel-timeout channel) now)
                       (io-channel-handle channel :timeout))))))))))))

(defmethod io-channel-ioport ((io-loop xlib-io-loop) (channel xlib:display))
  (list :display channel))

#+sbcl (setf *default-io-loop* 'sbcl-io-loop)
#-sbcl (setf *default-io-loop* 'xlib-io-loop)

(defmethod io-channel-ioport (io-loop (channel synonym-stream))
  (io-channel-ioport io-loop (symbol-value (synonym-stream-symbol channel))))

(defmethod io-channel-handle (channel event &key &allow-other-keys)
  (declare (ignore channel event)))

(defclass callback-channel ()
  ((current :initform nil)
   (stream :initarg :stream :reader callback-channel-stream)
   (read-function :initform nil :initarg :read)
   (write-function :initform nil :initarg :write)
   (events :initform :auto :initarg :events :accessor callback-channel-events)))

(defmethod io-loop-add :before (info (channel callback-channel))
  (when (slot-value channel 'current)
    (error "Callback channel is already registered with an I/O loop")))
(defmethod io-loop-add :after (info (channel callback-channel))
  (setf (slot-value channel 'current) info))
(defmethod io-loop-remove :after (info (channel callback-channel))
  (setf (slot-value channel 'current) nil))

(defmethod io-channel-ioport (io-loop (channel callback-channel))
  (io-channel-ioport io-loop (slot-value channel 'stream)))
(defmethod io-channel-events ((channel callback-channel))
  (with-slots (events) channel
    (if (eq events :auto)
        (let ((ret '()))
          (when (slot-value channel 'read-function)
            (push :read ret))
          (when (slot-value channel 'write-function)
            (push :write ret))
          ret)
        events)))
(defmethod io-channel-handle ((channel callback-channel) (event (eql :read)) &key)
  (funcall (slot-value channel 'read-function) channel))

(defmethod (setf callback-channel-events) (events channel)
  (setf (slot-value channel 'events) events)
  (with-slots (current) channel
    (when current (io-loop-update current channel))))

;;;; Copyright (C) 2016  Fredrik Tolf <fredrik@dolda2000.com>
;;;;
;;;;  This file is part of stumpwm.
;;;;
;;;; stumpwm is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.

;;;; stumpwm is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, see
;;;; <http://www.gnu.org/licenses/>.

(in-package :stumpwm)

;;;; This file implements a generic multiplexing I/O loop for listening
;;;; to I/O events from multiple sources. The model is as follows:
;;;;
;;;; An I/O multiplexer is represented as an object, with which I/O
;;;; channels can be registered to be monitored for events when the I/O
;;;; loop runs. An I/O channel is any object for which the generic
;;;; functions IO-CHANNEL-IOPORT, IO-CHANNEL-EVENTS and
;;;; IO-CHANNEL-HANDLE are implemented.
;;;;
;;;; IO-CHANNEL-IOPORT, given an I/O multiplexer and an I/O channel,
;;;; should return the underlying system I/O facility that the channel
;;;; operates on. The actual objects used to represent an I/O facility
;;;; depends on the Lisp implementation, operating system and the
;;;; specific I/O loop implementation, but, for example, on Unix
;;;; implementations they will likely be numeric file descriptors. The
;;;; I/O loop implementation implements IO-CHANNEL-IOPORT methods for
;;;; the facilities it understands (such as FD-STREAMs on SBCL), so
;;;; user-implemented channels should simply call IO-CHANNEL-IOPORT
;;;; recursively on whatever it operates on.
;;;;
;;;; IO-CHANNEL-EVENTS, given an I/O channel, should return a list of
;;;; the events that the channel is interested in. See the
;;;; documentation for IO-CHANNEL-EVENTS for further details.
;;;;
;;;; The I/O loop guarantees that it will check what events a channel
;;;; is interested in when it is first registered, and also at any time
;;;; the channel has been notified of an event. If the channel changes
;;;; its mind at any other point in time, it should use the
;;;; IO-LOOP-UPDATE function to notify the I/O loop of such
;;;; changes. The I/O loop may very well also update spuriously at
;;;; other times, but such updates are not guaranteed.
;;;;
;;;; IO-CHANNEL-HANDLE is called by the I/O loop to notify a channel of
;;;; an event.
;;;;
;;;; An I/O multiplexer is created with a MAKE-INSTANCE call on the
;;;; class of the desired multiplexer implementation. If the code using
;;;; the multiplexer has no certain preferences on an implementation
;;;; (which should be the usual case), the variable *DEFAULT-IO-LOOP*
;;;; points to a class that should be generally optimal given the
;;;; current Lisp implementation and operating system.
;;;;
;;;; Given a multiplexer, channels can be registered with it using
;;;; IO-LOOP-ADD, unregistered with IO-LOOP-REMOVE, and updated with
;;;; IO-LOOP-UPDATE (as described above). Call IO-LOOP on the
;;;; multiplexer to actually run it.

(export '(io-channel-ioport io-channel-events io-channel-handle
          *default-io-loop* *current-io-loop* *current-io-channel*
          io-loop io-loop-add io-loop-remove io-loop-update
          callback-channel callback-channel-stream callback-channel-events))

;;; General interface
(defgeneric io-channel-ioport (io-loop channel)
  (:documentation
   "Returns the I/O facility operated on by CHANNEL, in a
  representation understood by IO-LOOP. CHANNEL may be either an I/O
  channel or an object representing an underlying I/O facility, such
  as a stream object. An I/O loop implementation should implement
  methods for any primitive I/O facilities that it can monitor for
  events, and abstract channels should return whatever
  IO-CHANNEL-IOPORT returns for the primitive facility that it
  operates on.

  An I/O channel may also return NIL to indicate that it is only
  interested in purely virtual events, such as :TIMEOUT or :LOOP."))
(defgeneric io-channel-events (channel)
  (:documentation
   "Returns a list of events that CHANNEL is interested in. An event
  specification may be a simple symbol, or a list of a symbol and
  additional data for the event. Specific I/O loop implementations may
  implement additional events, but the following event specifications
  should be supported by all I/O loops:

      :READ -- The channel will be notified when its I/O port can be
      read from without blocking.

      :WRITE -- The channel will be notified when its I/O port can
      be written to without blocking.

      (:TIMEOUT TIME-SPEC) -- TIME-SPEC is a point in time in the
      same units as from (GET-INTERNAL-REAL-TIME), at which point
      the channel will be notified. It is permissible for TIME-SPEC
      to be a real number of any representation, but the system does
      not guarantee any particular level of accuracy.

      :LOOP -- The channel will be notifed for each iteration of the
      I/O loop, just before blocking for incoming events. This should
      be considered a hack to be avoided, but may be useful for
      certain libraries (such as XLIB).

  If, at any time, an empty list is returned, the channel is
  unregistered with the I/O loop.

  The I/O loop will check what events a channel is interested in when
  it is first registered with the loop, and whenever the channel has
  been notified of an event. If the channel changes its mind at any
  other point in time, it should use the IO-LOOP-UPDATE function to
  notify the I/O loop of such changes. The I/O loop may also update
  spuriously at any time, but such updates are not guaranteed."))
(defgeneric io-channel-handle (channel event &key &allow-other-keys)
  (:documentation
   "Called by the I/O loop to notify a channel that an event has
  occurred. EVENT is the symbol corresponding to the event
  specification from IO-CHANNEL-EVENTS (that is, :READ, :WRITE,
  :TIMEOUT or :LOOP). A number of keyword arguments with additional
  data specific to a certain event may also be passed, but no such
  arguments are currently defined."))

(defgeneric io-loop-add (io-loop channel)
  (:documentation "Add a channel to the given I/O multiplexer to be monitored."))
(defgeneric io-loop-remove (io-loop channel)
  (:documentation "Unregister a channel from the I/O multiplexer."))
(defgeneric io-loop-update (io-loop channel)
  (:documentation "Make the I/O loop update its knowledge of what
  events CHANNEL is interested in. See the documentation for
  IO-CHANNEL-EVENTS for more information."))
(defgeneric io-loop (io-loop &key &allow-other-keys)
  (:documentation "Run the given I/O multiplexer, watching for events
  on any channels registered with it. IO-LOOP will return when it has
  no channels left registered with it."))

(defvar *default-io-loop* nil
  "The default I/O loop implementation. Should be generically optimal
  for the given LISP implementation and operating system.")

(defvar *current-io-loop* nil
  "Dynamically bound to the I/O loop currently running, providing an
  easy way for event callbacks to register new channels.")

(defvar *current-io-channel* nil
  "While processing an I/O channel, this variable is dynamically bound
  to the channel in question. This is provided primarily for
  error-handling code.")

;; Default methods for the above
(defmethod io-channel-handle (channel event &key &allow-other-keys)
  (declare (ignore channel event)))

;;; SBCL implementation
;;;
;;; It would be generally nice if SBCL supported epoll/kqueue, but it
;;; doesn't. The general I/O loop interface is consistent with such
;;; implementations, however, so if support is added at any time, it
;;; could be supported fairly easily.
;;;
;;; If need should arise, it should also be quite simple to add
;;; thread-safe operation.
#+sbcl
(progn
  (defclass sbcl-io-loop ()
    ((channels :initform '()))
    (:documentation
     "Implements a select(2)-based I/O loop for SBCL. The
  implementation is not particularly optimal, mostly because any
  efficiency ambitions are mostly pointless as long as SBCL lacks
  support for epoll/kqueue, but should work well enough for I/O loops
  with relatively few channels.

  The implementation currently supports monitoring SB-SYS:FD-STREAM
  and XLIB:DISPLAY objects."))

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
    (let ((*current-io-loop* info))
      (with-simple-restart (:quit-ioloop "Quit I/O loop~A"
                                         (if description
                                             (format nil " (~A)" description)
                                             ""))
        (block io-loop
          (loop
             (with-simple-restart (:restart-ioloop "Restart at I/O loop~A"
                                                   (if description
                                                       (format nil " (~A)" description)
                                                       ""))
               (macrolet ((with-channel-restarts ((channel &optional remove-code) &body body)
                            (let ((ch (gensym "CHANNEL")))
                              `(let* ((,ch ,channel)
                                      (*current-io-channel* ,ch))
                                 (restart-case
                                     (progn ,@body)
                                   (:skip-channel ()
                                     :report (lambda (s)
                                               (format s "Continue as if without channel ~S" ,ch))
                                     nil)
                                   (:remove-channel ()
                                     :report (lambda (s)
                                               (format s "Unregister channel ~S and continue" ,ch))
                                     ,(or remove-code `(io-loop-remove info ,ch))
                                     nil))))))
                 (let ((ch-map (make-hash-table :test 'eql)) (rfds 0) (wfds 0) (maxfd 0)
                       (timeouts '())
                       (loop-ch '()))
                   ;; Since it is select(2)-based, this implementation
                   ;; updates the entire set of interesting events once
                   ;; every iteration.
                   (let ((remove '()))
                     (dolist (channel (slot-value info 'channels))
                       (with-channel-restarts (channel (push channel remove))
                         (let ((fd (io-channel-ioport info channel)))
                           (let ((events (io-channel-events channel)))
                             (if events
                                 (dolist (event events)
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
                                        (let ((timeout (car data)))
                                          (check-type timeout real)
                                          (push (cons timeout channel) timeouts)))
                                       (:loop
                                        (push channel loop-ch)))))
                                 (push channel remove))))))
                     (dolist (channel remove)
                       (io-loop-remove info channel))
                     (unless (slot-value info 'channels)
                       (return-from io-loop)))
                   ;; Call any :LOOP channels
                   (dolist (channel loop-ch)
                     (with-channel-restarts (channel)
                       (io-channel-handle channel :loop)))
                   (setf timeouts (sort timeouts '< :key 'car))
                   (multiple-value-bind (to-sec to-usec)
                       (if timeouts
                           (let ((left (max (round (* (/ (- (car (first timeouts)) (get-internal-real-time))
                                                         internal-time-units-per-second)
                                                      1000000))
                                            0)))
                             (floor left 1000000))
                           (values nil nil))
                     ;; Actually block for events
                     (multiple-value-bind (result rfds wfds efds)
                         (sb-unix:unix-select (1+ maxfd)
                                              rfds wfds rfds to-sec to-usec)
                       (cond ((null result)
                              (let ((errno rfds))
                                (cond ((eql errno sb-unix:eintr)
                                       nil)
                                      (t (error "Unexpected ~S error: ~A" 'sb-unix:unix-select (sb-int:strerror errno))))))
                             ((> result 0)
                              ;; Notify channels for transpired events
                              (let ((afds (logior rfds wfds efds)))
                                (maphash (lambda (fd evs)
                                           (when (not (= (logand afds (ash 1 fd)) 0))
                                             (let ((r (not (= (logand rfds (ash 1 fd)) 0)))
                                                   (w (not (= (logand wfds (ash 1 fd)) 0)))
                                                   (e (not (= (logand efds (ash 1 fd)) 0))))
                                               (dolist (ev evs)
                                                 (with-channel-restarts ((cdr ev))
                                                   (cond ((and (eq (car ev) :read)
                                                               (or r e))
                                                          (io-channel-handle (cdr ev) :read))
                                                         ((and (eq (car ev) :write)
                                                               w)
                                                          (io-channel-handle (cdr ev) :write))))))))
                                         ch-map))))))
                   ;; Check for timeouts
                   (when timeouts
                     (block timeouts
                       (let ((now (get-internal-real-time)))
                         (dolist (to timeouts)
                           (if (<= (car to) now)
                               (with-channel-restarts ((cdr to))
                                 (io-channel-handle (cdr to) :timeout))
                               (return-from timeouts))))))))))))))

  ;;; IO-CHANNEL-IOPORT methods for support facilities
  (defmethod io-channel-ioport (io-loop (channel sb-sys:fd-stream))
    (declare (ignore io-loop))
    (sb-sys:fd-stream-fd channel))
  (defmethod io-channel-ioport ((io-loop sbcl-io-loop) (channel xlib:display))
    (io-channel-ioport io-loop (xlib::display-input-stream channel))))

;;; Dummy XLIB I/O loop
(defclass xlib-io-loop ()
  ((display-ch :initform nil)
   (display :initform nil)
   (timers :initform '()))
  (:documentation
   "Implements a \"dummy\" I/O loop for Lisps lacking an actual
  implementation. The dummy loop should be sufficient for StumpWM
  usage, but lacks support for listening to multiple I/O channels. It
  supports monitoring exactly one XLIB:DISPLAY object, and any number
  of virtual channels."))

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
  (let ((*current-io-loop* info))
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
                         (io-channel-handle channel :timeout)))))))))))))

(defmethod io-channel-ioport ((io-loop xlib-io-loop) (channel xlib:display))
  (list :display channel))

;;; Select preferred I/O loop implementation depending on environment:
#+sbcl (setf *default-io-loop* 'sbcl-io-loop)
#-sbcl (setf *default-io-loop* 'xlib-io-loop)

;;; Default methods for widely supported objects
(defmethod io-channel-ioport (io-loop (channel synonym-stream))
  (io-channel-ioport io-loop (symbol-value (synonym-stream-symbol channel))))

;;; Callback channel implementation
(defclass callback-channel ()
  ((current :initform nil)
   (stream :initarg :stream :reader callback-channel-stream)
   (read-function :initform nil :initarg :read)
   (write-function :initform nil :initarg :write)
   (events :initform :auto :initarg :events :accessor callback-channel-events))
  (:documentation
   "Implements a convenience I/O channel which takes an underlying I/O
   facility and calls the given callback functions when an event
   occurs on the channel. The :STREAM init-argument specifies the I/O
   facility to monitor, :READ specifies a function to be called back
   when a read event occurs, and :WRITE a corresponding function for
   write events. Timeouts are not supported.

   By default, the channel will listen for read events iff a read
   callback function is given and correspondingly for write events,
   but CALLBACK-CHANNEL-EVENTS can be SETF'd to specify events
   explicitly in case certain events are only interesting
   sporadically. To restore default behavior, set it to :AUTO."))

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

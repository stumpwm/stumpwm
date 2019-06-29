;;;;
;;;; This is quicklisp.lisp, the quickstart file for Quicklisp. To use
;;;; it, start Lisp, then (load "quicklisp.lisp")
;;;;
;;;; Quicklisp is beta software and comes with no warranty of any kind.
;;;;
;;;; For more information about the Quicklisp beta, see:
;;;;
;;;;    http://www.quicklisp.org/beta/
;;;;
;;;; If you have any questions or comments about Quicklisp, please
;;;; contact:
;;;;
;;;;    Zach Beane <zach@quicklisp.org>
;;;;

(cl:in-package #:cl-user)
(cl:defpackage #:qlqs-user
  (:use #:cl))
(cl:in-package #:qlqs-user)

(defpackage #:qlqs-info
  (:export #:*version*))

(defvar qlqs-info:*version* "2015-01-28")

(defpackage #:qlqs-impl
  (:use #:cl)
  (:export #:*implementation*)
  (:export #:definterface
           #:defimplementation)
  (:export #:lisp
           #:abcl
           #:allegro
           #:ccl
           #:clasp
           #:clisp
           #:cmucl
           #:cormanlisp
           #:ecl
           #:gcl
           #:lispworks
	   #:mkcl
           #:scl
           #:sbcl))

(defpackage #:qlqs-impl-util
  (:use #:cl #:qlqs-impl)
  (:export #:call-with-quiet-compilation))

(defpackage #:qlqs-network
  (:use #:cl #:qlqs-impl)
  (:export #:open-connection
           #:write-octets
           #:read-octets
           #:close-connection
           #:with-connection))

(defpackage #:qlqs-progress
  (:use #:cl)
  (:export #:make-progress-bar
           #:start-display
           #:update-progress
           #:finish-display))

(defpackage #:qlqs-http
  (:use #:cl #:qlqs-network #:qlqs-progress)
  (:export #:fetch
           #:*proxy-url*
           #:*maximum-redirects*
           #:*default-url-defaults*))

(defpackage #:qlqs-minitar
  (:use #:cl)
  (:export #:unpack-tarball))

(defpackage #:quicklisp-quickstart
  (:use #:cl #:qlqs-impl #:qlqs-impl-util #:qlqs-http #:qlqs-minitar)
  (:export #:install
           #:help
           #:*proxy-url*
           #:*asdf-url*
           #:*quicklisp-tar-url*
           #:*setup-url*
           #:*help-message*
           #:*after-load-message*
           #:*after-initial-setup-message*))


;;;
;;; Defining implementation-specific packages and functionality
;;;

(in-package #:qlqs-impl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun error-unimplemented (&rest args)
    (declare (ignore args))
    (error "Not implemented")))

(defmacro neuter-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((definition (fdefinition 'error-unimplemented)))
       (do-external-symbols (symbol ,(string name))
         (unless (fboundp symbol)
           (setf (fdefinition symbol) definition))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun feature-expression-passes-p (expression)
    (cond ((keywordp expression)
           (member expression *features*))
          ((consp expression)
           (case (first expression)
             (or
              (some 'feature-expression-passes-p (rest expression)))
             (and
              (every 'feature-expression-passes-p (rest expression)))))
          (t (error "Unrecognized feature expression -- ~S" expression)))))


(defmacro define-implementation-package (feature package-name &rest options)
  (let* ((output-options '((:use)
                           (:export #:lisp)))
         (prep (cdr (assoc :prep options)))
         (class-option (cdr (assoc :class options)))
         (class (first class-option))
         (superclasses (rest class-option))
         (import-options '())
         (effectivep (feature-expression-passes-p feature)))
    (dolist (option options)
      (ecase (first option)
        ((:prep :class))
        ((:import-from
          :import)
         (push option import-options))
        ((:export
          :shadow
          :intern
          :documentation)
         (push option output-options))
        ((:reexport-from)
         (push (cons :export (cddr option)) output-options)
         (push (cons :import-from (cdr option)) import-options))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(when effectivep
               prep)
       (defclass ,class ,superclasses ())
       (defpackage ,package-name ,@output-options
                   ,@(when effectivep
                           import-options))
       ,@(when effectivep
               `((setf *implementation* (make-instance ',class))))
       ,@(unless effectivep
                 `((neuter-package ,package-name))))))

(defmacro definterface (name lambda-list &body options)
  (let* ((forbidden (intersection lambda-list lambda-list-keywords))
         (gf-options (remove :implementation options :key #'first))
         (implementations (set-difference options gf-options)))
    (when forbidden
      (error "~S not allowed in definterface lambda list" forbidden))
    (flet ((method-option (class body)
             `(:method ((*implementation* ,class) ,@lambda-list)
                ,@body)))
      (let ((generic-name (intern (format nil "%~A" name))))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (defgeneric ,generic-name (lisp ,@lambda-list)
             ,@gf-options
             ,@(mapcar (lambda (implementation)
                         (destructuring-bind (class &rest body)
                             (rest implementation)
                           (method-option class body)))
                       implementations))
           (defun ,name ,lambda-list
             (,generic-name *implementation* ,@lambda-list)))))))

(defmacro defimplementation (name-and-options
                             lambda-list &body body)
  (destructuring-bind (name &key (for t) qualifier)
      (if (consp name-and-options)
          name-and-options
          (list name-and-options))
    (unless for
      (error "You must specify an implementation name."))
    (let ((generic-name (find-symbol (format nil "%~A" name))))
      (unless (and generic-name
                   (fboundp generic-name))
        (error "~S does not name an implementation function" name))
      `(defmethod ,generic-name
           ,@(when qualifier (list qualifier))
         ,(list* `(*implementation* ,for) lambda-list) ,@body))))


;;; Bootstrap implementations

(defvar *implementation* nil)
(defclass lisp () ())


;;; Allegro Common Lisp

(define-implementation-package :allegro #:qlqs-allegro
  (:documentation
   "Allegro Common Lisp - http://www.franz.com/products/allegrocl/")
  (:class allegro)
  (:reexport-from #:socket
                  #:make-socket)
  (:reexport-from #:excl
                  #:read-vector))


;;; Armed Bear Common Lisp

(define-implementation-package :abcl #:qlqs-abcl
  (:documentation
   "Armed Bear Common Lisp - http://common-lisp.net/project/armedbear/")
  (:class abcl)
  (:reexport-from #:system
                  #:make-socket
                  #:get-socket-stream))

;;; Clozure CL

(define-implementation-package :ccl #:qlqs-ccl
  (:documentation
   "Clozure Common Lisp - http://www.clozure.com/clozurecl.html")
  (:class ccl)
  (:reexport-from #:ccl
                  #:make-socket))


;;; CLASP

(define-implementation-package :clasp #:qlqs-clasp
  (:documentation "CLASP - http://github.com/drmeister/clasp")
  (:class clasp)
  (:prep
   (require 'sockets))
  (:intern #:host-network-address)
  (:reexport-from #:sb-bsd-sockets
                  #:get-host-by-name
                  #:host-ent-address
                  #:socket-connect
                  #:socket-make-stream
                  #:inet-socket))


;;; GNU CLISP

(define-implementation-package :clisp #:qlqs-clisp
  (:documentation "GNU CLISP - http://clisp.cons.org/")
  (:class clisp)
  (:reexport-from #:socket
                  #:socket-connect)
  (:reexport-from #:ext
                  #:read-byte-sequence))


;;; CMUCL

(define-implementation-package :cmu #:qlqs-cmucl
  (:documentation "CMU Common Lisp - http://www.cons.org/cmucl/")
  (:class cmucl)
  (:reexport-from #:ext
                  #:*gc-verbose*)
  (:reexport-from #:system
                  #:make-fd-stream)
  (:reexport-from #:extensions
                  #:connect-to-inet-socket))

(defvar qlqs-cmucl:*gc-verbose* nil)


;;; Scieneer CL

(define-implementation-package :scl #:qlqs-scl
  (:documentation "Scieneer Common Lisp - http://www.scieneer.com/scl/")
  (:class scl)
  (:reexport-from #:system
                  #:make-fd-stream)
  (:reexport-from #:extensions
                  #:connect-to-inet-socket))

;;; ECL

(define-implementation-package :ecl #:qlqs-ecl
  (:documentation "ECL - http://ecls.sourceforge.net/")
  (:class ecl)
  (:prep
   (require 'sockets))
  (:intern #:host-network-address)
  (:reexport-from #:sb-bsd-sockets
                  #:get-host-by-name
                  #:host-ent-address
                  #:socket-connect
                  #:socket-make-stream
                  #:inet-socket))


;;; LispWorks

(define-implementation-package :lispworks #:qlqs-lispworks
  (:documentation "LispWorks - http://www.lispworks.com/")
  (:class lispworks)
  (:prep
   (require "comm"))
  (:reexport-from #:comm
                  #:open-tcp-stream
                  #:get-host-entry))


;;; SBCL

(define-implementation-package :sbcl #:qlqs-sbcl
  (:class sbcl)
  (:documentation
   "Steel Bank Common Lisp - http://www.sbcl.org/")
  (:prep
   (require 'sb-bsd-sockets))
  (:intern #:host-network-address)
  (:reexport-from #:sb-ext
                  #:compiler-note)
  (:reexport-from #:sb-bsd-sockets
                  #:get-host-by-name
                  #:inet-socket
                  #:host-ent-address
                  #:socket-connect
                  #:socket-make-stream))

;;; MKCL

(define-implementation-package :mkcl #:qlqs-mkcl
  (:class mkcl)
  (:documentation
   "ManKai Common Lisp - http://common-lisp.net/project/mkcl/")
  (:prep
   (require 'sockets))
  (:intern #:host-network-address)
  (:reexport-from #:sb-bsd-sockets
                  #:get-host-by-name
                  #:inet-socket
                  #:host-ent-address
                  #:socket-connect
                  #:socket-make-stream))

;;;
;;; Utility function
;;;

(in-package #:qlqs-impl-util)

(definterface call-with-quiet-compilation (fun)
  (:implementation t
    (let ((*load-verbose* nil)
          (*compile-verbose* nil)
          (*load-print* nil)
          (*compile-print* nil))
      (handler-bind ((warning #'muffle-warning))
        (funcall fun)))))

(defimplementation (call-with-quiet-compilation :for sbcl :qualifier :around)
    (fun)
  (declare (ignorable fun))
  (handler-bind ((qlqs-sbcl:compiler-note #'muffle-warning))
    (call-next-method)))

(defimplementation (call-with-quiet-compilation :for cmucl :qualifier :around)
    (fun)
  (declare (ignorable fun))
  (let ((qlqs-cmucl:*gc-verbose* nil))
    (call-next-method)))


;;;
;;; Low-level networking implementations
;;;

(in-package #:qlqs-network)

(definterface host-address (host)
  (:implementation t
    host)
  (:implementation mkcl
    (qlqs-mkcl:host-ent-address (qlqs-mkcl:get-host-by-name host)))
  (:implementation sbcl
    (qlqs-sbcl:host-ent-address (qlqs-sbcl:get-host-by-name host))))

(definterface open-connection (host port)
  (:implementation t
    (declare (ignorable host port))
    (error "Sorry, quicklisp in implementation ~S is not supported yet."
           (lisp-implementation-type)))
  (:implementation allegro
    (qlqs-allegro:make-socket :remote-host host
                             :remote-port port))
  (:implementation abcl
    (let ((socket (qlqs-abcl:make-socket host port)))
      (qlqs-abcl:get-socket-stream socket :element-type '(unsigned-byte 8))))
  (:implementation ccl
    (qlqs-ccl:make-socket :remote-host host
                         :remote-port port))
  (:implementation clasp
    (let* ((endpoint (qlqs-clasp:host-ent-address
                      (qlqs-clasp:get-host-by-name host)))
           (socket (make-instance 'qlqs-clasp:inet-socket
                                  :protocol :tcp
                                  :type :stream)))
      (qlqs-clasp:socket-connect socket endpoint port)
      (qlqs-clasp:socket-make-stream socket
                                  :element-type '(unsigned-byte 8)
                                  :input t
                                  :output t
                                  :buffering :full)))
  (:implementation clisp
    (qlqs-clisp:socket-connect port host :element-type '(unsigned-byte 8)))
  (:implementation cmucl
    (let ((fd (qlqs-cmucl:connect-to-inet-socket host port)))
      (qlqs-cmucl:make-fd-stream fd
                                :element-type '(unsigned-byte 8)
                                :binary-stream-p t
                                :input t
                                :output t)))
  (:implementation scl
    (let ((fd (qlqs-scl:connect-to-inet-socket host port)))
      (qlqs-scl:make-fd-stream fd
			       :element-type '(unsigned-byte 8)
			       :input t
			       :output t)))
  (:implementation ecl
    (let* ((endpoint (qlqs-ecl:host-ent-address
                      (qlqs-ecl:get-host-by-name host)))
           (socket (make-instance 'qlqs-ecl:inet-socket
                                  :protocol :tcp
                                  :type :stream)))
      (qlqs-ecl:socket-connect socket endpoint port)
      (qlqs-ecl:socket-make-stream socket
                                  :element-type '(unsigned-byte 8)
                                  :input t
                                  :output t
                                  :buffering :full)))
  (:implementation lispworks
    (qlqs-lispworks:open-tcp-stream host port
                                   :direction :io
                                   :errorp t
                                   :read-timeout nil
                                   :element-type '(unsigned-byte 8)
                                   :timeout 5))
  (:implementation mkcl
    (let* ((endpoint (qlqs-mkcl:host-ent-address
                      (qlqs-mkcl:get-host-by-name host)))
           (socket (make-instance 'qlqs-mkcl:inet-socket
                                  :protocol :tcp
                                  :type :stream)))
      (qlqs-mkcl:socket-connect socket endpoint port)
      (qlqs-mkcl:socket-make-stream socket
                                   :element-type '(unsigned-byte 8)
                                   :input t
                                   :output t
                                   :buffering :full)))
  (:implementation sbcl
    (let* ((endpoint (qlqs-sbcl:host-ent-address
                      (qlqs-sbcl:get-host-by-name host)))
           (socket (make-instance 'qlqs-sbcl:inet-socket
                                  :protocol :tcp
                                  :type :stream)))
      (qlqs-sbcl:socket-connect socket endpoint port)
      (qlqs-sbcl:socket-make-stream socket
                                   :element-type '(unsigned-byte 8)
                                   :input t
                                   :output t
                                   :buffering :full))))

(definterface read-octets (buffer connection)
  (:implementation t
    (read-sequence buffer connection))
  (:implementation allegro
    (qlqs-allegro:read-vector buffer connection))
  (:implementation clisp
    (qlqs-clisp:read-byte-sequence buffer connection
                                  :no-hang nil
                                  :interactive t)))

(definterface write-octets (buffer connection)
  (:implementation t
    (write-sequence buffer connection)
    (finish-output connection)))

(definterface close-connection (connection)
  (:implementation t
    (ignore-errors (close connection))))

(definterface call-with-connection (host port fun)
  (:implementation t
    (let (connection)
      (unwind-protect
           (progn
             (setf connection (open-connection host port))
             (funcall fun connection))
        (when connection
          (close connection))))))

(defmacro with-connection ((connection host port) &body body)
  `(call-with-connection ,host ,port (lambda (,connection) ,@body)))


;;;
;;; A text progress bar
;;;

(in-package #:qlqs-progress)

(defclass progress-bar ()
  ((start-time
    :initarg :start-time
    :accessor start-time)
   (end-time
    :initarg :end-time
    :accessor end-time)
   (progress-character
    :initarg :progress-character
    :accessor progress-character)
   (character-count
    :initarg :character-count
    :accessor character-count
    :documentation "How many characters wide is the progress bar?")
   (characters-so-far
    :initarg :characters-so-far
    :accessor characters-so-far)
   (update-interval
    :initarg :update-interval
    :accessor update-interval
    :documentation "Update the progress bar display after this many
    internal-time units.")
   (last-update-time
    :initarg :last-update-time
    :accessor last-update-time
    :documentation "The display was last updated at this time.")
   (total
    :initarg :total
    :accessor total
    :documentation "The total number of units tracked by this progress bar.")
   (progress
    :initarg :progress
    :accessor progress
    :documentation "How far in the progress are we?")
   (pending
    :initarg :pending
    :accessor pending
    :documentation "How many raw units should be tracked in the next
    display update?"))
  (:default-initargs
   :progress-character #\=
   :character-count 50
   :characters-so-far 0
   :update-interval (floor internal-time-units-per-second 4)
   :last-update-time 0
   :total 0
   :progress 0
   :pending 0))

(defgeneric start-display (progress-bar))
(defgeneric update-progress (progress-bar unit-count))
(defgeneric update-display (progress-bar))
(defgeneric finish-display (progress-bar))
(defgeneric elapsed-time (progress-bar))
(defgeneric units-per-second (progress-bar))

(defmethod start-display (progress-bar)
  (setf (last-update-time progress-bar) (get-internal-real-time))
  (setf (start-time progress-bar) (get-internal-real-time))
  (fresh-line)
  (finish-output))

(defmethod update-display (progress-bar)
  (incf (progress progress-bar) (pending progress-bar))
  (setf (pending progress-bar) 0)
  (setf (last-update-time progress-bar) (get-internal-real-time))
  (let* ((showable (floor (character-count progress-bar)
                          (/ (total progress-bar) (progress progress-bar))))
         (needed (- showable (characters-so-far progress-bar))))
    (setf (characters-so-far progress-bar) showable)
    (dotimes (i needed)
      (write-char (progress-character progress-bar)))
    (finish-output)))

(defmethod update-progress (progress-bar unit-count)
  (incf (pending progress-bar) unit-count)
  (let ((now (get-internal-real-time)))
    (when (< (update-interval progress-bar)
             (- now (last-update-time progress-bar)))
      (update-display progress-bar))))

(defmethod finish-display (progress-bar)
  (update-display progress-bar)
  (setf (end-time progress-bar) (get-internal-real-time))
  (terpri)
  (format t "~:D bytes in ~$ seconds (~$KB/sec)"
          (total progress-bar)
          (elapsed-time progress-bar)
          (/  (units-per-second progress-bar) 1024))
  (finish-output))

(defmethod elapsed-time (progress-bar)
  (/ (- (end-time progress-bar) (start-time progress-bar))
     internal-time-units-per-second))

(defmethod units-per-second (progress-bar)
  (if (plusp (elapsed-time progress-bar))
      (/ (total progress-bar) (elapsed-time progress-bar))
      0))

(defun kb/sec (progress-bar)
  (/ (units-per-second progress-bar) 1024))



(defparameter *uncertain-progress-chars* "?")

(defclass uncertain-size-progress-bar (progress-bar)
  ((progress-char-index
    :initarg :progress-char-index
    :accessor progress-char-index)
   (units-per-char
    :initarg :units-per-char
    :accessor units-per-char))
  (:default-initargs
   :total 0
   :progress-char-index 0
   :units-per-char (floor (expt 1024 2) 50)))

(defmethod update-progress :after ((progress-bar uncertain-size-progress-bar)
                            unit-count)
  (incf (total progress-bar) unit-count))

(defmethod progress-character ((progress-bar uncertain-size-progress-bar))
  (let ((index (progress-char-index progress-bar)))
    (prog1
        (char *uncertain-progress-chars* index)
      (setf (progress-char-index progress-bar)
            (mod (1+ index) (length *uncertain-progress-chars*))))))

(defmethod update-display ((progress-bar uncertain-size-progress-bar))
  (setf (last-update-time progress-bar) (get-internal-real-time))
  (multiple-value-bind (chars pend)
      (floor (pending progress-bar) (units-per-char progress-bar))
    (setf (pending progress-bar) pend)
    (dotimes (i chars)
      (write-char (progress-character progress-bar))
      (incf (characters-so-far progress-bar))
      (when (<= (character-count progress-bar)
                (characters-so-far progress-bar))
        (terpri)
        (setf (characters-so-far progress-bar) 0)
        (finish-output)))
    (finish-output)))

(defun make-progress-bar (total)
  (if (or (not total) (zerop total))
      (make-instance 'uncertain-size-progress-bar)
      (make-instance 'progress-bar :total total)))

;;;
;;; A simple HTTP client
;;;

(in-package #:qlqs-http)

;;; Octet data

(deftype octet ()
  '(unsigned-byte 8))

(defun make-octet-vector (size)
  (make-array size :element-type 'octet
              :initial-element 0))

(defun octet-vector (&rest octets)
  (make-array (length octets) :element-type 'octet
              :initial-contents octets))

;;; ASCII characters as integers

(defun acode (char)
  (cond ((eql char :cr)
         13)
        ((eql char :lf)
         10)
        (t
         (let ((code (char-code char)))
           (if (<= 0 code 127)
               code
               (error "Character ~S is not in the ASCII character set"
                      char))))))

(defvar *whitespace*
  (list (acode #\Space) (acode #\Tab) (acode :cr) (acode :lf)))

(defun whitep (code)
  (member code *whitespace*))

(defun ascii-vector (string)
  (let ((vector (make-octet-vector (length string))))
    (loop for char across string
          for code = (char-code char)
          for i from 0
          if (< 127 code) do
          (error "Invalid character for ASCII -- ~A" char)
          else
          do (setf (aref vector i) code))
    vector))

(defun ascii-subseq (vector start end)
  "Return a subseq of octet-specialized VECTOR as a string."
  (let ((string (make-string (- end start))))
    (loop for i from 0
          for j from start below end
          do (setf (char string i) (code-char (aref vector j))))
    string))

(defun ascii-downcase (code)
  (if (<= 65 code 90)
      (+ code 32)
      code))

(defun ascii-equal (a b)
  (eql (ascii-downcase a) (ascii-downcase b)))

(defmacro acase (value &body cases)
  (flet ((convert-case-keys (keys)
           (mapcar (lambda (key)
                     (etypecase key
                       (integer key)
                       (character (char-code key))
                       (symbol
                        (ecase key
                          (:cr 13)
                          (:lf 10)
                          ((t) t)))))
                   (if (consp keys) keys (list keys)))))
    `(case ,value
       ,@(mapcar (lambda (case)
                   (destructuring-bind (keys &rest body)
                       case
                     `(,(if (eql keys t)
                            t
                            (convert-case-keys keys))
                        ,@body)))
                 cases))))

;;; Pattern matching (for finding headers)

(defclass matcher ()
  ((pattern
    :initarg :pattern
    :reader pattern)
   (pos
    :initform 0
    :accessor match-pos)
   (matchedp
    :initform nil
    :accessor matchedp)))

(defun reset-match (matcher)
  (setf (match-pos matcher) 0
        (matchedp matcher) nil))

(define-condition match-failure (error) ())

(defun match (matcher input &key (start 0) end error)
  (let ((i start)
        (end (or end (length input)))
        (match-end (length (pattern matcher))))
    (with-slots (pattern pos)
        matcher
      (loop
       (cond ((= pos match-end)
              (let ((match-start (- i pos)))
                (setf pos 0)
                (setf (matchedp matcher) t)
                (return (values match-start (+ match-start match-end)))))
             ((= i end)
              (return nil))
             ((= (aref pattern pos)
                 (aref input i))
              (incf i)
              (incf pos))
             (t
              (if error
                  (error 'match-failure)
                  (if (zerop pos)
                      (incf i)
                      (setf pos 0)))))))))

(defun ascii-matcher (string)
  (make-instance 'matcher
                 :pattern (ascii-vector string)))

(defun octet-matcher (&rest octets)
  (make-instance 'matcher
                 :pattern (apply 'octet-vector octets)))

(defun acode-matcher (&rest codes)
  (make-instance 'matcher
                 :pattern (make-array (length codes)
                                      :element-type 'octet
                                      :initial-contents
                                      (mapcar 'acode codes))))


;;; "Connection Buffers" are a kind of callback-driven,
;;; pattern-matching chunky stream. Callbacks can be called for a
;;; certain number of octets or until one or more patterns are seen in
;;; the input. cbufs automatically refill themselves from a
;;; connection as needed.

(defvar *cbuf-buffer-size* 8192)

(define-condition end-of-data (error) ())

(defclass cbuf ()
  ((data
    :initarg :data
    :accessor data)
   (connection
    :initarg :connection
    :accessor connection)
   (start
    :initarg :start
    :accessor start)
   (end
    :initarg :end
    :accessor end)
   (eofp
    :initarg :eofp
    :accessor eofp))
  (:default-initargs
   :data (make-octet-vector *cbuf-buffer-size*)
   :connection nil
   :start 0
   :end 0
   :eofp nil)
  (:documentation "A CBUF is a connection buffer that keeps track of
  incoming data from a connection. Several functions make it easy to
  treat a CBUF as a kind of chunky, callback-driven stream."))

(define-condition cbuf-progress ()
  ((size
    :initarg :size
    :accessor cbuf-progress-size
    :initform 0)))

(defun call-processor (fun cbuf start end)
  (signal 'cbuf-progress :size (- end start))
  (funcall fun (data cbuf) start end))

(defun make-cbuf (connection)
  (make-instance 'cbuf :connection connection))

(defun make-stream-writer (stream)
  "Create a callback for writing data to STREAM."
  (lambda (data start end)
    (write-sequence data stream :start start :end end)))

(defgeneric size (cbuf)
  (:method ((cbuf cbuf))
    (- (end cbuf) (start cbuf))))

(defgeneric emptyp (cbuf)
  (:method ((cbuf cbuf))
    (zerop (size cbuf))))

(defgeneric refill (cbuf)
  (:method ((cbuf cbuf))
    (when (eofp cbuf)
      (error 'end-of-data))
    (setf (start cbuf) 0)
    (setf (end cbuf)
          (read-octets (data cbuf)
                       (connection cbuf)))
    (cond ((emptyp cbuf)
           (setf (eofp cbuf) t)
           (error 'end-of-data))
          (t (size cbuf)))))

(defun process-all (fun cbuf)
  (unless (emptyp cbuf)
    (call-processor fun cbuf (start cbuf) (end cbuf))))

(defun multi-cmatch (matchers cbuf)
  (let (start end)
    (dolist (matcher matchers (values start end))
      (multiple-value-bind (s e)
          (match matcher (data cbuf)
                 :start (start cbuf)
                 :end (end cbuf))
        (when (and s (or (null start) (< s start)))
          (setf start s
                end e))))))

(defun cmatch (matcher cbuf)
  (if (consp matcher)
      (multi-cmatch matcher cbuf)
      (match matcher (data cbuf) :start (start cbuf) :end (end cbuf))))

(defun call-until-end (fun cbuf)
  (handler-case
      (loop
       (process-all fun cbuf)
       (refill cbuf))
    (end-of-data ()
      (return-from call-until-end))))

(defun show-cbuf (context cbuf)
  (format t "cbuf: ~A ~D - ~D~%" context (start cbuf) (end cbuf)))

(defun call-for-n-octets (n fun cbuf)
  (let ((remaining n))
    (loop
     (when (<= remaining (size cbuf))
       (let ((end (+ (start cbuf) remaining)))
         (call-processor fun cbuf (start cbuf) end)
         (setf (start cbuf) end)
         (return)))
     (process-all fun cbuf)
     (decf remaining (size cbuf))
     (refill cbuf))))

(defun call-until-matching (matcher fun cbuf)
  (loop
   (multiple-value-bind (start end)
       (cmatch matcher cbuf)
     (when start
       (call-processor fun cbuf (start cbuf) end)
       (setf (start cbuf) end)
       (return)))
   (process-all fun cbuf)
   (refill cbuf)))

(defun ignore-data (data start end)
  (declare (ignore data start end)))

(defun skip-until-matching (matcher cbuf)
  (call-until-matching matcher 'ignore-data cbuf))


;;; Creating HTTP requests as octet buffers

(defclass octet-sink ()
  ((storage
    :initarg :storage
    :accessor storage))
  (:default-initargs
   :storage (make-array 1024 :element-type 'octet
                        :fill-pointer 0
                        :adjustable t))
  (:documentation "A simple stream-like target for collecting
  octets."))

(defun add-octet (octet sink)
  (vector-push-extend octet (storage sink)))

(defun add-octets (octets sink &key (start 0) end)
  (setf end (or end (length octets)))
  (loop for i from start below end
        do (add-octet (aref octets i) sink)))

(defun add-string (string sink)
  (loop for char across string
        for code = (char-code char)
        do (add-octet code sink)))

(defun add-strings (sink &rest strings)
  (mapc (lambda (string) (add-string string sink)) strings))

(defun add-newline (sink)
  (add-octet 13 sink)
  (add-octet 10 sink))

(defun sink-buffer (sink)
  (subseq (storage sink) 0))

(defvar *proxy-url* nil)

(defun full-proxy-path (host port path)
  (format nil "~:[http~;https~]://~A~:[:~D~;~*~]~A"
                       (= port 443)
                       host
                       (or (= port 80)
                           (= port 443))
                       port
                       path))

(defun make-request-buffer (host port path &key (method "GET"))
  (setf method (string method))
  (when *proxy-url*
    (setf path (full-proxy-path host port path)))
  (let ((sink (make-instance 'octet-sink)))
    (flet ((add-line (&rest strings)
             (apply #'add-strings sink strings)
             (add-newline sink)))
      (add-line method " " path " HTTP/1.1")
      (add-line "Host: " host (if (= port 80) ""
                                  (format nil ":~D" port)))
      (add-line "Connection: close")
      ;; FIXME: get this version string from somewhere else.
      (add-line "User-Agent: quicklisp-bootstrap/"
                qlqs-info:*version*)
      (add-newline sink)
      (sink-buffer sink))))

(defun sink-until-matching (matcher cbuf)
  (let ((sink (make-instance 'octet-sink)))
    (call-until-matching
     matcher
     (lambda (buffer start end)
       (add-octets buffer sink :start start :end end))
     cbuf)
    (sink-buffer sink)))


;;; HTTP headers

(defclass header ()
  ((data
    :initarg :data
    :accessor data)
   (status
    :initarg :status
    :accessor status)
   (name-starts
    :initarg :name-starts
    :accessor name-starts)
   (name-ends
    :initarg :name-ends
    :accessor name-ends)
   (value-starts
    :initarg :value-starts
    :accessor value-starts)
   (value-ends
    :initarg :value-ends
    :accessor value-ends)))

(defmethod print-object ((header header) stream)
  (print-unreadable-object (header stream :type t)
    (prin1 (status header) stream)))

(defun matches-at (pattern target pos)
  (= (mismatch pattern target :start2 pos) (length pattern)))

(defun header-value-indexes (field-name header)
  (loop with data = (data header)
        with pattern = (ascii-vector (string-downcase field-name))
        for start across (name-starts header)
        for i from 0
        when (matches-at pattern data start)
        return (values (aref (value-starts header) i)
                       (aref (value-ends header) i))))

(defun ascii-header-value (field-name header)
  (multiple-value-bind (start end)
      (header-value-indexes field-name header)
    (when start
      (ascii-subseq (data header) start end))))

(defun all-field-names (header)
  (map 'list
       (lambda (start end)
         (ascii-subseq (data header) start end))
       (name-starts header)
       (name-ends header)))

(defun headers-alist (header)
  (mapcar (lambda (name)
            (cons name (ascii-header-value name header)))
          (all-field-names header)))

(defmethod describe-object :after ((header header) stream)
  (format stream "~&Decoded headers:~%  ~S~%" (headers-alist header)))

(defun content-length (header)
  (let ((field-value (ascii-header-value "content-length" header)))
    (when field-value
      (let ((value (ignore-errors (parse-integer field-value))))
        (or value
            (error "Content-Length header field value is not a number -- ~A"
                   field-value))))))

(defun chunkedp (header)
  (string= (ascii-header-value "transfer-encoding" header) "chunked"))

(defun location (header)
  (ascii-header-value "location" header))

(defun status-code (vector)
  (let* ((space (position (acode #\Space) vector))
         (c1 (- (aref vector (incf space)) 48))
         (c2 (- (aref vector (incf space)) 48))
         (c3 (- (aref vector (incf space)) 48)))
    (+ (* c1 100)
       (* c2  10)
       (* c3   1))))

(defun force-downcase-field-names (header)
  (loop with data = (data header)
        for start across (name-starts header)
        for end across (name-ends header)
        do (loop for i from start below end
                 for code = (aref data i)
                 do (setf (aref data i) (ascii-downcase code)))))

(defun skip-white-forward (pos vector)
  (position-if-not 'whitep vector :start pos))

(defun skip-white-backward (pos vector)
  (let ((nonwhite (position-if-not 'whitep vector :end pos :from-end t)))
    (if nonwhite
        (1+ nonwhite)
        pos)))

(defun contract-field-value-indexes (header)
  "Header field values exclude leading and trailing whitespace; adjust
the indexes in the header accordingly."
  (loop with starts = (value-starts header)
        with ends = (value-ends header)
        with data = (data header)
        for i from 0
        for start across starts
        for end across ends
        do
        (setf (aref starts i) (skip-white-forward start data))
        (setf (aref ends i) (skip-white-backward end data))))

(defun next-line-pos (vector)
  (let ((pos 0))
    (labels ((finish (&optional (i pos))
               (return-from next-line-pos i))
             (after-cr (code)
               (acase code
                 (:lf (finish pos))
                 (t (finish (1- pos)))))
             (pending (code)
               (acase code
                 (:cr #'after-cr)
                 (:lf (finish pos))
                 (t #'pending))))
      (let ((state #'pending))
        (loop
         (setf state (funcall state (aref vector pos)))
         (incf pos))))))

(defun make-hvector ()
  (make-array 16 :fill-pointer 0 :adjustable t))

(defun process-header (vector)
  "Create a HEADER instance from the octet data in VECTOR."
  (let* ((name-starts (make-hvector))
         (name-ends (make-hvector))
         (value-starts (make-hvector))
         (value-ends (make-hvector))
         (header (make-instance 'header
                                :data vector
                                :status 999
                                :name-starts name-starts
                                :name-ends name-ends
                                :value-starts value-starts
                                :value-ends value-ends))
         (mark nil)
         (pos (next-line-pos vector)))
    (unless pos
      (error "Unable to process HTTP header"))
    (setf (status header) (status-code vector))
    (labels ((save (value vector)
               (vector-push-extend value vector))
             (mark ()
               (setf mark pos))
             (clear-mark ()
               (setf mark nil))
             (finish ()
               (if mark
                   (save mark value-ends)
                   (save pos value-ends))
              (force-downcase-field-names header)
              (contract-field-value-indexes header)
              (return-from process-header header))
             (in-new-line (code)
               (acase code
                 ((#\Tab #\Space) (setf mark nil) #'in-value)
                 (t
                  (when mark
                    (save mark value-ends))
                  (clear-mark)
                  (save pos name-starts)
                  (in-name code))))
             (after-cr (code)
               (acase code
                 (:lf #'in-new-line)
                 (t (in-new-line code))))
             (pending-value (code)
               (acase code
                 ((#\Tab #\Space) #'pending-value)
                 (:cr #'after-cr)
                 (:lf #'in-new-line)
                 (t (save pos value-starts) #'in-value)))
             (in-name (code)
               (acase code
                 (#\:
                  (save pos name-ends)
                  (save (1+ pos) value-starts)
                  #'in-value)
                 ((:cr :lf)
                  (finish))
                 ((#\Tab #\Space)
                  (error "Unexpected whitespace in header field name"))
                 (t
                  (unless (<= 0 code 127)
                    (error "Unexpected non-ASCII header field name"))
                  #'in-name)))
             (in-value (code)
               (acase code
                 (:lf (mark) #'in-new-line)
                 (:cr (mark) #'after-cr)
                 (t #'in-value))))
      (let ((state #'in-new-line))
        (loop
         (incf pos)
         (when (<= (length vector) pos)
           (error "No header found in response"))
         (setf state (funcall state (aref vector pos))))))))


;;; HTTP URL parsing

(defclass url ()
  ((hostname
    :initarg :hostname
    :accessor hostname
    :initform nil)
   (port
    :initarg :port
    :accessor port
    :initform 80)
   (path
    :initarg :path
    :accessor path
    :initform "/")))

(defun parse-urlstring (urlstring)
  (setf urlstring (string-trim " " urlstring))
  (let* ((pos (mismatch urlstring "http://" :test 'char-equal))
         (mark pos)
         (url (make-instance 'url)))
    (labels ((save ()
               (subseq urlstring mark pos))
             (mark ()
               (setf mark pos))
             (finish ()
               (return-from parse-urlstring url))
             (hostname-char-p (char)
               (position char "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_."
                         :test 'char-equal))
             (at-start (char)
               (case char
                 (#\/
                  (setf (port url) nil)
                  (mark)
                  #'in-path)
                 (t
                  #'in-host)))
             (in-host (char)
               (case char
                 ((#\/ :end)
                  (setf (hostname url) (save))
                  (mark)
                  #'in-path)
                 (#\:
                  (setf (hostname url) (save))
                  (mark)
                  #'in-port)
                 (t
                  (unless (hostname-char-p char)
                    (error "~S is not a valid URL" urlstring))
                  #'in-host)))
             (in-port (char)
               (case char
                 ((#\/ :end)
                  (setf (port url)
                        (parse-integer urlstring
                                       :start (1+ mark)
                                       :end pos))
                  (mark)
                  #'in-path)
                 (t
                  (unless (digit-char-p char)
                    (error "Bad port in URL ~S" urlstring))
                  #'in-port)))
             (in-path (char)
               (case char
                 ((#\# :end)
                  (setf (path url) (save))
                  (finish)))
               #'in-path))
      (let ((state #'at-start))
        (loop
         (when (<= (length urlstring) pos)
           (funcall state :end)
           (finish))
         (setf state (funcall state (aref urlstring pos)))
         (incf pos))))))

(defun url (thing)
  (if (stringp thing)
      (parse-urlstring thing)
      thing))

(defgeneric request-buffer (method url)
  (:method (method url)
    (setf url (url url))
    (make-request-buffer (hostname url) (port url) (path url)
                         :method method)))

(defun urlstring (url)
  (format nil "~@[http://~A~]~@[:~D~]~A"
          (hostname url)
          (and (/= 80 (port url)) (port url))
          (path url)))

(defmethod print-object ((url url) stream)
  (print-unreadable-object (url stream :type t)
    (prin1 (urlstring url) stream)))

(defun merge-urls (url1 url2)
  (setf url1 (url url1))
  (setf url2 (url url2))
  (make-instance 'url
                 :hostname (or (hostname url1)
                               (hostname url2))
                 :port (or (port url1)
                           (port url2))
                 :path (or (path url1)
                           (path url2))))


;;; Requesting an URL and saving it to a file

(defparameter *maximum-redirects* 10)
(defvar *default-url-defaults* (url "http://src.quicklisp.org/"))

(defun read-http-header (cbuf)
  (let ((header-data (sink-until-matching (list (acode-matcher :lf :lf)
                                                (acode-matcher :cr :cr)
                                                (acode-matcher :cr :lf :cr :lf))
                                 cbuf)))
    (process-header header-data)))

(defun read-chunk-header (cbuf)
  (let* ((header-data (sink-until-matching (acode-matcher :cr :lf) cbuf))
         (end (or (position (acode :cr) header-data)
                  (position (acode #\;) header-data))))
    (values (parse-integer (ascii-subseq header-data 0 end) :radix 16))))

(defun save-chunk-response (stream cbuf)
  "For a chunked response, read all chunks and write them to STREAM."
  (let ((fun (make-stream-writer stream))
        (matcher (acode-matcher :cr :lf)))
    (loop
     (let ((chunk-size (read-chunk-header cbuf)))
       (when (zerop chunk-size)
         (return))
       (call-for-n-octets chunk-size fun cbuf)
       (skip-until-matching matcher cbuf)))))

(defun save-response (file header cbuf)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :element-type 'octet)
    (let ((content-length (content-length header)))
      (cond ((chunkedp header)
             (save-chunk-response stream cbuf))
            (content-length
             (call-for-n-octets content-length
                                (make-stream-writer stream)
                                cbuf))
            (t
             (call-until-end (make-stream-writer stream) cbuf))))))

(defun call-with-progress-bar (size fun)
  (let ((progress-bar (make-progress-bar size)))
    (start-display progress-bar)
    (flet ((update (condition)
             (update-progress progress-bar
                              (cbuf-progress-size condition))))
      (handler-bind ((cbuf-progress #'update))
        (funcall fun)))
    (finish-display progress-bar)))

(defun fetch (url file &key (follow-redirects t) quietly
              (maximum-redirects *maximum-redirects*))
  "Request URL and write the body of the response to FILE."
  (setf url (merge-urls url *default-url-defaults*))
  (setf file (merge-pathnames file))
  (let ((redirect-count 0)
        (original-url url)
        (connect-url (or (url *proxy-url*) url))
        (stream (if quietly
                    (make-broadcast-stream)
                    *trace-output*)))
    (loop
     (when (<= maximum-redirects redirect-count)
       (error "Too many redirects for ~A" original-url))
     (with-connection (connection (hostname connect-url) (port connect-url))
       (let ((cbuf (make-instance 'cbuf :connection connection))
             (request (request-buffer "GET" url)))
         (write-octets request connection)
         (let ((header (read-http-header cbuf)))
           (loop while (= (status header) 100)
                 do (setf header (read-http-header cbuf)))
           (cond ((= (status header) 200)
                  (let ((size (content-length header)))
                    (format stream "~&; Fetching ~A~%" url)
                    (if (and (numberp size)
                             (plusp size))
                        (format stream "; ~$KB~%" (/ size 1024))
                        (format stream "; Unknown size~%"))
                    (if quietly
                        (save-response file header cbuf)
                        (call-with-progress-bar (content-length header)
                                                (lambda ()
                                                  (save-response file header cbuf))))))
                 ((not (<= 300 (status header) 399))
                  (error "Unexpected status for ~A: ~A"
                         url (status header))))
           (if (and follow-redirects (<= 300 (status header) 399))
               (let ((new-urlstring (ascii-header-value "location" header)))
                 (when (not new-urlstring)
                   (error "Redirect code ~D received, but no Location: header"
                          (status header)))
                 (incf redirect-count)
                 (setf url (merge-urls new-urlstring
                                       url))
                 (format stream "~&; Redirecting to ~A~%" url))
               (return (values header (and file (probe-file file)))))))))))


;;; A primitive tar unpacker

(in-package #:qlqs-minitar)

(defun make-block-buffer ()
  (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0))

(defun skip-n-blocks (n stream)
  (let ((block (make-block-buffer)))
    (dotimes (i n)
      (read-sequence block stream))))

(defun ascii-subseq (vector start end)
  (let ((string (make-string (- end start))))
    (loop for i from 0
          for j from start below end
          do (setf (char string i) (code-char (aref vector j))))
    string))

(defun block-asciiz-string (block start length)
  (let* ((end (+ start length))
         (eos (or (position 0 block :start start :end end)
                            end)))
    (ascii-subseq block start eos)))

(defun prefix (header)
  (when (plusp (aref header 345))
    (block-asciiz-string header 345 155)))

(defun name (header)
  (block-asciiz-string header 0 100))

(defun payload-size (header)
  (values (parse-integer (block-asciiz-string header 124 12) :radix 8)))

(defun nth-block (n file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (let ((block (make-block-buffer)))
      (skip-n-blocks (1- n) stream)
      (read-sequence block stream)
      block)))

(defun payload-type (code)
  (case code
    (0 :file)
    (48 :file)
    (53 :directory)
    (t :unsupported)))

(defun full-path (header)
  (let ((prefix (prefix header))
        (name (name header)))
    (if prefix
        (format nil "~A/~A" prefix name)
        name)))

(defun save-file (file size stream)
  (multiple-value-bind (full-blocks partial)
      (truncate size 512)
    (ensure-directories-exist file)
    (with-open-file (outstream file
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
      (let ((block (make-block-buffer)))
        (dotimes (i full-blocks)
          (read-sequence block stream)
          (write-sequence block outstream))
        (when (plusp partial)
          (read-sequence block stream)
          (write-sequence block outstream :end partial))))))

(defun unpack-tarball (tarfile &key (directory *default-pathname-defaults*))
  (let ((block (make-block-buffer)))
    (with-open-file (stream tarfile :element-type '(unsigned-byte 8))
      (loop
       (let ((size (read-sequence block stream)))
         (when (zerop size)
           (return))
         (unless (= size 512)
           (error "Bad size on tarfile"))
         (when (every #'zerop block)
           (return))
         (let* ((payload-code (aref block 156))
                (payload-type (payload-type payload-code))
                (tar-path (full-path block))
                (full-path (merge-pathnames tar-path directory))
                (payload-size (payload-size block)))
         (case payload-type
           (:file
            (save-file full-path payload-size stream))
           (:directory
            (ensure-directories-exist full-path))
           (t
            (warn "Unknown tar block payload code -- ~D" payload-code)
            (skip-n-blocks (ceiling (payload-size block) 512) stream)))))))))

(defun contents (tarfile)
  (let ((block (make-block-buffer))
        (result '()))
    (with-open-file (stream tarfile :element-type '(unsigned-byte 8))
      (loop
        (let ((size (read-sequence block stream)))
          (when (zerop size)
            (return (nreverse result)))
          (unless (= size 512)
            (error "Bad size on tarfile"))
          (when (every #'zerop block)
            (return (nreverse result)))
          (let* ((payload-type (payload-type (aref block 156)))
                 (tar-path (full-path block))
                 (payload-size (payload-size block)))
            (skip-n-blocks (ceiling payload-size 512) stream)
            (case payload-type
              (:file
               (push tar-path result))
              (:directory
               (push tar-path result)))))))))


;;;
;;; The actual bootstrapping work
;;;

(in-package #:quicklisp-quickstart)

(defvar *home*
  (merge-pathnames (make-pathname :directory '(:relative "quicklisp"))
                   (user-homedir-pathname)))

(defun qmerge (pathname)
  (merge-pathnames pathname *home*))

(defun renaming-fetch (url file)
  (let ((tmpfile (qmerge "tmp/fetch.dat")))
    (fetch url tmpfile)
    (rename-file tmpfile file)))

(defvar *quickstart-parameters* nil
  "This plist is populated with parameters that may carry over to the
  initial configuration of the client, e.g. :proxy-url
  or :initial-dist-url")

(defvar *quicklisp-hostname* "beta.quicklisp.org")

(defvar *client-info-url*
  (format nil "http://~A/client/quicklisp.sexp"
          *quicklisp-hostname*))

(defclass client-info ()
  ((setup-url
    :reader setup-url
    :initarg :setup-url)
   (asdf-url
    :reader asdf-url
    :initarg :asdf-url)
   (client-tar-url
    :reader client-tar-url
    :initarg :client-tar-url)
   (version
    :reader version
    :initarg :version)
   (plist
    :reader plist
    :initarg :plist)
   (source-file
    :reader source-file
    :initarg :source-file)))

(defmethod print-object ((client-info client-info) stream)
  (print-unreadable-object (client-info stream :type t)
    (prin1 (version client-info) stream)))

(defun safely-read (stream)
  (let ((*read-eval* nil))
    (read stream)))

(defun fetch-client-info-plist (url)
  "Fetch and return the client info data at URL."
  (let ((local-client-info-file (qmerge "tmp/client-info.sexp")))
    (ensure-directories-exist local-client-info-file)
    (renaming-fetch url local-client-info-file)
    (with-open-file (stream local-client-info-file)
      (list* :source-file local-client-info-file
             (safely-read stream)))))

(defun fetch-client-info (url)
  (let ((plist (fetch-client-info-plist url)))
    (destructuring-bind (&key setup asdf client-tar version
                              source-file
                              &allow-other-keys)
        plist
      (unless (and setup asdf client-tar version)
        (error "Invalid data from client info URL -- ~A" url))
      (make-instance 'client-info
                     :setup-url (getf setup :url)
                     :asdf-url (getf asdf :url)
                     :client-tar-url (getf client-tar :url)
                     :version version
                     :plist plist
                     :source-file source-file))))

(defun client-info-url-from-version (version)
  (format nil "http://~A/client/~A/client-info.sexp"
          *quicklisp-hostname*
          version))

(defun distinfo-url-from-version (version)
  (format nil "http://~A/dist/~A/distinfo.txt"
          *quicklisp-hostname*
          version))

(defvar *help-message*
  (format nil "~&~%  ==== quicklisp quickstart install help ====~%~%    ~
               quicklisp-quickstart:install can take the following ~
               optional arguments:~%~%      ~
                 :path \"/path/to/installation/\"~%~%      ~
                 :proxy \"http://your.proxy:port/\"~%~%      ~
                 :client-url <url>~%~%      ~
                 :client-version <version>~%~%      ~
                 :dist-url <url>~%~%      ~
                 :dist-version <version>~%~%"))

(defvar *after-load-message*
  (format nil "~&~%  ==== quicklisp quickstart ~A loaded ====~%~%    ~
               To continue with installation, evaluate: (quicklisp-quickstart:install)~%~%    ~
               For installation options, evaluate: (quicklisp-quickstart:help)~%~%"
          qlqs-info:*version*))

(defvar *after-initial-setup-message*
  (with-output-to-string (*standard-output*)
    (format t "~&~%  ==== quicklisp installed ====~%~%")
    (format t "    To load a system, use: (ql:quickload \"system-name\")~%~%")
    (format t "    To find systems, use: (ql:system-apropos \"term\")~%~%")
    (format t "    To load Quicklisp every time you start Lisp, use: (ql:add-to-init-file)~%~%")
    (format t "    For more information, see http://www.quicklisp.org/beta/~%~%")))

(defun initial-install (&key (client-url *client-info-url*) dist-url)
  (setf *quickstart-parameters*
        (list :proxy-url *proxy-url*
              :initial-dist-url dist-url))
  (ensure-directories-exist (qmerge "tmp/"))
  (let ((client-info (fetch-client-info client-url))
        (tmptar (qmerge "tmp/quicklisp.tar"))
        (setup (qmerge "setup.lisp"))
        (asdf (qmerge "asdf.lisp")))
    (renaming-fetch (client-tar-url client-info) tmptar)
    (unpack-tarball tmptar :directory (qmerge "./"))
    (renaming-fetch (setup-url client-info) setup)
    (renaming-fetch (asdf-url client-info) asdf)
    (rename-file (source-file client-info) (qmerge "client-info.sexp"))
    (load setup :verbose nil :print nil)
    (write-string *after-initial-setup-message*)
    (finish-output)))

(defun help ()
  (write-string *help-message*)
  t)

(defun non-empty-file-namestring (pathname)
  (let ((string (file-namestring pathname)))
    (unless (or (null string)
                (equal string ""))
      string)))

(defun install (&key ((:path *home*) *home*)
                  ((:proxy *proxy-url*) *proxy-url*)
                  client-url
                  client-version
                  dist-url
                  dist-version)
  (setf *home* (merge-pathnames *home* (truename *default-pathname-defaults*)))
  (let ((name (non-empty-file-namestring *home*)))
    (when name
      (warn "Making ~A part of the install pathname directory"
            name)
      ;; This corrects a pathname like "/foo/bar" to "/foo/bar/" and
      ;; "foo" to "foo/"
      (setf *home*
            (make-pathname :defaults *home*
                           :directory (append (pathname-directory *home*)
                                              (list name))))))
  (let ((setup-file (qmerge "setup.lisp")))
    (when (probe-file setup-file)
      (multiple-value-bind (result proceed)
          (with-simple-restart (load-setup "Load ~S" setup-file)
            (error "Quicklisp has already been installed. Load ~S instead."
                   setup-file))
        (declare (ignore result))
        (when proceed
          (return-from install (load setup-file))))))
  (if (find-package '#:ql)
      (progn
        (write-line "!!! Quicklisp has already been set up. !!!")
        (write-string *after-initial-setup-message*)
        t)
      (call-with-quiet-compilation
       (lambda ()
         (let ((client-url (or client-url
                               (and client-version
                                    (client-info-url-from-version client-version))
                               *client-info-url*))
               ;; It's ok for dist-url to be nil; there's a default in
               ;; the client
               (dist-url (or dist-url
                             (and dist-version
                                  (distinfo-url-from-version dist-version)))))
           (initial-install :client-url client-url
                            :dist-url dist-url))))))

(write-string *after-load-message*)

;;; End of quicklisp.lisp

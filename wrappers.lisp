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
;; portability wrappers. Any code that must run different code for
;; different lisps should be wrapped up in a function and put here.
;;
;; Code:

(in-package #:stumpwm)

(export '(getenv))

;;; XXX: DISPLAY env var isn't set for cmucl
(defun run-prog (prog &rest opts &key args (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  #+gcl (declare (ignore wait))
  (setq opts (remove-plist opts :args :wait))
  #+allegro (apply #'excl:run-shell-command (apply #'vector prog prog args)
                   :wait wait opts)
  #+(and clisp      lisp=cl)
  (progn
    ;; Arg. We can't pass in an environment so just set the DISPLAY
    ;; variable so it's inherited by the child process.
    (setf (getenv "DISPLAY") (format nil "~a:~d.~d"
                                     (screen-host (current-screen))
                                     (xlib:display-display *display*)
                                     (screen-id (current-screen))))
    (apply #'ext:run-program prog :arguments args :wait wait opts))
  #+(and clisp (not lisp=cl))
  (if wait
      (apply #'lisp:run-program prog :arguments args opts)
      (lisp:shell (format nil "~a~{ '~a'~} &" prog args)))
  #+cmu (apply #'ext:run-program prog args :output t :error t :wait wait opts)
  #+gcl (apply #'si:run-process prog args)
  #+liquid (apply #'lcl:run-program prog args)
  #+lispworks (apply #'sys::call-system
                     (format nil "~a~{ '~a'~}~@[ &~]" prog args (not wait))
                     opts)
  #+lucid (apply #'lcl:run-program prog :wait wait :arguments args opts)
  #+sbcl (apply #'sb-ext:run-program prog args :output t :error t :wait wait
                ;; inject the DISPLAY variable in so programs show up
                ;; on the right screen.
                :environment (cons (screen-display-string (current-screen))
                                   (remove-if (lambda (str)
                                                (string= "DISPLAY=" str :end2 (min 8 (length str))))
                                              (sb-ext:posix-environ)))
                opts)
  #+openmcl (apply #'ccl:run-program prog args :wait wait :output t :error t)
  #-(or allegro clisp cmu gcl liquid lispworks lucid sbcl openmcl)
  (error 'not-implemented :proc (list 'run-prog prog opts)))

;;; XXX: DISPLAY isn't set for cmucl
(defun run-prog-collect-output (prog &rest args)
  "run a command and read its output."
  #+allegro (with-output-to-string (s)
              (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                      :output s :wait t))
  ;; FIXME: this is a dumb hack but I don't care right now.
  #+clisp (with-output-to-string (s)
            ;; Arg. We can't pass in an environment so just set the DISPLAY
            ;; variable so it's inherited by the child process.
            (setf (getenv "DISPLAY") (format nil "~a:~d.~d"
                                             (screen-host (current-screen))
                                             (xlib:display-display *display*)
                                             (screen-id (current-screen))))
            (let ((out (ext:run-program prog :arguments args :wait t :output :stream)))
              (loop for i = (read-char out nil out)
                    until (eq i out)
                    do (write-char i s))))
  #+cmu (with-output-to-string (s) (ext:run-program prog args :output s :error s :wait t))
  #+sbcl (with-output-to-string (s)
           (sb-ext:run-program prog args :output s :error s :wait t
                               ;; inject the DISPLAY variable in so programs show up
                               ;; on the right screen.
                               :environment (cons (screen-display-string (current-screen))
                                                  (remove-if (lambda (str)
                                                               (string= "DISPLAY=" str :end2 (min 8 (length str))))
                                                             (sb-ext:posix-environ)))))
  #+openmcl (with-output-to-string (s)
              (ccl:run-program prog args :wait t :output s :error t))
  #-(or allegro clisp cmu sbcl openmcl)
  (error 'not-implemented :proc (list 'pipe-input prog args)))

(defun getenv (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (ext:getenv (string var))
  #+(or cmu scl)
  (cdr (assoc (string var) ext:*environment-list* :test #'equalp
              :key #'string))
  #+gcl (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+mcl (ccl::getenv var)
  #+sbcl (sb-posix:getenv (string var))
  #+openmcl (ccl:getenv (string var))
  #-(or allegro clisp cmu gcl lispworks lucid mcl sbcl scl openmcl)
  (error 'not-implemented :proc (list 'getenv var)))

(defun (setf getenv) (val var)
  "Set the value of the environment variable, @var{var} to @var{val}."
  #+allegro (setf (sys::getenv (string var)) (string val))
  #+clisp (setf (ext:getenv (string var)) (string val))
  #+(or cmu scl)
  (let ((cell (assoc (string var) ext:*environment-list* :test #'equalp
                     :key #'string)))
    (if cell
        (setf (cdr cell) (string val))
        (push (cons (intern (string var) "KEYWORD") (string val))
              ext:*environment-list*)))
  #+gcl (si:setenv (string var) (string val))
  #+lispworks (setf (lw:environment-variable (string var)) (string val))
  #+lucid (setf (lcl:environment-variable (string var)) (string val))
  #+sbcl (sb-posix:putenv (format nil "~A=~A" (string var) (string val)))
  #+openmcl (ccl:setenv (string var) (string val))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl scl openmcl)
  (error 'not-implemented :proc (list '(setf getenv) var)))

(defun pathname-is-executable-p (pathname)
  "Return T if the pathname describes an executable file."
  #+sbcl
  (let ((filename (coerce (sb-int:unix-namestring pathname) 'base-string)))
    (and (eq (sb-unix:unix-file-kind filename) :file)
         (sb-unix:unix-access filename sb-unix:x_ok)))
  ;; FIXME: this is not exactly perfect
  #+clisp
  (logand (posix:convert-mode (posix:file-stat-mode (posix:file-stat pathname)))
          (posix:convert-mode '(:xusr :xgrp :xoth)))
  #-(or sbcl clisp) t)

(defun probe-path (path)
  "Return the truename of a supplied path, or nil if it does not exist."
  (handler-case
      (truename
       (let ((pathname (pathname path)))
         ;; If there is neither a type nor a name, we have a directory
         ;; pathname already. Otherwise make a valid one.
         (if (and (not (pathname-name pathname))
                  (not (pathname-type pathname)))
             pathname
             (make-pathname
              :directory (append (or (pathname-directory pathname)
                                     (list :relative))
                                 (list (file-namestring pathname)))
              :name nil :type nil :defaults pathname))))
    (file-error () nil)))

(defun portable-file-write-date (pathname)
  ;; clisp errors out if you run file-write-date on a directory.
  #+clisp (posix:file-stat-mtime (posix:file-stat pathname))
  #-clisp (file-write-date pathname))

(defun print-backtrace (&optional (frames 100))
  "print a backtrace of FRAMES number of frames to standard-output"
  #+sbcl (sb-debug:backtrace frames *standard-output*)
  #+clisp (ext:show-stack 1 frames (sys::the-frame))
  #+openmcl (ccl:print-call-history :count frames)

  #-(or sbcl clisp openmcl) (write-line "Sorry, no backtrace for you."))

(defun bytes-to-string (data)
  "Convert a list of bytes into a string."
  #+sbcl
  (sb-ext:octets-to-string
   (make-array (length data) :element-type '(unsigned-byte 8) :initial-contents data))
  #+clisp
  (ext:convert-string-from-bytes 
   (make-array (length data) :element-type '(unsigned-byte 8) :initial-contents data)
   custom:*terminal-encoding*)
  #-(or sbcl clisp)
  (map 'list #'code-char string))

(defun string-to-bytes (string)
  "Convert a string to a vector of octets."
  #+sbcl
  (sb-ext:string-to-octets string)
  #+clisp
  (ext:convert-string-to-bytes string custom:*terminal-encoding*)
  #-(or sbcl clisp)
  (map 'list #'char-code string))

(defun utf8-to-string (octets)
  "Convert the list of octets to a string."
  #+sbcl (sb-ext:octets-to-string
          (coerce octets '(vector (unsigned-byte 8)))
          :external-format :utf-8)
  #+clisp (ext:convert-string-from-bytes (coerce octets '(vector (unsigned-byte 8)))
                                         charset:utf-8)
  #-(or sbcl clisp)
  (map 'string #'code-char octets))

(defun string-to-utf8 (string)
  "Convert the string to a vector of octets."
  #+sbcl (sb-ext:string-to-octets
          string
          :external-format :utf-8)
  #+clisp (ext:convert-string-to-bytes string charset:utf-8)
  #-(or sbcl clisp)
  (map 'list #'char-code string))

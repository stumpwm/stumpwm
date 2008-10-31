;;; Amixer module for StumpWM.
;;;
;;; Copyright 2008 Julian Stecklina
;;;
;;; Maintainer: Julian Stecklina
;;;
;;; This module is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This module is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA
;;;

;;; Thanks to Fredrik Tolf for formatting it as a stumpwm module.

(in-package :stumpwm-user)

;;; XXX This is only a workaround for SBCLs with a unreliable
;;; run-program implementation (every version at least until
;;; 1.0.21). If someone makes run-program race-free, this should be
;;; removed! - Julian Stecklina (Oct 23th, 2008)
#+sbcl
(progn
  (defun exec-and-collect-output (name args env)
    "Runs the command NAME with ARGS as parameters and return everything
the command has printed on stdout as string."
    (flet ((to-simple-strings (string-list)
	     (mapcar (lambda (x)
		       (coerce x 'simple-string))
		     string-list)))
      (let ((simplified-args (to-simple-strings (cons name args)))
	    (simplified-env (to-simple-strings env))
	    (progname (sb-impl::native-namestring name))
	    (devnull (sb-posix:open "/dev/null" sb-posix:o-rdwr)))
	(multiple-value-bind (pipe-read pipe-write)
	    (sb-posix:pipe)
	  (unwind-protect
	       (let ((child 
		      ;; Any nicer way to do this?
		      (sb-sys:without-gcing 
			(sb-impl::with-c-strvec (c-argv simplified-args)
			  (sb-impl::with-c-strvec (c-env simplified-env)
			    (sb-impl::spawn  progname c-argv devnull 
					     pipe-write ; stdout
					     devnull 1 c-env 
					     nil ; PTY
					     1 ; wait? (seems to do nothing)
					     ))))))
		 (when (= child -1)
		   (error "Starting ~A failed." name))
		 ;; We need to close this end of the pipe to get EOF when the child is done.
		 (sb-posix:close pipe-write)
		 (setq pipe-write nil)
		 (with-output-to-string (out)
		   ;; XXX Could probably be optimized. But shouldn't
		   ;; make a difference for our use case.
		   (loop 
		      with in-stream = (sb-sys:make-fd-stream pipe-read :buffering :none)
		      for char = (read-char in-stream nil nil)
		      while char
		      do (write-char char out))
		   ;; The child is now finished. Call waitpid to avoid
		   ;; creating zombies.
		   (handler-case
		       (sb-posix:waitpid child 0)
		     (sb-posix:syscall-error ()
		       ;; If we get a syscall-error, RUN-PROGRAM's
		       ;; SIGCHLD handler probably retired our child
		       ;; already. So we are fine here to ignore this.
		       nil))))
	    ;; Cleanup
	    (sb-posix:close pipe-read)
	    (when pipe-write
	      (sb-posix:close pipe-write))
	    (sb-posix:close devnull))))))

  (defun stumpwm::run-prog-collect-output (prog &rest args)
    "run a command and read its output."
    #+sbcl (exec-and-collect-output prog args (cons (stumpwm::screen-display-string (current-screen))
						    (remove-if (lambda (str)
								 (string= "DISPLAY=" str :end2 (min 8 (length str))))
							       (sb-ext:posix-environ))))))

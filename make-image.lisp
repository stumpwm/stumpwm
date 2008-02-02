;;; SBCL
#+sbcl
(progn
  (require 'asdf)
  (require 'stumpwm))
#+sbcl
(progn
  (load "stumpwm.asd")
  (sb-ext:save-lisp-and-die "stumpwm" :toplevel (lambda ()
                                                  ;; asdf requires sbcl_home to be set, so set it to the value when the image was built
                                                  (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
                                                  (stumpwm:stumpwm)
                                                  0)
                            :executable t))

;;; CLISP

;; a custom loader for clisp. It beats trying to look for asdf, for now.
#+clisp
(progn
  (load "asdf.lisp")
  (load "stumpwm.asd"))
#+clisp
(progn
  (push (merge-pathnames  #p".sbcl/systems/" (user-homedir-pathname)) asdf:*central-registry*)
  (asdf:oos 'asdf:load-op 'stumpwm))
#+clisp
(progn
  (ext:saveinitmem "stumpwm" :init-function (lambda ()
                                              (stumpwm:stumpwm)
                                              (ext:quit))
                   :executable t :keep-global-handlers t :norc t :documentation "The StumpWM Executable"))


#-(or sbcl clisp) (error "This lisp implementation is not supported.")

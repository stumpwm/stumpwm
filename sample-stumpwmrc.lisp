;; -*-lisp-*-
;;
;; Here is a sample .stumpwmrc file

(in-package :stumpwm)

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(define-stumpwm-command "colon1" (screen (initial :rest nil))
  (let ((cmd (read-one-line screen ": " initial)))
    (when cmd
      (interactive-command cmd screen))))

;; Read some doc
(set-key-binding #\d '() "exec gv")
;; Browse somewhere
(set-key-binding #\b '() "colon1 exec firefox http://www.")
;; Ssh somewhere
(set-key-binding #\s '(:control) "colon1 exec xterm -e ssh ")
;; Lock screen
(set-key-binding #\l '(:control) "exec xlock")

;; Web jump (works for Google and Imdb)
(defmacro make-web-jump (name prefix)
  `(define-stumpwm-command ,name (screen (search :rest ,(concatenate 'string name " search: ")))
     (declare (ignore screen))
     (substitute #\+ #\Space search)
     (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "google" "firefox http://www.google.fr/search?q=")
(make-web-jump "imdb" "firefox http://www.imdb.com/find?q=")

(set-key-binding #\g '() "google")
(set-key-binding #\i '() "imdb")

;; Message window font
(setf *font-name* "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")

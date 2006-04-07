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
(define-key *root-map* (kbd "d") "exec gv")
;; Browse somewhere
(define-key *root-map* (kbd "b") "colon1 exec firefox http://www.")
;; Ssh somewhere
(define-key *root-map* (kbd "C-s") "colon1 exec xterm -e ssh ")
;; Lock screen
(define-key *root-map* (kbd "C-l") "exec xlock")

;; Web jump (works for Google and Imdb)
(defmacro make-web-jump (name prefix)
  `(define-stumpwm-command ,name (screen (search :rest ,(concatenate 'string name " search: ")))
     (declare (ignore screen))
     (substitute #\+ #\Space search)
     (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "google" "firefox http://www.google.fr/search?q=")
(make-web-jump "imdb" "firefox http://www.imdb.com/find?q=")

(define-key *root-map* (kbd "g") "google")
(define-key *root-map* (kbd "i") "imdb")

;; Message window font
(setf *font-name* "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")

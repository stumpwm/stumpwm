;; -*-lisp-*-
;;
;; Here is a sample .stumpwmrc file

(in-package :stumpwm)

;; Read some doc
(set-key-binding #\d '() (lambda (s) (run-command-string "gv")))
;; Browse somewhere
(set-key-binding #\b '() (partial-command "" "firefox http://www."))
;; Ssh somewhere
(set-key-binding #\s '(:control) (partial-command "" "xterm -e ssh "))
;; Lock screen
(set-key-binding #\l '(:control) (lambda (s) (run-command-string "xlock")))

;; Web search (works for Google and Imdb)
(defun make-web-search (prompt prefix)
  #'(lambda (s)
      (let ((search (read-one-line s prompt)))
	(unless (null search)
	  (run-command-string 
	   (concatenate 'string prefix
			(substitute #\+ #\Space search)))))))
(set-key-binding #\g '() (make-web-search "Google search: " "firefox http://www.google.fr/search?q="))
(set-key-binding #\i '() (make-web-search "IMDB search: " "firefox http://www.imdb.com/find?q="))

;; Message window font
(setf *font-name* "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")

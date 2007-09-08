;; -*-lisp-*-
;;
;; Here is a sample .stumpwmrc file

(in-package :stumpwm)

;; change the prefix key to something else
(set-prefix-key (kbd "C-z"))

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(define-stumpwm-command "colon1" ((initial :rest nil))
  (let ((cmd (read-one-line (current-screen) ": " initial)))
    (when cmd
      (interactive-command cmd (current-screen)))))

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
  `(define-stumpwm-command ,name ((search :rest ,(concatenate 'string name " search: ")))
     (substitute #\+ #\Space search)
     (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "google" "firefox http://www.google.fr/search?q=")
(make-web-jump "imdb" "firefox http://www.imdb.com/find?q=")

;; C-t M-s is a terrble binding, but you get the idea.
(define-key *root-map* (kbd "M-s") "google")
(define-key *root-map* (kbd "i") "imdb")

;; Message window font
(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")

;;; Define window placement policy...

;; Clear rules
(setf *window-placement-rules* '())

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: rules won't do anything if the target groups/frames don't exist! Save
;; your layout with "asfdump" and "asfrestore" will re-create everything for
;; you.

(define-frame-preference "Default"
  ;; frame raise lock (lock AND raise == jumpto)
  (0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
  (1 t nil :class "XTerm"))

(define-frame-preference "Ardour"
  (0 t   t   :instance "ardour_editor" :type :normal)
  (0 t   t   :title "Ardour - Session Control")
  (0 nil nil :class "XTerm")
  (1 t   nil :type :normal)
  (1 t   t   :instance "ardour_mixer")
  (2 t   t   :instance "jvmetro")
  (1 t   t   :instance "qjackctl")
  (3 t   t   :instance "qjackctl" :role "qjackctlMainForm"))

(define-frame-preference "Shareland"
  (0 t   nil :calss "XTerm")
  (1 nil t   :class "aMule"))


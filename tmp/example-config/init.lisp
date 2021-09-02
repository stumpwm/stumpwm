(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))

(in-package :stumpwm)
(defvar *config-dir* "~/.stumpwm.d/")

;;; Applications

(define-key *top-map* (stumpwm:kbd "s-RET")
  ; replace alacritty with your favorite terminal
  "run-shell-command alacritty")

;;; Modeline

(setf *mode-line-position* :bottom)
(define-key *top-map* (stumpwm:kbd "s-M") "mode-line")
(enable-mode-line (current-screen) (current-head) t)

(load (str:concat *config-dir* "dfg-setup.lisp"))



;;; Group / Workspace

;; Create group/workspace layout.
(when (ignore-errors (ql:quickload :stumpwm))
  (load (str:concat *config-dir*
                    "dynamic-floating-group.lisp")))
(grename "1")
(loop for n in '(2 3 4 5 6 7 8 9 0)
      do (stumpwm-dfg::gnew-dyn-float-bg (format nil "~a" n)))
(gkill)
(stumpwm-dfg::gnew-dyn-float "1")

;; Keybindings for selecting and moving groups.
(mapcar
 (lambda (n)
   (let ((shift-keys '(")" "!" "@" "#" "$"
                       "%" "^" "&" "*" "(")))
     (define-key *top-map*
         (kbd (format nil "s-~a" n))
       (format nil "gselect ~a" n))
     (define-key *top-map*
         (kbd (format nil "s-~a" (nth n shift-keys)))
       (format nil "gmove ~a" n))))
 '(1 2 3 4 5 6 7 8 9 0))



;;; Keybindings for features

(in-package :stumpwm-dfg)
(define-key *top-map* (stumpwm:kbd "s-j") "focus-next-window")
(define-key *top-map* (stumpwm:kbd "s-k") "focus-last-window")
(define-key *top-map* (stumpwm:kbd "s-J") "permute-window-list")
(define-key *top-map* (stumpwm:kbd "s-K") "permute-window-list--reverse")
(define-key *top-map* (stumpwm:kbd "s-_") "unfree-all")
(define-key *top-map* (stumpwm:kbd "s-+") "increase-master-ratio")
(define-key *top-map* (stumpwm:kbd "s--") "decrease-master-ratio")
(define-key *top-map* (stumpwm:kbd "s-=") "set-default-master-ratio")
(define-key *top-map* (stumpwm:kbd "s-f") "toggle-fullscreen-layout")
(define-key *top-map* (stumpwm:kbd "s-F") "select-next-layout")
(define-key *top-map* (stumpwm:kbd "s-F") "select-next-layout")
(define-key *top-map* (stumpwm:kbd "s-M-,") "window-size-increase")
(define-key *top-map* (stumpwm:kbd "s-M-m") "window-size-decrease")
(define-key *top-map* (stumpwm:kbd "s-M-j") "window-move-down")
(define-key *top-map* (stumpwm:kbd "s-M-k") "window-move-up")
(define-key *top-map* (stumpwm:kbd "s-M-l") "window-move-right")
(define-key *top-map* (stumpwm:kbd "s-M-h") "window-move-left")
(define-key *top-map* (stumpwm:kbd "s-[") "gap-toggle")
(define-key *top-map* (stumpwm:kbd "s-]") "gap-set-default")
(define-key *top-map* (stumpwm:kbd "s-}") "gap-increase")
(define-key *top-map* (stumpwm:kbd "s-{") "gap-decrease")

(in-package :stumpwm)

(defvar *minor-mode*)

(defclass minor-mode ()
  ((group
    :initarg :group
    :initform nil)
   (global
    :initarg :global
    :accessor minor-mode-global-p
    :initform nil)))

(defun minor-mode-p (obj)
  (typep obj 'minor-mode))

(defmethod minor-mode-group ((minor-mode minor-mode))
  (let ((g (slot-value minor-mode 'group)))
    (if (typep g 'group)
        g
        (current-group))))

(defmethod (setf minor-mode-group) (new (minor-mode minor-mode))
  (setf (slot-value minor-mode 'group) new))

(defmethod initialize-instance :after ((obj minor-mode) &key &allow-other-keys)
  (let ((group (minor-mode-group obj)))
    (if (minor-mode-global-p obj)
        (progn (push obj (slot-value group 'global-minor-modes))
               (setf (minor-mode-group obj) :global))
        (push obj (slot-value group 'minor-modes)))))

;;; Minor mode generics

(defgeneric minor-mode-id (minor-mode)
  (:method ((minor-mode minor-mode))
    (format nil "(~A ~A)"
            (type-of minor-mode)
            (group-name (minor-mode-group minor-mode))))
  (:documentation
   "Return an ID for the minor mode, suitable for click events in the mode line"))

(defgeneric minor-mode-on-enable (minor-mode group)
  (:method ((minor-mode minor-mode) (group group))
    (sync-keys))
  (:documentation "Run when a minor mode is enabled"))

(defgeneric minor-mode-on-disable (minor-mode group)
  (:method ((minor-mode minor-mode) (group group))
    (sync-keys))
  (:documentation "Run when a minor mode is disabled"))

(defgeneric minor-mode-keymap (minor-mode)
  (:method (minor-mode) nil)
  (:documentation "Return the top map for the minor mode"))

(defgeneric minor-mode-top-maps (minor-mode)
  (:method ((group group))
    (loop for minor-mode in (group-minor-modes group)
          collect (minor-mode-keymap minor-mode)))
  (:documentation "Return a list of keymaps for all active minor modes."))


;;; Keymaps

(defun generate-keymap (keymap-spec &optional
                                      (top-map (stumpwm:make-sparse-keymap))
                                      (filter-bindings #'identity))
  "Generate a (potentially nested) keymap based on KEYMAP. KEYMAP is a list of
keymap specs, where each spec is a cons cell containing an input sequence and
something to bind it to. The input sequence is a string representing an
arbitrary sequence of keys, eg \"C-x C-s\". The thing to bind it to is an
arbitrary thing which will be passed to FILTER-BINDINGS, which defaults to
#'identity. TOP-MAP is the keymap to bind everything in, and defaults to an
empty keymap."
  (let* ((topmap top-map)
         (curmap topmap)
         (keymap keymap-spec))
    (flet
        ((create-keymap-binding (keys)
           (let ((in-seq (car keys))
                 (bind-to (cdr keys)))
             (labels
                 ((bind-it (key &optional to)
                    (cond (to (stumpwm:define-key curmap (stumpwm:kbd key) to))
                          (t (stumpwm:define-key curmap (stumpwm:kbd key)
                               (funcall filter-bindings bind-to)))))
                  (attempt-binding (key rest bind seq)
                    (cond
                      ((and bind (stumpwm::kmap-p bind))
                       (if (null rest)
                           (restart-case
                               (error "~A in ~A is already bound to a keymap"
                                      (stumpwm::print-key (stumpwm:kbd key)) seq)
                             (keep-binding ()
                               :report "Keep the current binding"
                               nil)
                             (replace-binding ()
                               :report (lambda (s)
                                         (format s "Replace with binding ~A"
                                                 bind-to))
                               (bind-it key)))
                           (setf curmap bind)))
                      (bind
                       ;; If bind exists and is not a keymap then were replacing
                       ;; a binding regardless of whether REST is NULL or not,
                       ;; its just a question of whether were replacing it with
                       ;; another command/symbol/function or a keymap.
                       (restart-case (error "~A in ~A is already bound to ~A"
                                            (stumpwm::print-key (stumpwm:kbd key))
                                            seq
                                            bind)
                         (replace-binding ()
                           :report
                           (lambda (s)
                             (format s "Replace with binding ~A"
                                     (if (null rest)
                                         bind-to
                                         (format nil "the keymap ~{~A~^ ~}"
                                                 rest))))
                           (bind-it key))))
                      ((null rest)
                       (bind-it key))
                      (t (let ((m (stumpwm:make-sparse-keymap)))
                           (bind-it key m)
                           (setf curmap m)))))
                  (traverse-and-bind (seq)
                    (loop for (key . rest) on (cl-ppcre:split " " seq)
                          do (let ((bind (stumpwm:lookup-key curmap
                                                             (stumpwm:kbd key))))
                               (attempt-binding key rest bind seq)))))
               ;; Labels body
               (if (not (or (symbolp bind-to)
                            (stringp bind-to)
                            (functionp bind-to)))
                   (restart-case (error "Invalid binding ~A" bind-to)
                     (bind-anyway ()
                       :report "Bind the key binding regardless"
                       (traverse-and-bind in-seq))
                     (skip-binding ()
                       :report "skip this binding"
                       nil))
                   (traverse-and-bind in-seq))))))
      ;; Flet body, check how we should generate kmaps
      (cond ((null keymap)
             topmap)
            ((or (symbolp keymap)
                 (stumpwm::kmap-p keymap))
             keymap)
            ((listp keymap)
             (restart-case (mapc (lambda (keys)
                                   (create-keymap-binding keys)
                                   (setf curmap topmap))
                                 keymap)
               (abort-bindings ()
                 :report "Return the keymap without binding further keys"
                 topmap)
               (abort-bindings* ()
                 :report "Return an empty keymap"
                 (stumpwm:make-sparse-keymap)))
             topmap)
            (t (restart-case
                   (error "Function MAKE-MINOR-MODE-KEYMAP cant understand ~A"
                          keymap)
                 (use-empty-keymap ()
                   :report "Use an empty keymap"
                   (stumpwm:make-sparse-keymap))))))))

(defun make-minor-mode-keymap (spec)
  (generate-keymap spec))

(defun make-minor-mode-top-map (top-map-spec root-map-spec)
  "Create a top map for a minor mode based upon its TOP-MAP-SPEC and
ROOT-MAP-SPEC."
  (let ((top-map nil)
        (root-map (if root-map-spec
                      (make-minor-mode-keymap root-map-spec)
                      (make-sparse-keymap))))
    (fill-keymap top-map *escape-key* root-map)
    (generate-keymap top-map-spec top-map)))

;; Usage of the define-minor-mode macro:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-special-variable-name (mode name)
    (intern (format nil "*~A-~A*" mode name)))
  
  (defun make-minor-mode-lighter (lighter make-clickable)
    (cond ((stringp lighter)
           (if make-clickable
               `(lambda (mode)
                 (format-with-on-click-id ,lighter
                  :ml-on-click-minor-mode
                  (minor-mode-id mode)))
               `(lambda (mode)
                 (declare (ignore mode))
                 ,lighter)))
          ((functionp lighter)
           (if make-clickable
               `(lambda (mode)
                 (format-with-on-click-id (funcall ,lighter mode)
                  :ml-on-click-minor-mode
                  (minor-mode-id mode)))
               lighter))
          ((symbolp lighter)
           (if make-clickable
               `(lambda (mode)
                 (format-with-on-click-id
                  (cond ((fboundp ,lighter)
                         (funcall ,lighter mode))
                        ((boundp ,lighter)
                         (symbol-value ,lighter))
                        (t ""))
                  :ml-on-click-minor-mode
                  (minor-mode-id mode)))
               `(lambda (mode)
                 (cond ((fboundp ,lighter)
                        (funcall ,lighter mode))
                       ((boundp ,lighter)
                        (symbol-value ,lighter))
                       (t "")))))
          (t
           `(lambda (mode)
             (declare (ignore mode))
             ""))))
  
  (defun parse-minor-mode-options (options)
    (let ((valid-options
            '((:interactive            1)
              (:global                 1)
              (:lighter-make-clickable 1)
              (:lighter                1)
              (:expose-keymaps         1)
              (:root-map               t)
              (:top-map                t)
              (:on-enable              1)
              (:on-disable             1)
              (:make-hooks             1)
              (:default-initargs       t)
              (:define-command-definer 1)))
          (all-vals '())
          (other-opts '()))
      (flet ((collect-values (option)
               (let ((argcount (cadr (assoc (car option) valid-options))))
                 (if argcount
                     (progn (if (and (numberp argcount)
                                     (= argcount 1))
                                (push (cadr option) all-vals)
                                (push (cdr option) all-vals))
                            (push (car option) all-vals))
                     (push option other-opts)))))
        (mapc #'collect-values options)
        (values all-vals other-opts))))

  (defun minor-mode-parse-default-initargs (default-initargs global gpp)
    (if gpp
        (let ((copy (copy-seq default-initargs)))
          (remf copy :global)
          (cons :global (cons global copy)))
        default-initargs))

  (defun define-command-macro (mode)
    `(defmacro ,(intern (string-upcase (format nil "define-~A-command" mode))
                        (find-package :stumpwm))
         (name (&rest args) (&rest interactive-args) &body body)
       `(defcommand (,name ,',mode) ,args ,interactive-args
          (let ((*minor-mode* (find-minor-mode ',',mode (current-group))))
            ,@body)))))

(defmacro define-minor-mode (mode superclasses slots &rest options)
  "Define a minor mode. Define a class which will be instantiated when the minor
mode is activated. If @var{SUPERCLASSES} is provided then it will override the
default superclass of MINOR-MODE. @var{OPTIONS} may include all normal options
when defining a class, with the addition of the following options:

:TOP-MAP spec ...
:ROOT-MAP spec ...
:EXPOSE-KEYMAPS bool
:LIGHTER function
:LIGHTER-MAKE-CLICKABLE bool
:GLOBAL bool
:INTERACTIVE bool
:ON-ENABLE function
:ON-DISABLE function
:MAKE-HOOKS bool
:DEFINE-COMMAND-DEFINER bool

:TOP-MAP and :ROOT-MAP take an arbitrary number of key specifications, where the
key is a string denoting one or more key presses separated by a single space and
the binding is a normal binding.

When :EXPOSE-KEYMAPS is T dynamic variables are defined to hold the top and root
maps. They are named *MODE-[top|root]-map*.

:LIGHTER may be a function, a string, a symbol, or nil. When :LIGHTER is a
function it is called with the mode instance. When :LIGHTER is a string it is
returned as is. When :LIGHTER is a symbol, if it is fbound it is called with the
mode, otherwise if it is bound its value is returned. When :LIGHTER is null an
empty string is returned. 

When :LIGHTER-MAKE-CLICKABLE is true then :LIGHTER is wrapped in a call to
format-with-on-click-id with the id :ML-ON-CLICK-MINOR-MODE and the argument
retrieved by calling the generic function MINOR-MODE-ID with the mode instance.

When :GLOBAL is true, the minor mode will be enabled and disabled in every
group. When it is nil the minor mode will only be enabled and disabled in the
specified group.

When :INTERACTIVE is true a command with the same name as MODE is defined which
toggles the minor mode.

If :ON-ENABLE is provided it must be a function which takes the mode intance and
the group it was enabled in.

If :ON-DISABLE is provided it must be a function which takes the mode intance
and the group it was enabled in.

When :MAKE-HOOKS is true a set of hook variables are defined named
*MODE-[enable|disable]-hook*. When enabling or disabling a minor mode the
:ON-ENABLE and :ON-DISABLE functions are run before the hooks.

When :DEFINE-COMMAND-DEFINER is true a macro is defined to define commands that
are only active when the minor mode is active in the current group. This macro
is named define-MODE-command. 
"
  (when (null superclasses)
    (setq superclasses '(minor-mode)))
  (multiple-value-bind (mm-opts other-opts)
      (parse-minor-mode-options options)
    (destructuring-bind (&key top-map root-map expose-keymaps
                           lighter (lighter-make-clickable t)
                           (global t gpp) interactive
                           on-enable on-disable
                           (make-hooks t) (define-command-definer t)
                           default-initargs)
        mm-opts
      (with-gensyms (gmode gkeymap glighter ggroup)
        `(progn
           ,@(when expose-keymaps 
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (defvar ,(make-special-variable-name mode 'root-map)
                     (make-minor-mode-keymap ',root-map)))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (defvar ,(make-special-variable-name mode 'top-map)
                     (make-minor-mode-top-map
                      ',top-map
                      ',(make-special-variable-name mode 'root-map))))))
           ,@(when make-hooks
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (defvar ,(make-special-variable-name mode 'enable-hook) nil
                     ,(format nil "A hook run when enabling ~A, called with the mode object and a group" mode))
                   (defvar ,(make-special-variable-name mode 'disable-hook) nil
                     ,(format nil "A hook run when disabling ~A, called with the mode object and a group" mode)))))
           (defclass ,mode ,superclasses
             ((,gkeymap
               :initform ,@(if expose-keymaps
                               `(',(make-special-variable-name mode 'top-map))
                               `((make-minor-mode-top-map
                                  ',top-map
                                  (make-minor-mode-keymap ',root-map))
                                 ;; (make-minor-mode-keymap ',root-map)
                                 ))
               :allocation :class)
              (,glighter
               :initform
               ,(make-minor-mode-lighter lighter lighter-make-clickable)
               :allocation :class)
              ,@slots)
             (:default-initargs
              ,@(minor-mode-parse-default-initargs default-initargs global gpp))
             ,@other-opts)
           (defmethod minor-mode-keymap ((,gmode ,mode))
             (slot-value ,gmode ',gkeymap))
           ,@(when interactive
               `((defcommand ,mode () ()
                   (if (minor-mode-enabled-p ',mode)
                       (progn (disable-minor-mode ',mode)
                              (message "~A disabled" ',mode))
                       (progn (enable-minor-mode ',mode)
                              (message "~A enabled" ',mode))))))
           ,@(when (or on-enable make-hooks)
               `((defmethod minor-mode-on-enable ((,gmode ,mode) (,ggroup group))
                   ,@(when on-enable
                       `((funcall ,on-enable ,gmode ,ggroup)))
                   (call-next-method)
                   ,@(when make-hooks
                       `((run-hook-with-args
                          ,(make-special-variable-name mode 'enable-hook)
                          ,gmode ,ggroup))))))
           ,@(when (or on-disable make-hooks)
               `((defmethod minor-mode-on-disable ((,gmode ,mode) (,ggroup group))
                   ,@(when on-disable
                       `((funcall ,on-disable ,gmode ,ggroup)))
                   (call-next-method)
                   ,@(when make-hooks
                       `((run-hook-with-args
                          ,(make-special-variable-name mode 'disable-hook)
                          ,gmode ,ggroup))))))
           ,@(when define-command-definer
               (list (define-command-macro mode))))))))

(defun minor-mode-enabled-p (mode &optional (group (current-group)))
  (check-type mode symbol)
  (member-if (lambda (m)
               (typep m mode))
             (group-minor-modes group)))

(defun find-minor-mode (mode group)
  (car (minor-mode-enabled-p mode group)))

(defun enable-minor-mode (mode &optional (group (current-group)))
  (check-type mode symbol)
  (let ((minor-modes (group-minor-modes group)))
    (if (some (lambda (mm) (typep mm mode)) minor-modes)
        (message "Minor mode ~A is already active in group ~A" mode group)
        (let ((mm (make-instance mode :group group)))
          (minor-mode-on-enable mm group)))))

(defun disable-minor-mode (mode &optional (group (current-group)))
  (check-type mode symbol)
  (let ((mm (find-if (lambda (mm)
                       (typep mm mode))
                     (group-minor-modes group))))
    (if (minor-mode-global-p mm)
        (setf (slot-value group 'global-minor-modes)
              (remove mm (slot-value group 'global-minor-modes)))
        (setf (slot-value group 'minor-modes)
              (remove mm (slot-value group 'minor-modes))))
    (minor-mode-on-disable mm group)))

;;; Example minor mode definition

;; (define-minor-mode my-test-mode () ()
;;   (:top-map ("s-o" . "fnext"))
;;   (:interactive t))

;; (define-my-test-mode-command test-cmd () ()
;;   (message "Successful command filtering, in mode ~A" *minor-mode*))

;; (define-minor-mode music-player-mode (mdc:music-daemon-connection minor-mode)
;;     ()
;;   (:top-map ("s-p" . "my-music-player-volume-up-command")
;;             ("s-n" . "my-music-player-volume-down-command")
;;             ("s-f" . "my-music-player-next-track-command")
;;             ("s-p" . "my-music-player-prev-track-command"))
;;   (:root-map ("C-M-m o" . "my-music-player-open-player-command")
;;              ("C-M-m s" . "my-music-player-skip-n-tracks-cmd"))
;;   (:expose-keymaps t)
;;   (:lighter-make-clickable nil)
;;   (:lighter
;;    (lambda (mode)
;;      (format-with-on-click-id 
;;       (music-player-mode-current-track mode)
;;       :ml-on-click-music-player)))
;;   (:interactive t)
;;   (:on-enable (lambda (mode group)
;;                 (setf (music-player-mode-daemon mode)
;;                       (connect-to-music-daemon))))
;;   (:on-disable (lambda (mode group)
;;                  (disconnect-from-music-daemon (music-player-mode-daemon mode))
;;                  (setf (music-player-mode-daemon mode) nil))))

;; (define-music-player-mode-command my-music-player-volume-up-command (amnt)
;;     ((:number "Increment Volume by: "))
;;   (incf (volume (music-player-mode-daemon *minor-mode*)) amnt))

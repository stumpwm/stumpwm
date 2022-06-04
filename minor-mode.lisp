(in-package :stumpwm)

(export '(define-minor-mode
          minor-mode-id
          minor-mode-scope
          enable-minor-mode
          disable-minor-mode
          minor-mode-on-enable
          minor-mode-on-disable
          minor-mode-keymap
          minor-mode-lighter
          generate-keymap))

(defvar *minor-mode*)

(defvar *active-global-minor-modes* nil
  "A list of active minor modes with a global scope.")

(defun global-minor-modes (&optional ignore)
  (declare (ignore ignore))
  *active-global-minor-modes*)

(defun set-global-minor-modes (ignore value)
  (declare (ignore ignore))
  (setf *active-global-minor-modes* value))

(defsetf global-minor-modes set-global-minor-modes)

(defvar *active-frame-minor-modes* nil
  "An alist of active minor modes with the frame scope with frame numbers as keys")

(defun frame-minor-modes (frame)
  (check-type frame frame)
  (let ((number (frame-number frame)))
    (cdr (assoc number *active-frame-minor-modes*))))

(defun set-frame-minor-modes (frame value)
  (check-type frame frame)
  (let* ((number (frame-number frame))
         (a (assoc number *active-frame-minor-modes*)))
    (if a
        (setf (cdr (assoc number *active-frame-minor-modes*)) value)
        (push (cons number value) *active-frame-minor-modes*))))

(defsetf frame-minor-modes set-frame-minor-modes)

;; Hooks for ensuring keys are synced

(defun minor-mode-frame-focus-hook (new old)
  (when (or (frame-minor-modes new)
            (frame-minor-modes old))
    (sync-keys)))

(defun minor-mode-window-focus-hook (new old)
  (when (or (and new (window-minor-modes new))
            (and old (window-minor-modes old)))
    (sync-keys)))

(add-hook *focus-frame-hook* #'minor-mode-frame-focus-hook)
(add-hook *focus-window-hook* #'minor-mode-window-focus-hook)

;; Mode class

(defclass minor-mode ()
  ((scoped-to
    :initarg :scope-object
    :initform nil
    :accessor minor-mode-scope-object
    :documentation
    "Hold a reference to the scope object for the minor mode instance.")))

;; Every minor mode class defined must have a method MINOR-MODE-SCOPE which
;; takes a minor mode object and returns its scope as a keyword.

(defun minor-mode-p (obj)
  (typep obj 'minor-mode))

;;; Minor mode generics

(defgeneric minor-mode-scope (minor-mode)
  (:documentation "Return as a keyword the scope of the minor mode"))

(defgeneric minor-mode-id (minor-mode)
  (:method ((minor-mode minor-mode))
    (format nil "(~A ~A)"
            (type-of minor-mode)
            (type-of (minor-mode-scope minor-mode))))
  (:documentation
   "Return an ID for the minor mode, suitable for click events in the mode line"))

(defgeneric enable-minor-mode (mode &optional scope-object)
  (:documentation
   "Enable the minor mode MODE for the scopes current object or SCOPE-OBJECT if
provided.")
  (:method (mode &optional obj)
    (declare (ignore obj))
    (error "Dont know how to enable minor mode ~A" mode)))

(defgeneric disable-minor-mode (mode &optional scope-object)
  (:documentation
   "Disable the minor mode MODE for the scopes current object or SCOPE-OBJECT if
provided.")
  (:method (mode &optional obj)
    (declare (ignore obj))
    (error "Dont know how to disable minor mode ~A" mode)))

(defgeneric minor-mode-on-enable (minor-mode)
  (:method ((minor-mode minor-mode))
    (sync-keys))
  (:documentation "Run when a minor mode is enabled"))

(defgeneric minor-mode-on-disable (minor-mode)
  (:method ((minor-mode minor-mode))
    (sync-keys))
  (:documentation "Run when a minor mode is disabled"))

(defgeneric minor-mode-keymap (minor-mode)
  (:method (minor-mode) nil)
  (:documentation "Return the top map for the minor mode"))

(defgeneric minor-mode-lighter (minor-mode)
  (:documentation "Return a string for display in the mode line"))

(defun list-minor-modes (&key (group (current-group)) frame window head screen)
  (let* ((f (or frame (tile-group-current-frame group)))
         (w (or window (frame-window f)))
         (h (or head (group-current-head group)))
         (s (or screen (group-screen group))))
    (append (when w (window-minor-modes w))
            (frame-minor-modes f)
            (head-minor-modes h)
            (group-minor-modes group)
            (screen-minor-modes s)
            (global-minor-modes))))

(defun minor-mode-enabled-p (mode &rest rest &key (group (current-group))
                                               frame window head screen)
  (declare (ignore group frame window head screen))
  (check-type mode symbol)
  (member-if (lambda (m)
               (typep m mode))
             (apply 'list-minor-modes rest)))

(defun find-minor-mode (mode group)
  (car (minor-mode-enabled-p mode :group group)))

(defun minor-mode-top-maps (group)
  "Return a list of keymaps for all active minor modes."
  (check-type group group)
  (let ((minor-modes (list-minor-modes :group group)))
    (flatten 
     (loop for mode in minor-modes
           collect (minor-mode-keymap mode)))))

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

;; lighter formatter

(add-screen-mode-line-formatter #\m 'fmt-minor-modes)
(defun fmt-minor-modes (ml)
  (let ((minor-modes (list-minor-modes :group (mode-line-current-group ml))))
    (format nil "~{~A~^ ~}"
            (loop for mode in minor-modes
                  collect (minor-mode-lighter mode)))))

;; Usage of the define-minor-mode macro:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-special-variable-name (mode name)
    (intern (format nil "*~A-~A*" mode name)))
  
  (defun parse-minor-mode-options (options)
    (let ((valid-options
            '((:interactive            1)
              (:scope                  1)
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
            ,@body))))
  
  (defun define-enable-methods (mode scope)
    (let ((optarg (case scope
                    ((:window) '((current-window)
                                 window-minor-modes
                                 window))
                    ((:frame)  '((tile-group-current-frame (current-group))
                                 frame-minor-modes
                                 frame))
                    ((:group)  '((current-group)
                                 group-minor-modes
                                 group))
                    ((:head)   '((current-head)
                                 head-minor-modes
                                 head))
                    ((:screen) '((current-screen)
                                 screen-minor-modes
                                 screen))
                    ((:global) '(*active-global-minor-modes*
                                 global-minor-modes
                                 t))
                    (otherwise
                     (error "Unknown minor mode scope ~A" scope)))))
      `((defmethod enable-minor-mode ((mode (eql ',mode))
                                      &optional (obj ,(car optarg)))
          (check-type obj ,(caddr optarg))
          (let ((minor-modes (,(cadr optarg) obj)))
            (if (some (lambda (mm) (typep mm mode)) minor-modes)
                (message "Minor mode ~A is already active in object ~A"
                         mode ,(if (eql scope :global)
                                   ''*active-global-minor-modes*
                                   'obj))
                (let ((mm (make-instance mode :scope-object obj)))
                  (push mm (,(cadr optarg) obj))
                  (minor-mode-on-enable mm)
                  (message "~A enabled" ',mode)))))
        (defmethod enable-minor-mode ((mode ,mode)
                                      &optional (obj ,(car optarg)))
          (check-type obj ,(caddr optarg))
          (let ((minor-modes (,(cadr optarg) obj)))
            (if (some (lambda (mm) (typep mm (type-of mode))) minor-modes)
                (message "Minor mode ~A is already active in object ~A"
                         (type-of mode) ,(if (eql scope :global)
                                             ''*active-global-minor-modes*
                                             'obj))
                (progn (push mode (,(cadr optarg) obj))
                       (minor-mode-on-enable mode)
                       (message "~A enabled" ',mode)))))
        (defmethod disable-minor-mode ((mode (eql ',mode))
                                       &optional (obj ,(car optarg)))
          (check-type obj ,(caddr optarg))
          (let ((mm (find-if (lambda (mm) (typep mm mode)) (,(cadr optarg) obj))))
            (when mm
              (setf (,(cadr optarg) obj) (remove mm (,(cadr optarg) obj)))
              (minor-mode-on-disable mm)
              (message "~A disabled" ',mode))))
        (defmethod disable-minor-mode ((mode ,mode)
                                       &optional (obj ,(car optarg)))
          (check-type obj ,(caddr optarg))
          (let ((mm (find mode (,(cadr optarg) obj))))
            (when mm
              (setf (,(cadr optarg) obj) (remove mm (,(cadr optarg) obj)))
              (minor-mode-on-disable mm)
              (message "~A disabled" ',mode)))))))

  (defun genlighter (mode lighter)
    (cond ((functionp lighter)
           lighter)
          ((stringp lighter)
           `(lambda (mode)
              (declare (ignore mode))
              ,lighter))
          ((and (symbolp lighter) (not (eql nil lighter)))
           `(function ,lighter))
          ((and (listp lighter)
                (eql (car lighter) 'lambda))
           lighter)
          (t
           `(lambda (mode)
              (declare (ignore mode))
              ,(subseq (format nil "~A" mode) 0 1))))))

(defmacro define-minor-mode (mode superclasses slots &rest options)
  "Define a minor mode. Define a class which will be instantiated when the minor
mode is activated. If @var{SUPERCLASSES} is provided then it will override the
default superclass of MINOR-MODE. @var{OPTIONS} may include all normal options
when defining a class, with the addition of the following options:

@verbatim
:TOP-MAP                spec ...
:ROOT-MAP               spec ...
:EXPOSE-KEYMAPS         bool
:LIGHTER                (or function string symbol null)
:LIGHTER-MAKE-CLICKABLE bool
:SCOPE                  keyword
:INTERACTIVE            bool
:ON-ENABLE              function
:ON-DISABLE             function
:MAKE-HOOKS             bool
:DEFINE-COMMAND-DEFINER bool
@end verbatim

:TOP-MAP and :ROOT-MAP take an arbitrary number of key specifications, where the
key is a string denoting one or more key presses separated by a single space and
the binding is an unquoted binding.

When :EXPOSE-KEYMAPS is T dynamic variables are defined to hold the top and root
maps. They are named *MODE-[top|root]-map*. This option defaults to T.

:LIGHTER may be a function, a string, a symbol, or nil. When :LIGHTER is a
function it is called with the mode instance. When :LIGHTER is a string it is
returned as is. When :LIGHTER is a symbol it is assumed to denote a function
that takes a single mode object and returns a string. When :LIGHTER is null then
the first letter of the minor mode is used.

When :LIGHTER-MAKE-CLICKABLE is true then whatever is provided to :LIGHTER is
wrapped in a call to format-with-on-click-id with the id :ML-ON-CLICK-MINOR-MODE
and the argument retrieved by calling the generic function MINOR-MODE-ID with
the mode instance. This option defaults to T.

:SCOPE is a keyword denoting what the scope of the minor mode is. This may be
one of :window :frame, :group, :head, :screen, or :global. When :SCOPE is
:window :frame :group :head or :screen the minor mode is only active when the
window/frame/group/head/screen it is scoped to is active. When :SCOPE is :global
the minor mode is always active. This option defaults to :global.

When :INTERACTIVE is true a command with the same name as MODE is defined which
toggles the minor mode.

If :ON-ENABLE is provided it must be a function which takes the mode instance.

If :ON-DISABLE is provided it must be a function which takes the mode instance.

When :MAKE-HOOKS is true a set of hook variables are defined named
*MODE-[enable|disable]-hook*. When enabling or disabling a minor mode the
:ON-ENABLE and :ON-DISABLE functions are run before the hooks. Functions hung on
these hooks are called with the minor mode object. This option defaults to T.

When :DEFINE-COMMAND-DEFINER is true a macro is defined to define commands that
are only active when the minor mode is active in the current group. This macro
is named define-MODE-command. This option defaults to T."
  (when (null superclasses)
    (setq superclasses '(minor-mode)))
  (multiple-value-bind (mm-opts other-opts)
      (parse-minor-mode-options options)
    (destructuring-bind (&key top-map root-map (expose-keymaps t)
                           lighter (lighter-make-clickable t)
                           (scope :global) interactive
                           on-enable on-disable
                           (make-hooks t) (define-command-definer t)
                           default-initargs)
        mm-opts
      (with-gensyms (gmode gkeymap)
        `(progn
           ,@(when expose-keymaps 
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (defparameter ,(make-special-variable-name mode 'root-map)
                     (make-minor-mode-keymap ',root-map)))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (defparameter ,(make-special-variable-name mode 'top-map)
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
              ,@slots)
             (:default-initargs ,@default-initargs)
             ,@other-opts)
           (defmethod minor-mode-lighter ((,gmode ,mode))
             ,(if lighter-make-clickable
                  `(format-with-on-click-id (funcall ,(genlighter mode lighter)
                                                     ,gmode)
                                            :ml-on-click-minor-mode
                                            (minor-mode-id ,gmode))
                  `(funcall ,(genlighter mode lighter) ,gmode)))
           (defmethod minor-mode-scope ((,gmode ,mode))
             (declare (ignore ,gmode))
             ,scope)
           (defmethod minor-mode-keymap ((,gmode ,mode))
             (slot-value ,gmode ',gkeymap))
           ,@(define-enable-methods mode scope)
           ,@(when interactive
               `((defcommand ,mode (&optional (yn nil ynpp)) ((:y-or-n))
                   (flet ((enable () (enable-minor-mode ',mode))
                          (disable () (disable-minor-mode ',mode)))
                     (cond (yn (enable))
                           (ynpp (disable))
                           ((minor-mode-enabled-p ',mode) (disable))
                           (t (enable)))))))
           ,@(when (or on-enable make-hooks)
               `((defmethod minor-mode-on-enable ((,gmode ,mode))
                   ,@(when on-enable
                       `((funcall ,on-enable ,gmode)))
                   (call-next-method)
                   ,@(when make-hooks
                       `((run-hook-with-args
                          ,(make-special-variable-name mode 'enable-hook)
                          ,gmode))))))
           ,@(when (or on-disable make-hooks)
               `((defmethod minor-mode-on-disable ((,gmode ,mode))
                   ,@(when on-disable
                       `((funcall ,on-disable ,gmode)))
                   (call-next-method)
                   ,@(when make-hooks
                       `((run-hook-with-args
                          ,(make-special-variable-name mode 'disable-hook)
                          ,gmode))))))
           ,@(when define-command-definer
               (list (define-command-macro mode))))))))

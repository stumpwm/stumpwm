(in-package :stumpwm)

(export '(define-minor-mode

          *minor-mode*
          *minor-mode-enable-hook*
          *minor-mode-disable-hook*
          *global-minor-modes*

          minor-mode-scope
          enable-minor-mode
          disable-minor-mode
          minor-mode-keymap
          minor-mode-lighter

          list-modes
          list-minor-modes
          current-minor-modes
          minor-mode-enabled-p
          find-minor-mode

          generate-keymap))

(defvar *minor-mode*)

(setf (documentation '*minor-mode* 'variable)
"A dynamic variable bound to the minor mode object when executing a minor mode
command.")

(defvar *minor-mode-enable-hook* ()
  "A hook run whenever a minor mode is enabled. Functions are called with the
minor mode symbol and the object they have been added to.")

(defvar *minor-mode-disable-hook* ()
  "A hook run whenever a minor mode is disabled. Functions are called with the
minor mode symbol and the object they will be removed from.")

(defclass global-modes () ())

(defvar *global-minor-modes* (make-instance 'global-modes)
  "A dynamic variable holding all global minor modes as mixed into the same
object.")

(defclass minor-mode () ())

(defgeneric minor-mode-scope (minor-mode-symbol)
  (:documentation "Return as a keyword the scope of the minor mode"))

(defgeneric enable-minor-mode (mode scope-object)
  (:documentation
   "Enable the minor mode MODE for the scopes current object or SCOPE-OBJECT if
provided.")
  (:method (mode obj)
    (declare (ignore obj))
    (error "Dont know how to enable minor mode ~A" mode))
  (:method :after (mode obj)
    (run-hook-with-args *minor-mode-enable-hook* mode obj)
    (sync-keys)))

(defgeneric disable-minor-mode (mode scope-object)
  (:documentation
   "Disable the minor mode MODE for the scopes current object or SCOPE-OBJECT if
provided.")
  (:method (mode obj)
    (declare (ignore obj))
    (error "Dont know how to disable minor mode ~A" mode))
  (:method :after (mode obj)
    (run-hook-with-args *minor-mode-disable-hook* mode obj)
    (sync-keys)))

(defgeneric minor-mode-keymap (minor-mode)
  (:method (minor-mode) nil)
  (:documentation "Return the top map for the minor mode"))

(defgeneric minor-mode-lighter (mode)
  (:method (minor-mode) nil)
  (:method :around (mode)
    (apply #'concatenate 'string (call-next-method)))
  (:documentation "Return a string of minor mode lighters."))

(defun list-modes (object)
  "List all minor modes followed by the major mode for OBJECT."
  (when (typep object 'dynamic-mixins:mixin-object)
    (mapcar #'class-name (dynamic-mixins:mixin-classes (class-of object)))))

(defun list-minor-modes (object)
  "List all minor modes active in OBJECT"
  (butlast (list-modes object)))

(defun current-minor-modes (&optional (screen (current-screen)))
  "Return all currently active minor modes."
  (let* ((group (current-group screen))
         (head (current-head group))
         (frame (when (typep group 'tile-group)
                  (tile-group-current-frame group)))
         (window (group-current-window group)))
    (apply #'append
           (mapcar #'list-minor-modes
                   (list window frame head group screen *global-minor-modes*)))))

(defun minor-mode-enabled-p (minor-mode &optional (screen (current-screen)))
  "Return T if MINOR-MODE is active"
  (check-type minor-mode symbol)
  (member minor-mode (current-minor-modes screen)))

(defun find-minor-mode (minor-mode &optional (screen (current-screen)))
  "Return the minor mode object associated with MINOR-MODE."
  (check-type minor-mode symbol)
  (flet ((ct (o)
           (and (typep o minor-mode) o)))
    (let ((group (current-group screen)))
      (or (ct *global-minor-modes*)
          (ct screen)
          (ct group)
          (ct (current-head group))
          (ct (when (typep group 'tile-group)
                (tile-group-current-frame group)))
          (ct (group-current-window group))))))

(defun minor-mode-command-active-p (group command)
  (find-minor-mode (command-class command) (group-screen group)))

(push #'minor-mode-command-active-p *custom-command-filters*)

(defun minor-mode-top-maps (group)
  "Return a list of all minor mode top maps."
  (let* ((screen (group-screen group))
         (head (current-head group))
         (frame (when (typep group 'tile-group)
                  (tile-group-current-frame group)))
         (window (group-current-window group)))
    (apply #'append
           (mapcar #'minor-mode-keymap
                   (list window frame head group screen *global-minor-modes*)))))

(push #'minor-mode-top-maps *minor-mode-maps*)

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
              (:root-map               1)
              (:top-map                1)
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

  (defun define-command-macro (mode)
    `(defmacro ,(intern (string-upcase (format nil "define-~A-command" mode))
                        (find-package :stumpwm))
         (name (&rest args) (&rest interactive-args) &body body)
       `(defcommand (,name ,',mode) ,args ,interactive-args
          (let ((*minor-mode* (find-minor-mode ',',mode (current-screen))))
            ,@body))))
  
  (defun define-enable-methods (mode scope hooks-defined on-enable on-disable)
    (let ((optarg (case scope
                    ((:window) '((current-window) window))
                    ((:frame)  `((let ((group (current-group)))
                                   (if (typep group 'tile-group)
                                       (tile-group-current-frame group)
                                       (error "Cannot enable minor mode ~A~%~4TExpected a frame, but group ~A is not a tiling group" ,mode (group-name group))))
                                 frame))
                    ((:head)   '((current-head) head))
                    ((:group)  '((current-group) group))
                    ((:screen) '((current-screen) screen))
                    ((:global) '(*global-minor-modes* t))
                    (otherwise
                     (error "Unknown minor mode scope ~A" scope)))))
      `((defmethod enable-minor-mode ((mode (eql ',mode)) (obj ,mode))
          (error "Minor mode ~A is already active in object ~A" mode obj))
        (defmethod enable-minor-mode ((mode (eql ',mode)) (obj null))
          (enable-minor-mode mode ,(car optarg)))
        (defmethod enable-minor-mode ((mode (eql ',mode)) (obj ,(cadr optarg)))
          (dynamic-mixins:ensure-mix obj ',mode)
          ,@(when on-enable `((funcall ,on-enable mode obj)))
          ,@(when hooks-defined
              `((run-hook-with-args ,(make-special-variable-name mode 'enable-hook)
                                    mode obj))))
        (defmethod disable-minor-mode ((mode (eql ',mode)) (obj ,mode))
          ,@(when hooks-defined
              `((run-hook-with-args
                 ,(make-special-variable-name mode 'disable-hook)
                 mode obj)))
          ,@(when on-disable `((funcall ,on-disable mode obj)))
          (dynamic-mixins:delete-from-mix obj ',mode))
        (defmethod disable-minor-mode ((mode (eql ',mode)) (obj null))
          (disable-minor-mode mode ,(car optarg))))))

  (defun genlighter (mode lighter)
    (cond ((null lighter)
           (flet ((nullgen (s l)
                    (mapcar (lambda (e)
                              (if (or (string-equal e "mode") (< (length e) l))
                                  e
                                  (subseq e 0 l)))
                            s)))
             `(lambda (mode)
                (declare (ignore mode))
                ,(let ((split (remove-if (lambda (s) (string= s ""))
                                         (cl-ppcre:split "-" (symbol-name mode)))))
                   (format nil "~{~A~^-~}" (case (length split)
                                             ((1) split)
                                             ((2) (nullgen split 3))
                                             ((3) (nullgen split 2))
                                             (otherwise (nullgen split 1))))))))
          ((stringp lighter)
           `(lambda (mode)
              (declare (ignore mode))
              ,lighter))
          (t
           (when (or (symbolp lighter)
                     (and (listp lighter)
                          (not (or (eql (car lighter) 'lambda)
                                   (eql (car lighter) 'function)))))
             (warn "Assuming ~A is funcallable" lighter))
           lighter))))

(defmacro define-minor-mode (mode superclasses slots &rest options)
  "Define a minor mode as a class to be instantiated when the minor mode is
activated. Minor modes are dynamically mixed in to and out of the appropriate
object when they are enabled or disabled.

If @var{SUPERCLASSES} is not provided a default superclass of MINOR-MODE will be
provided. @var{OPTIONS} may include all normal options when defining a class,
with the addition of the following options:

@itemize
@item
(:SCOPE (MEMBER :WINDOW :FRAME :HEAD :GROUP :SCREEN :GLOBAL))@*
The :SCOPE option determines what object the minor mode shall be mixed in
with. This object is obtained by calling the appropriate CURRENT-* function if
it is not explicitly provided.

@item
(:TOP-MAP spec)@*
The minor modes top map is created based upon the provided spec,
which must be a list of cons cells whose car is a key sequence and whose cdr is
a binding. For example: @code{(list (cons \"C-m x\" \"exec\"))}. This would bind
the key sequence @kbd{C-m x} to the echo command. A reference to this keymap is
stored as a slot in the minor mode object and can be accessed via the reader
@code{MODE-KEYMAP} where @code{MODE} is the minor mode name.

@item
(:ROOT-MAP spec)@*
The minor modes root map is created based upon the provided spec. The spec is as
described in the :TOP-MAP option.

@item
(:EXPOSE-KEYMAPS (OR T NIL))@*
This value is used at macroexpansion time to determine whether or not to
generate keymap variables or store the keymap within the object. When T the
variables *MODE-TOP-MAP* and *MODE-ROOT-MAP* will be generated. 

@item
(:LIGHTER T)@*
The :LIGHTER option will be used to generate a function returning a string to
display in the mode line. When :LIGHTER is NULL a string is generated based upon
the mode name. When it is a string that string is used as is. Otherwise :LIGHTER
will assumed to be funcallable and used as is. When it is a symbol or a list
that doesn't begin with LAMBDA or FUNCTION a warning is issued that
DEFINE-MINOR-MODE is assuming it is funcallable. When assumed to be funcallable,
it is called with the mode object as its only argument.

@item
(:LIGHTER-MAKE-CLICKABLE (OR T NIL))@*
When :LIGHTER-MAKE-CLICKABLE is T then the :LIGHTER is wrapped in a call to
FORMAT-WITH-ON-CLICK-ID, called with the id :ML-ON-CLICK-MINOR-MODE and the mode
as a quoted symbol. 

@item
(:INTERACTIVE (OR T NIL))@*
The :INTERACTIVE option determines whether a command to toggle the minor mode on
and off is generated. If it is T then a command with the same name as the minor
mode is generated.

@item
(:ON-ENABLE FUNCTION)@*
The :ON-ENABLE option must be a funcallable object which will be called with the
mode symbol and the mode object. It will be called whenever the minor mode is
enabled. 

@item
(:ON-DISABLE FUNCTION)@*
The :ON-DISABLE option must be a funcallable object which will be called with
the mode symbol and the mode object. It will be called whenever the minor mode
is disabled.

@item
(:MAKE-HOOKS (OR T NIL))@*
When :MAKE-HOOKS is T a set of hook variables are generated. These variables are
called *MODE-ENABLE-HOOK* and *MODE-DISABLE-HOOK*. This option defaults to T.

@item
(:DEFINE-COMMAND-DEFINER (OR T NIL))@*
When :DEFINE-COMMAND-DEFINER is T a macro is defined for defining commands that
are active only when the minor mode is active. Commands defined with this macro
have the special variable *MINOR-MODE* bound to the minor mode object in their
body. The generated macro is called DEFINE-MODE-COMMAND. This option defaults to
T. 
@end itemize

Example:
@verbatim
(define-minor-mode evil-mode () ()
  (:scope :screen)
  (:top-map '((\"j\" . \"move-focus down\")
              (\"k\" . \"move-focus up\")
              (\"h\" . \"move-focus left\")
              (\"l\" . \"move-focus right\")
              (\"x\" . *exchange-window-map*)
              (\"C-m b\" . \"evil-mode\")))
  (:lighter \"EVIL\")
  (:lighter-make-clickabl nil))

(define-evil-mode-command evil-echo () ()
  (run-commands \"echo\"))
@end verbatim
"
  (when (null superclasses)
    (setq superclasses '(minor-mode)))
  (multiple-value-bind (mm-opts other-opts)
      (parse-minor-mode-options options)
    (destructuring-bind (&key top-map root-map (expose-keymaps t)
                           lighter lighter-make-clickable
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
                     (make-minor-mode-keymap ,root-map)
                     ,(format nil "The root map for ~A" mode)))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (defparameter ,(make-special-variable-name mode 'top-map)
                     (make-minor-mode-top-map
                      ,top-map
                      ',(make-special-variable-name mode 'root-map))
                     ,(format nil "The top map for ~A" mode)))))
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
                                  (make-minor-mode-keymap ',root-map))))
               :reader ,(intern (format nil "~A-keymap" mode))
               :allocation :class)
              ,@slots)
             (:default-initargs ,@default-initargs)
             ,@other-opts)
           (defmethod minor-mode-lighter ((,gmode ,mode))
             (cons
              ,(if lighter-make-clickable
                   `(format-with-on-click-id (funcall ,(genlighter mode lighter)
                                                      ,gmode)
                                             :ml-on-click-minor-mode
                                             ',mode)
                   `(funcall ,(genlighter mode lighter) ,gmode))
              (call-next-method)))
           (defmethod minor-mode-scope ((,gmode (eql ',mode)))
             (declare (ignore ,gmode))
             ,scope)
           (defmethod minor-mode-keymap ((,gmode ,mode))
             (cons (slot-value ,gmode ',gkeymap) (call-next-method)))
           ,@(define-enable-methods mode scope make-hooks on-enable on-disable)
           ,@(when interactive
               `((defcommand ,mode (&optional (yn nil ynpp)) ((:y-or-n))
                   (flet ((enable () (enable-minor-mode ',mode nil))
                          (disable () (disable-minor-mode ',mode nil)))
                     (cond (yn (enable))
                           (ynpp (disable))
                           ((minor-mode-enabled-p ',mode) (disable))
                           (t (enable)))))))
           ,@(when define-command-definer
               (list (define-command-macro mode))))))))

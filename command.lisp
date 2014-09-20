;; Copyright (C) 2003-2008 Shawn Betts
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; implementation of commands
;;
;; Code:

(in-package #:stumpwm)

(export '(argument-line-end-p
          argument-pop
          argument-pop-or-read
          argument-pop-rest
          define-stumpwm-command
          defcommand
          defcommand-alias
          define-stumpwm-type
          run-commands
          %interactivep%))

(defstruct command-alias
  from to)

(defstruct command
  name class args)

(defvar *command-hash* (make-hash-table :test 'eq)
  "A list of interactive stumpwm commands.")

(defvar *max-command-alias-depth* 10
  "")

;; XXX: I'd like to just use straight warn, but sbcl drops to the
;; debugger when compiling so i've made a style warning instead
;; -sabetts
(define-condition command-docstring-warning (style-warning)
  ((command :initarg :command))
  (:report
   (lambda (c s)
     (format s "command ~a doesn't have a docstring" (slot-value c 'command)))))

(defmacro defcommand (name (&rest args) (&rest interactive-args) &body body)
  "Create a command function and store its interactive hints in
*command-hash*. The local variable %interactivep% can be used to check
if the command was called interactively. If it is non-NIL then it was
called from a keybinding or from the colon command.

The NAME argument can be a string, or a list of two symbols. If the
latter, the first symbol names the command, and the second indicates
the type of group under which this command will be usable. Currently,
tile-group and floating-group are the two possible values.

INTERACTIVE-ARGS is a list of the following form: ((TYPE PROMPT) (TYPE PROMPT) ...)

each element in INTERACTIVE-ARGS declares the type and prompt for the
command's arguments.

TYPE can be one of the following:

@table @var
@item :y-or-n
A yes or no question returning T or NIL.
@item :variable
A lisp variable
@item :function
A lisp function
@item :command
A stumpwm command as a string.
@item :key-seq
A key sequence starting from *TOP-MAP*
@item :window-number
An existing window number
@item :number
An integer number
@item :string
A string
@item :key
A single key chord
@item :window-name
An existing window's name
@item :direction
A direction symbol. One of :UP :DOWN :LEFT :RIGHT
@item :gravity
A gravity symbol. One of :center :top :right :bottom :left :top-right :top-left :bottom-right :bottom-left
@item :group
An existing group
@item :frame
A frame
@item :shell
A shell command
@item :rest
The rest of the input yes to be parsed.
@item :module
An existing stumpwm module
@end table

Note that new argument types can be created with DEFINE-STUMPWM-TYPE.

PROMPT can be string. In this case, if the corresponding argument is
missing from an interactive call, stumpwm will use prompt for its
value using PROMPT. If PROMPT is missing or nil, then the argument is
considered an optional interactive argument and is not prompted for
when missing.

Alternatively, instead of specifying nil for PROMPT or leaving it
out, an element can just be the argument type."
  (check-type name (or symbol list))
  (let ((docstring (if (stringp (first body))
                     (first body)
                     (warn (make-condition 'command-docstring-warning :command name))))
        (body (if (stringp (first body))
                  (cdr body) body))
        (name (if (atom name)
                  name
                  (first name)))
        (group (if (atom name)
                   t
                   (second name))))
  `(progn
     (defun ,name ,args
       ,docstring
       (let ((%interactivep% *interactivep*)
	     (*interactivep* nil))
	 (declare (ignorable %interactivep%))
	 ,@body))
     (export ',name)
     (setf (gethash ',name *command-hash*)
           (make-command :name ',name
                         :class ',group
                         :args ',interactive-args)))))

(defmacro define-stumpwm-command (name (&rest args) &body body)
  "Deprecated. use `defcommand' instead."
  (check-type name string)
  (setf name (intern1 name))
  `(progn
     (defun ,name ,(mapcar 'car args) ,@body)
     (setf (gethash ',name *command-hash*)
           (make-command :name ',name
                         :args ',(mapcar 'rest args)))))

(defmacro defcommand-alias (alias original)
  "Since interactive commands are functions and can conflict with
package symbols. But for backwards compatibility this macro creates an
alias name for the command that is only accessible interactively."
  `(setf (gethash ',alias *command-hash*)
         (make-command-alias :from ',alias
                             :to ',original)))

(defun dereference-command-symbol (command)
  "Given a string or symbol look it up in the command database and return
whatever it finds: a command, an alias, or nil."
  (maphash (lambda (k v)
             (when (string-equal k command)
               (return-from dereference-command-symbol v)))
           *command-hash*))

(defun command-active-p (command)
  (typep (current-group) (command-class command))
  ;; TODO: minor modes
  )

(defun get-command-structure (command &optional (only-active t))
  "Return the command structure for COMMAND. COMMAND can be a string,
symbol, command, or command-alias. By default only search active
commands."
  (declare (type (or string symbol command command-alias) command))
  (when (or (stringp command) (symbolp command))
    (setf command (dereference-command-symbol command)))
  (when (command-alias-p command)
    (setf command (loop for c = (gethash (command-alias-to command) *command-hash*)
                     then (gethash (command-alias-to c) *command-hash*)
                     for depth from 1
                     until (or (null c)
                               (command-p c))
                     when (> depth *max-command-alias-depth*)
                     do (error "Maximum command alias depth exceded")
                     finally (return c))))
  (when (and command
             (or (not only-active)
                 (command-active-p command)))
    command))

(defun all-commands (&optional (only-active t))
  "Return a list of all interactive commands as strings. By default
only return active commands."
  (let (acc)
    (maphash (lambda (k v)
               ;; make sure its an active command
               (when (get-command-structure v only-active)
                 (push (string-downcase k) acc)))
             *command-hash*)
    (sort acc 'string<)))

;;; command arguments

(defstruct argument-line
  string start)

(defvar *command-type-hash* (make-hash-table)
  "A hash table of types and functions to deal with these types.")

(defun argument-line-end-p (input)
  "Return T if we're outta arguments from the input line."
  (>= (argument-line-start input)
      (length (argument-line-string input))))

(defun argument-pop (input)
  "Pop the next argument off."
  (unless (argument-line-end-p input)
    (let* ((p1 (position-if-not (lambda (ch)
                                  (char= ch #\Space))
                                (argument-line-string input)
                                :start (argument-line-start input)))
           (p2 (or (and p1 (position #\Space (argument-line-string input) :start p1))
                   (length (argument-line-string input)))))
      (prog1
          ;; we wanna return nil if they're the same
          (unless (= p1 p2)
            (subseq (argument-line-string input) p1 p2))
        (setf (argument-line-start input) (1+ p2))))))

(defun argument-pop-or-read (input prompt &optional completions)
  (or (argument-pop input)
      (if completions
          (completing-read (current-screen) prompt completions)
          (read-one-line (current-screen) prompt))
      (throw 'error :abort)))

(defun argument-pop-rest (input)
  "Return the remainder of the argument text."
  (unless (argument-line-end-p input)
    (prog1
        (subseq (argument-line-string input) (argument-line-start input))
      (setf (argument-line-start input) (length (argument-line-string input))))))

(defun argument-pop-rest-or-read (input prompt &optional completions)
  (or (argument-pop-rest input)
      (if completions
          (completing-read (current-screen) prompt completions)
          (read-one-line (current-screen) prompt))
      (throw 'error :abort)))

(defmacro define-stumpwm-type (type (input prompt) &body body)
  "Create a new type that can be used for command arguments. @var{type} can be any symbol. 

When @var{body} is evaluated @var{input} is bound to the
argument-line. It is passed to @code{argument-pop},
@code{argument-pop-rest}, etc. @var{prompt} is the prompt that should
be used when prompting the user for the argument.

@example
\(define-stumpwm-type :symbol (input prompt)
 (or (find-symbol (string-upcase
		     (or (argument-pop input)
                         ;; Whitespace messes up find-symbol.
		         (string-trim \" \"
		           (completing-read (current-screen)
					  prompt
					  ;; find all symbols in the
					  ;;  stumpwm package.
					  (let (acc)
					    (do-symbols (s (find-package \"STUMPWM\"))
					      (push (string-downcase (symbol-name s)) acc))
					    acc)))
                      (throw 'error \"Abort.\")))
                  \"STUMPWM\")
     (throw 'error \"Symbol not in STUMPWM package\")))

\(defcommand \"symbol\" (sym) ((:symbol \"Pick a symbol: \"))
  (message \"~a\" (with-output-to-string (s)
	          (describe sym s))))
@end example

This code creates a new type called @code{:symbol} which finds the
symbol in the stumpwm package. The command @code{symbol} uses it and
then describes the symbol."
  `(setf (gethash ,type *command-type-hash*)
    (lambda (,input ,prompt)
      ,@body)))

(define-stumpwm-type :y-or-n (input prompt)
  (let ((s (or (argument-pop input)
               (read-one-line (current-screen) (concat prompt "(y/n): ")))))
    (when s
      (values (list (equal s "y"))))))

(defun lookup-symbol (string)
  ;; FIXME: should we really use string-upcase?
  (let* ((ofs (split-string string ":"))
         (pkg (if (> (length ofs) 1)
                  (find-package (string-upcase (pop ofs)))
                  *package*))
         (var (string-upcase (pop ofs)))
         (ret (find-symbol var pkg)))
    (when (plusp (length ofs))
      (throw 'error "Too many :'s"))
    (if ret
        (values ret pkg var)
        (throw 'error (format nil "No such symbol: ~a::~a."
                              (package-name pkg) var)))))

(define-stumpwm-type :variable (input prompt)
  (lookup-symbol (argument-pop-or-read input prompt)))

(define-stumpwm-type :function (input prompt)
  (multiple-value-bind (sym pkg var)
      (lookup-symbol (argument-pop-or-read input prompt))
    (if (symbol-function sym)
        (symbol-function sym)
        (throw 'error (format nil "the symbol ~a::~a has no function."
                              (package-name pkg) var)))))

(define-stumpwm-type :command (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen)
                       prompt
                       (all-commands))))

(define-stumpwm-type :key-seq (input prompt)
  (labels ((update (seq)
             (message "~a: ~{~a ~}"
                      prompt
                      (mapcar 'print-key (reverse seq)))))
    (let ((rest (argument-pop-rest input)))
      (or (and rest (parse-key-seq rest))
          ;; read a key sequence from the user
          (with-focus (screen-key-window (current-screen))
            (message "~a" prompt)
            (nreverse (second (multiple-value-list
                               (read-from-keymap (top-maps) #'update)))))))))

(define-stumpwm-type :window-number (input prompt)
  (let ((n (or (argument-pop input)
               (completing-read (current-screen)
                                prompt
                                (mapcar 'window-map-number
                                        (group-windows (current-group)))))))
    (when n
      (let ((win (find n (group-windows (current-group))
                       :test #'string=
                       :key #'window-map-number)))
        (if win
            (window-number win)
            (throw 'error "No Such Window."))))))

(define-stumpwm-type :number (input prompt)
  (let ((n (or (argument-pop input)
               (read-one-line (current-screen) prompt))))
    (when n
      (handler-case
          (parse-integer n)
        (parse-error (c)
          (declare (ignore c))
          (throw 'error "Number required."))))))


(define-stumpwm-type :string (input prompt)
  (or (argument-pop input)
      (read-one-line (current-screen) prompt)))

(define-stumpwm-type :password (input prompt)
  (or (argument-pop input)
      (read-one-line (current-screen) prompt :password t)))

(define-stumpwm-type :key (input prompt)
  (let ((s (or (argument-pop input)
               (read-one-line (current-screen) prompt))))
    (when s
      (kbd s))))

(define-stumpwm-type :window-name (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen) prompt
                       (mapcar 'window-name
                               (group-windows (current-group))))))

(define-stumpwm-type :direction (input prompt)
  (let* ((values '(("up" :up)
                   ("down" :down)
                   ("left" :left)
                   ("right" :right)))
         (dir (second (assoc (argument-pop-or-read input prompt values)
                             values :test 'string-equal))))
    (or dir
        (throw 'error "No matching direction."))))

(define-stumpwm-type :gravity (input prompt)
"Set the current window's gravity."
  (let* ((values '(("center" :center)
                   ("top" :top)
                   ("right" :right)
                   ("bottom" :bottom)
                   ("left" :left)
                   ("top-right" :top-right)
                   ("top-left" :top-left)
                   ("bottom-right" :bottom-right)
                   ("bottom-left" :bottom-left)))
         (gravity (second (assoc (argument-pop-or-read input prompt values) values :test 'string-equal))))
    (or gravity
        (throw 'error "No matching gravity."))))

(defun select-group (screen query)
  "Attempt to match string QUERY against group number or partial name."
  (labels ((match-num (grp)
             (string-equal (group-map-number grp) query))
           (match-whole (grp)
             (string-equal (group-name grp) query))
           (match-partial (grp)
             (let* ((end (min (length (group-name grp)) (length query))))
               (string-equal (group-name grp) query :end1 end :end2 end))))
    (when query
      (or (find-if #'match-num (screen-groups screen))
          (find-if #'match-whole (screen-groups screen))
          (find-if #'match-partial (screen-groups screen))))))

(define-stumpwm-type :group (input prompt)
  (let ((match (select-group (current-screen)
                             (or (argument-pop input)
                                 (completing-read (current-screen) prompt
                                                  (mapcar 'group-name
                                                          (screen-groups (current-screen))))))))
    (or match
        (throw 'error "No Such Group."))))

(define-stumpwm-type :frame (input prompt)
  (declare (ignore prompt))
  (let ((arg (argument-pop input)))
    (if arg
        (or (find arg (group-frames (current-group))
                  :key (lambda (f)
                         (string (get-frame-number-translation f)))
                  :test 'string=)
            (throw 'error "Frame not found."))
        (or (choose-frame-by-number (current-group))
            (throw 'error :abort)))))

(define-stumpwm-type :shell (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt 'complete-program)))

(define-stumpwm-type :rest (input prompt)
  (or (argument-pop-rest input)
      (read-one-line (current-screen) prompt)))

;;;

(defun call-interactively (command &optional (input ""))
  "Parse the command's arguments from input given the command's
argument specifications then execute it. Returns a string or nil if
user aborted."
  (declare (type (or string symbol) command)
           (type (or string argument-line) input))
  ;; Catch parse errors
  (catch 'error
    (let* ((arg-line (if (stringp input)
                         (make-argument-line :string input
                                             :start 0)
                         input))
           (cmd-data (or (get-command-structure command)
                         (throw 'error (format nil "Command '~a' not found." command))))
           (arg-specs (command-args cmd-data))
           (args (loop for spec in arg-specs
                    collect (let* ((type (if (listp spec)
                                             (first spec)
                                             spec))
                                   (prompt (when (listp spec)
                                             (second spec)))
                                   (fn (gethash type *command-type-hash*)))
                              (unless fn
                                (throw 'error (format nil "Bad argument type: ~s" type)))
                              ;; If the prompt is NIL then it's
                              ;; considered an optional argument and
                              ;; we shouldn't prompt for it if the
                              ;; arg line is empty.
                              (if (and (null prompt)
                                       (argument-line-end-p arg-line))
                                  (loop-finish)
                                  (funcall fn arg-line prompt))))))
      ;; Did the whole string get parsed?
      (unless (or (argument-line-end-p arg-line)
                  (position-if 'alphanumericp (argument-line-string arg-line) :start (argument-line-start arg-line)))
        (throw 'error (format nil "Trailing garbage: ~{~A~^ ~}" (subseq (argument-line-string arg-line)
                                                                        (argument-line-start arg-line)))))
      ;; Success
      (prog1
          (apply (command-name cmd-data) args)
        (setf *last-command* command)))))

(defun eval-command (cmd &optional interactivep)
  "exec cmd and echo the result."
  (labels ((parse-and-run-command (input)
             (let* ((arg-line (make-argument-line :string input
                                                  :start 0))
                    (cmd (argument-pop arg-line)))
               (let ((*interactivep* interactivep))
		 (call-interactively cmd arg-line)))))
    (multiple-value-bind (result error-p)
        ;; this fancy footwork lets us grab the backtrace from where the
        ;; error actually happened.
        (restart-case
            (handler-bind 
                ((error (lambda (c)
                          (invoke-restart 'eval-command-error
                                          (format nil "^B^1*Error In Command '^b~a^B': ^n~A~a" 
                                                  cmd c (if *show-command-backtrace* 
                                                            (backtrace-string) ""))))))
              (parse-and-run-command cmd))
          (eval-command-error (err-text)
            :interactive (lambda () nil)
            (values err-text t)))
      ;; interactive commands update the modeline
      (update-all-mode-lines)
      (cond ((stringp result)
             (if error-p
                 (message-no-timeout "~a" result)
                 (message "~a" result)))
            ((eq result :abort)
             (unless *suppress-abort-messages*
               (message "Abort.")))))))

(defun run-commands (&rest commands)
  "Run each stumpwm command in sequence. This could be used if you're
used to ratpoison's rc file and you just want to run commands or don't
know lisp very well. One might put the following in one's rc file:

@example
\(stumpwm:run-commands
  \"escape C-z\"
  \"exec firefox\"
  \"split\")
@end example"
  (loop for i in commands do
        (eval-command i)))

(defcommand colon (&optional initial-input) (:rest)
  "Read a command from the user. @var{initial-text} is optional. When
supplied, the text will appear in the prompt."
  (let ((cmd (completing-read (current-screen) ": " (all-commands) :initial-input (or initial-input ""))))
    (unless cmd
      (throw 'error :abort))
    (when (plusp (length cmd))
      (eval-command cmd t))))

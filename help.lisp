;; Copyright (C) 2008 Shawn Betts
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
;; Help and introspection commands
;;
;; Code:

(in-package #:stumpwm)

(export '(*help-max-height* *message-max-width*))
(defvar *message-max-width* 80
  "The maximum width of a message before it wraps.")
(defvar *help-max-height* 10
  "Maximum number of lines for help to display.")

(defun columnize (list columns &key col-aligns (pad 1) (char #\Space) (align :left))
  ;; only somewhat nasty
  (let* ((rows (ceiling (length list) columns))
         (data (loop for i from 0 below (length list) by (max rows 1)
                  collect (subseq list i (min (+ i rows) (length list)))))
         (max (mapcar (lambda (col)
                        (reduce 'max col :key 'length :initial-value 0))
                      data))
         (padstr (make-string pad :initial-element char))
         (cols ;; normalize width
          (loop
             for i in data
             for j in max
             for c from 0
             collect (loop
                        for k from 0 below rows
                        for s = (or (nth k i) "")
                        for len = (make-string (- j (length s))
                                               :initial-element char)
                        collect (ecase (or (nth c col-aligns) align)
                                  (:left (format nil "~a~a~a" (if (= c 0) "" padstr) s len))
                                  (:right (format nil "~a~a~a" (if (= c 0) "" padstr) len s)))))))
    (apply 'mapcar 'concat (or cols '(nil)))))

(defun display-bindings-for-keymaps (key-seq &rest keymaps)
  (let* ((screen (current-screen))
         (data (mapcan (lambda (map)
                         (mapcar (lambda (b) (format nil "^5*~5a^n ~a" (print-key (binding-key b)) (binding-command b))) (kmap-bindings map)))
                       keymaps))
         (cols (ceiling (1+ (length data))
                        (truncate (- (head-height (current-head)) (* 2 (screen-msg-border-width screen)))
                                  (font-height (screen-font screen))))))
    (message-no-timeout "Prefix: ~a~%~{~a~^~%~}"
                        (print-key-seq key-seq)
                        (or (columnize data cols) '("(EMPTY MAP)")))))

(defcommand commands () ()
"List all available commands."
  (let* ((screen (current-screen))
         (data (all-commands))
         (cols (ceiling (length data)
                        (truncate (- (head-height (current-head)) (* 2 (screen-msg-border-width screen)))
                                  (font-height (screen-font screen))))))
    (message-no-timeout "~{~a~^~%~}"
                        (columnize data cols))))

(defun wrap (words &optional (max-col *message-max-width*) stream)
  "Word wrap at the MAX-COL."
  ;; Format insanity edited from Gene Michael Stover's "Advanced Use of Lisp's
  ;; FORMAT Function (2004)"

  ;; Note that using format without a constant format string is not very
  ;; efficient. Not doing so comes at the cost of *message-max-width* being
  ;; available at compile time, so users would not be able to configure it at
  ;; runtime.
  (format stream (concatenate 'string "~{~<~%~1,"
                              (with-output-to-string (s) (princ max-col s) s)
                              ":;~A~> ~}")
          (split-string words " ")))

(defun final-key-p (keys class)
  "Determine if the key is a memeber of a class"
  (member (lastcar keys) (mapcar #'parse-key class) :test #'equalp))

(defun help-key-p (keys)
  "If the key is for the help command."
  (final-key-p keys '("?" "C-h")))

(defun cancel-key-p (keys)
  "If a key is the cancelling key binding."
  (final-key-p keys '("C-g")))

(defcommand describe-key (keys) ((:key-seq "Describe Key:"))
  "Either interactively type the key sequence or supply it as text. This
  command prints the command bound to the specified key sequence."
  (if-let ((cmd (loop for map in (top-maps)
                      for cmd = (lookup-key-sequence map keys)
                      when cmd return cmd))
           (printed-key (mapcar 'print-key keys)))
    (let ((cmd-without-args (argument-pop
                              (make-argument-line :string cmd :start 0))))
      (message-no-timeout "~{~A~^ ~} is bound to \"~A\".~%~A"
                          printed-key cmd
                          (describe-command-to-stream cmd-without-args nil)))
    (cond ((and (help-key-p keys)
                (cdr printed-key))
           (message "~{~A~^ ~} shows the bindings for the prefix map under ~{~A~^ ~}."
                    printed-key (butlast printed-key)))
          ((cancel-key-p keys)
           (message "Any command ending in ~A is meant to cancel any command in progress \"ABORT\".~%"
                    (lastcar printed-key)))
          (t (message "~{~A~^ ~} is not bound." printed-key)))))

(defun describe-variable-to-stream (var stream)
  "Write the help for the variable to the stream."
  (format stream "variable:^5 ~a^n~%~a~%Its value is:~%~a."
          var
          (or (documentation var 'variable) "")
          (let* ((value (format nil "~a" (symbol-value var)))
                 (split (split-string value (format nil "~%"))))
            (if (> (1+ *help-max-height*)
                   (length split))
                value
                (format nil "~a.."
                        (wrap (format nil "~{~a~^~%~}"
                                      (take *help-max-height* split))))))))

(defcommand describe-variable (var) ((:variable "Describe Variable: "))
"Print the online help associated with the specified variable."
  (message-no-timeout "~a"
                      (with-output-to-string (s)
                        (describe-variable-to-stream var s))))

(defun describe-function-to-stream (fn stream)
  "Write the help for the function to the stream."
  (format stream "function:^5 ~a^n~%" (string-downcase (symbol-name fn)))
  (when-let ((lambda-list (sb-introspect:function-lambda-list
                           (symbol-function fn))))
    (format stream "(^5~a ^B~{~a~^ ~}^b^n)~&~%" (string-downcase (symbol-name fn)) lambda-list))
  (format stream "~&~a"(or (documentation fn 'function) "")))

(defcommand describe-function (fn) ((:function "Describe Function: "))
"Print the online help associated with the specified function."
  (message-no-timeout "~a"
                      (with-output-to-string (s)
                        (describe-function-to-stream fn s))))

(defun describe-command-to-stream (com stream)
  "Write the help for the command to the stream."
  (let* ((deref (dereference-command-symbol com))
         (struct (get-command-structure com nil))
         (name (command-name struct)))
    (wrap (concat
           (unless (eq deref struct)
             (format nil "\"~a\" is an alias for the command \"~a\":~%"
                     (command-alias-from deref)
                     name))
           (when-let ((message (where-is-to-stream name nil)))
             (format nil "~&~A~&" message))
           (when-let ((lambda-list (sb-introspect:function-lambda-list
                                  (symbol-function name))))
             (format nil "~%^5~a ^B~{~a~^ ~}^b^n~&~%"
                     name
                     lambda-list))
           (format nil "~&~a" (or (documentation name 'function) "")))
          *message-max-width*
          stream)))

(defcommand describe-command (com) ((:command "Describe Command: "))
  "Print the online help associated with the specified command."
  (if (null (get-command-structure com nil))
      (message-no-timeout "Error: Command \"~a\" not found."
                          (command-name com))
      (message-no-timeout "~a" (describe-command-to-stream com nil))))

(defun where-is-to-stream (cmd stream)
  (labels ((keys (cmd)
             (loop for map in (top-maps) append (search-kmap cmd map)))
           (sym (comm alias-accessor)
             (typecase comm
               (command-alias (sym (funcall alias-accessor comm) alias-accessor))
               (command (command-name comm))
               (string (intern (string-upcase comm)))
               (symbol comm))))
    (let ((cmd (string-downcase cmd)))
     (if-let ((bindings (keys cmd)))
       (format stream "\"~a\" is on ~{~a~^, ~}." cmd
               (mapcar 'print-key-seq bindings))
       (format stream "Command \"~a\" is not currently bound." cmd))
     (let ((reverse-hash (make-hash-table :size (hash-table-size *command-hash*)
                                          :test 'eq)))
       (loop for k being each hash-key of *command-hash* using (hash-value v)
             do (setf #1=(gethash (sym v #'command-alias-to) reverse-hash)
                      (let ((sym (sym v #'command-alias-from)))
                        (when (not (eql sym (sym v #'command-alias-to)))
                          (cons sym #1#)))))
       (when-let ((aliases (gethash (intern (string-upcase cmd)) reverse-hash)))
         (format stream "~%\"~a\" is aliased to ~{\"~a\"~^, ~}."
                 cmd (mapcar #'string-downcase aliases))
         (loop for a in aliases
               for k = #2=(keys (string-downcase (symbol-name a))) then #2#
               when k do (format stream "~%\"~a\" is on ~{~a~^, ~}." (string-downcase a) (mapcar 'print-key-seq k))))))))

(defcommand where-is (cmd) ((:command "Where is command: "))
  "Print the key sequences bound to the specified command."
  (let ((stream (make-string-output-stream)))
    (where-is-to-stream cmd stream)
    (message-no-timeout "~A" (get-output-stream-string stream))))

(defun get-kmaps-at-key (kmaps key)
  (dereference-kmaps
   (reduce
    (lambda (result map)
      (let* ((binding (handler-case (find key (kmap-bindings map)
                                          :key 'binding-key :test 'equalp)
                        (type-error () nil)))
             (command (when binding (binding-command binding))))
        (if command
            (setf result (cons command result))
            result)))
    kmaps
    :initial-value ())))

(defun get-kmaps-at-key-seq (kmaps key-seq)
  "get a list of kmaps that are activated when pressing KEY-SEQ when
KMAPS are enabled"
  (if (= 1 (length key-seq))
      (get-kmaps-at-key kmaps (first key-seq))
      (get-kmaps-at-key-seq (get-kmaps-at-key kmaps (first key-seq))
                            (rest key-seq))))

(defun which-key-mode-key-press-hook (key key-seq cmd)
  "*key-press-hook* for which-key-mode"
  (declare (ignore key cmd))
  (when (not (eq *top-map* *resize-map*))
    (let* ((oriented-key-seq (reverse key-seq))
           (maps (get-kmaps-at-key-seq (dereference-kmaps (top-maps)) oriented-key-seq)))
      (when-let ((only-maps (remove-if-not 'kmap-p maps)))
        (apply 'display-bindings-for-keymaps oriented-key-seq only-maps)))))

(defcommand which-key-mode () ()
  "Toggle which-key-mode"
  (if (find 'which-key-mode-key-press-hook *key-press-hook*)
      (remove-hook *key-press-hook* 'which-key-mode-key-press-hook)
      (add-hook *key-press-hook* 'which-key-mode-key-press-hook)))

(defcommand modifiers () ()
  "List the modifiers stumpwm recognizes and what MOD-X it thinks they're on."
  (message "~@{~5@a: ~{~(~a~)~^ ~}~%~}"
           "Meta" (modifiers-meta *modifiers*)
           "Alt" (modifiers-alt *modifiers*)
           "Super" (modifiers-super *modifiers*)
           "Hyper" (modifiers-hyper *modifiers*)
           "AltGr" (modifiers-altgr *modifiers*)))

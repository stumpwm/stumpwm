;;; Aumix front end module for stumpwm
;;;
;;; Copyright (C) 2008 Fredrik Tolf
;;;
;;; Maintainer: Fredrik Tolf
;;;
;;; This module is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This module is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;

;;; USAGE:
;;;
;;; Put:
;;;
;;;     (load-module "aumix")
;;;
;;; In your ~/.stumpwmrc

(in-package :stumpwm-user)

(defvar *aumix-program* "/usr/bin/aumix")

(defvar *aumix-channels*
  '((:pcm . "w")
    (:master . "v")
    (:alt-pcm . "W")
    (:line . "l")))

(defun assert-ret (val)
  (assert val)
  val)

(defun process-aumix-volstring (output)
  (do* ((i 0 (+ i 1))
        (ch (aref output i) (aref output i))
        (st 'ch)
        (buf "")
        left right)
       (nil)
    (setq st (case st
               ((ch) (if (eql ch #\space) 'left 'ch))
               ((left) (if (digit-char-p ch)
                           (progn (setq buf (concatenate 'string buf (string ch))) 'left)
                           (progn (setq left (/ (parse-integer buf) 100)) 'space)))
               ((space) (if (eql ch #\space) (progn (setq buf "") 'right) (error "Invalid output from aumix")))
               ((right) (if (digit-char-p ch)
                            (progn (setq buf (concatenate 'string buf (string ch))) 'right)
                            (progn (setq right (/ (parse-integer buf) 100))
                                   (return (values (/ (+ left right) 2) left right)))))
               (t (error "Invalid output from aumix"))))))

(defun aumix-call (channel op amount)
  (let* ((ch (assert-ret (cdr (assoc channel *aumix-channels*))))
         (opstr (concat
                 (case op
                   ((:up) "+")
                   ((:down) "-")
                   ((:set) "")
                   (t (error "Unknown volume operation")))
                 (format nil "~D" (round (* 100 amount)))))
         (output (stumpwm::run-prog-collect-output
                  *aumix-program*
                  (concat "-" ch opstr)
                  (concat "-" ch "q"))))
    (process-aumix-volstring output)))

(defun aumix-get (channel)
  (process-aumix-volstring
   (stumpwm::run-prog-collect-output
    *aumix-program*
    (concat "-" (assert-ret (cdr (assoc channel *aumix-channels*))) "q"))))

(define-stumpwm-type :mixer-channel (input prompt)
  (let ((n (or (argument-pop input)
               (completing-read (current-screen) prompt (mapcar (lambda (sym)
                                                                  (string-downcase (symbol-name (car sym))))
                                                                *aumix-channels*)))))
    (intern (string-upcase n) 'keyword)))

(defcommand mixer (channel opstr) ((:mixer-channel "Channel: ") (:rest "Op: "))
  "Change mixer channel."
  (let* ((fc (aref opstr 0))
         (op (cond ((eql fc #\+) (setq opstr (subseq opstr 1)) :up)
                   ((eql fc #\-) (setq opstr (subseq opstr 1)) :down)
                   ((eql fc #\=) (setq opstr (subseq opstr 1)) :set)
                   ((digit-char-p fc) :set)
                   (t (error "Illegal mixer operation"))))
         (amount (parse-integer opstr)))
    (message "~A: ~D%" (symbol-name channel) (round (* (aumix-call channel op (/ amount 100)) 100)))))

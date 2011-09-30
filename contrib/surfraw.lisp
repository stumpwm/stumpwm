;; SURFRAW module for StumpWM.
;;
;; Copyright (C) 2008 Ivy Foster
;; Copyright (C) 2010 Ivy Foster, Friedrich Delgado
;;
;; Maintainer: Ivy Foster
;;
;; This module is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This module is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; I like surfraw (http://surfraw.alioth.debian.org). If you're
;; reading this, you probably like surfraw. I've got surfraw-related
;; code in my .stumpwmrc, and (judging from some judicious googling
;; for RC files early on in my use of stumpwm) I know that I'm not the
;; only one. So it seemed like a good idea to just put that code in
;; a library.

;;; Usage:
;;
;; Just add the following line to your .stumpwmrc file:
;;
;; (load "/path/to/stumpwm/contrib/surfraw.lisp")
;;
;; ...and then either call the functions here with "colon" (C-t ;) or
;; bind them to a key. I figure other people will probably have
;; different key preferences than I have, so I leave them entirely up
;; to you.
;;
;; If you want to use the bookmark functions, don't forget to tell
;; stumpwm where your *surfraw-bookmark-file* is.
;;
;; Note that there are also "surfraw-selection" variants on each
;; command that work on the X selection.

;;; Code:

(defun split-by-- (str)
  (let ((pos (position #\- str :start (1+ (position #\- str)))))
    (list (subseq str 0 (1- pos))
          (subseq str (1+ pos)))))

(defun surfraw-elvis-list ()
  (mapcar (lambda (x)
            (mapcar (lambda (x) (string-trim '(#\Space #\Tab #\Newline) x))
                    (split-by-- x)))
          (cdr (split-string (run-shell-command "surfraw -elvi" :collect-output-p)
                             '(#\Newline)))))

(defmacro auto-define-surfraw-commands-from-elvis-list ()
  (let ((commands nil))
    (dolist (elvi (surfraw-elvis-list))
      (let ((key (first elvi))
            (description (second elvi)))
        (push `(defcommand ,(intern (concat "sr-" key)) (search)
                 ((:string ,(concat description ": ")))
                 ,description
                 (surfraw ,key search))
              commands)
        (push `(defcommand ,(intern (concat "sr-sel-" key)) () ()
                 (surfraw ,key (get-x-selection)))
              commands)))
    (cons 'progn (reverse commands))))
(auto-define-surfraw-commands-from-elvis-list)
;;; Regular surfraw commands

(defcommand surfraw (engine search)
  ((:string "What engine? ") (:string "Search for what? "))
  "Use SURFRAW to surf the net; reclaim heathen lands."
  (check-type engine string)
  (check-type search string)
  (run-shell-command (concat "exec surfraw -g " engine " " search)))

;;; Bookmarks

(defun display-file (file)
  "Display a file in the message area."
  (if (probe-file file)
      (run-shell-command (concat "cat " file) t)
    (message "The file ~a does not exist." file)))

(defvar *surfraw-bookmark-file* nil
  "The surfraw bookmark file")

(defcommand sr-bookmark (bmk) ((:string "Bookmark: "))
  (surfraw "" bmk))

(defcommand sr-bookmark-file-display () ()
  (display-file *surfraw-bookmark-file*))

;;; surfraw.lisp ends here

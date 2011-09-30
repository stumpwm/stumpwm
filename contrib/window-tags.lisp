;; Copyright 2009 Michael Raskin
;;
;; Maintainer: Michael Raskin
;;
;; This file is part of stumpwm.
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

(in-package :stumpwm)

; Window tags. Window tags are special window properties (stored in X11 window properties)
; that can be used for window manipulations. They can survive temporary WM change and allow
; more flexible classification of windows than selecting window groups for them.

; String parsing for commands

(defun string-split-by-spaces (x)
  (if (not x) nil 
    (if (listp x) (mapcar 'string-upcase x)
      (cl-ppcre:split " " (string-upcase x)))))

; Basic operations

(defcommand window-tags (&optional (argwin nil)) ()
	    "Show window tags"
  	(let* ((win (or argwin (current-window)))
	       (tags (xlib:get-property (window-xwin win) :STUMPWM_TAGS))
	       (tagstring (utf8-to-string tags))
	       (taglist 
		 (if tags (string-split-by-spaces tagstring) nil)))
	  (if argwin taglist (message "Tags: ~{~%~a~}" taglist))))

(defun (setf window-tags) (newtags &optional (argwin nil))
  "Set the window tag set for a window"
  	(let*
	  ((win (or argwin (current-window)))
	   (tagstring (format nil "~{~a ~}" (mapcar 'string-upcase newtags))))
	  (xlib:change-property (window-xwin win)
				:STUMPWM_TAGS
				(string-to-utf8 tagstring)
				:UTF8_STRING 8)))

(defun clear-tags-if (clearp &optional (argwin nil))
  "Remove tags matched by predicate"
  (let*
    ((win (or argwin (current-window)))
     (new-tags (remove-if clearp (window-tags win))))
    (setf (window-tags win) new-tags)))

; Commands for basic operations

(defcommand clear-tags (&optional (argtags nil) (argwin nil)) (:rest :rest)
	    "Remove specified or all tags"
	    (let*
	      ((tags (string-split-by-spaces argtags))
	       (condition (if tags (lambda(x) (find x tags :test 'equalp)) (lambda (x) t)))) 
	      (clear-tags-if condition argwin)))

(defcommand clear-all-tags () ()
	    "Remove all tags and start afresh"
	    (mapcar (lambda(x) (clear-tags nil x)) (screen-windows (current-screen))))

(defcommand tag-window (argtag &optional (argwin nil)) ((:rest "Tag to set: ") :rest)
	    "Add a tag to current window"
	    (let*
	      ((win (or argwin (current-window)))
	       (tag (string-split-by-spaces argtag)))
	      (setf (window-tags win) (union tag (window-tags win) :test 'equalp))))

(defcommand all-tags () ()
	    "List all windows with their tags"
	    (let ((*suppress-echo-timeout* t))
	      (message 
		"Window list: ~{~%~{[ ~a ] ( ~a | ~a | ~a ) ~% ->~{~a, ~}~}~}"
		(mapcar
		  (lambda(x)
		    (list
		      (window-title x)
		      (window-class x)
		      (window-res x)
		      (window-role x)
		      (window-tags x)))
		  (screen-windows (current-screen))))))

; Selection of tags and windows by tags

(defun tags-from (argtags &optional (argwindow nil))
  "Check whether (current) window has one of the specified tags.
  Tag T is implicitly assigned to all windows."
  (let*
    ((tags (string-split-by-spaces argtags))
     (window (or argwindow (current-window)))
     (wtags (union (list "T") (window-tags window) :test 'equalp)))
    (intersection tags wtags :test 'equalp)))

(defun select-by-tags (argtags &optional (without nil))
  "Select windows with (without) one of the specified tags 
  (any of the specified tags) from current screen. Tag T
  is implicitly assigned to every window"
  (let*
    ((tags (string-split-by-spaces argtags))
     (condition (lambda(w) (tags-from tags w)))
     (windows (screen-windows (current-screen))))
    (if without 
      (remove-if condition windows)
      (remove-if-not condition windows))))

; Window manipulations using tags

; General function

(defun move-windows-to-group (windows &optional (arggroup nil))
  "Move all windows from the list to the group"
  (let*
    ((group 
       (if (stringp arggroup) 
	 (or 
	   (find-group (current-screen) arggroup) 
	   (add-group (current-screen) arggroup))
	 (or arggroup (current-group)))))
    (mapcar (lambda (w) (move-window-to-group w group)) windows)))

; And convenient instances

(defcommand pull-tag (argtag) ((:rest "Tag(s) to pull: "))
	    "Pull all windows with the tag (any of the tags) to current group"
	    (move-windows-to-group (select-by-tags (string-split-by-spaces argtag))))

(defcommand push-without-tag (argtag) ((:rest "Tag(s) needed to stay in the group: "))
	    "Push windows not having the tag (any of the tags) to .tag-store"
	    (move-windows-to-group (select-by-tags (string-split-by-spaces argtag) T) ".tag-store"))

(defcommand push-tag (argtag) ((:rest "Tag(s) to push: "))
	    "Push windows having the tag (any of the tags) to .tag-store"
	    (move-windows-to-group (select-by-tags (string-split-by-spaces argtag)) ".tag-store"))

(defcommand pull+push (argtag) ((:rest "Tag(s) to select: "))
	    "Pull all windows with the tag, push all without"
	    (pull-tag argtag)
	    (push-without-tag argtag))

(defcommand push-window () ()
	    "Push window to tag store"
	    (move-windows-to-group (list (current-window)) ".tag-store"))

; Manage window numbers by tags..

(defun window-number-from-tag (window)
  "Find a numeric tag, if any, and parse it"
  (let*
    ((tags (window-tags window))
     (numtag (find-if (lambda(x) (cl-ppcre:scan "^[0-9]+$" x)) tags))
     (num (and numtag (parse-integer numtag))))
    num))

(defcommand number-by-tags () ()
	    "Every window tagged <number> will have a chance to have that number. 
	    The remaining windows will have packed numbers"

	    ; First, assign impossible numbers.
	    (mapcar
	      (lambda(x)
		(setf (window-number x) -1))
	      (group-windows (current-group)))
	    ; Now try to assign numbers to windows holding corresponding tags.
	    (mapcar
	      (lambda (x) 
		(let* 
		  ((num (window-number-from-tag x))
		   (occupied (mapcar 'window-number (group-windows (current-group)))))
		  (if (and num (not (find num occupied)))
		    (setf (window-number x) num))))
	      (group-windows (current-group)))
	    ; Give up and give smallest numbers possible
	    (repack-window-numbers 
	      (mapcar 'window-number
		      (remove-if-not 
			(lambda(x) (equalp (window-number x) (window-number-from-tag x)))
			(group-windows (current-group))))))

(defcommand tag-visible (&optional (argtags nil)) (:rest)
	    "IN-CURRENT-GROUP or another specified tag will be assigned to all windows 
	    in current group and only to them"
	    (let* 
	      (
	       (tags (if (or (equalp argtags "") (not argtags)) "IN-CURRENT-GROUP" argtags)))
	      (mapcar (lambda (x) (clear-tags tags x)) (screen-windows (current-screen)))
	      (mapcar (lambda (x) (tag-window tags x)) (group-windows (current-group)))))

(defcommand raise-tag (tag) ((:rest "Tag to pull: "))
	    "Make window current by tag"
	    (let*
	      ((window (car (select-by-tags tag))))
	      (if window
		(progn
		  (move-window-to-group window (current-group))
		  (really-raise-window window)
		  window)
		nil)))

(defcommand search-tag (tag-regex) ((:rest "Tag regex to select: "))
  (only)
  (fclear)
  (let*
      (
       (current (current-group (current-screen)))
       (tag-store (find-group (current-screen) ".tag-store")))
    (loop for w in (screen-windows (current-screen)) do
         (if
          (find-if (lambda (s) (cl-ppcre:scan (concatenate 'string "(?i)" tag-regex) s)) (window-tags w))
          (move-window-to-group w current)
          (move-window-to-group w tag-store)))))

(defcommand search-tag-pull (tag-regex) ((:rest "Tag regex to pull: "))
  (only)
  (fclear)
  (let*
      (
       (current (current-group (current-screen)))
       (tag-store (find-group (current-screen) ".tag-store")))
    (loop for w in (screen-windows (current-screen)) do
         (if
          (find-if (lambda (s) (cl-ppcre:scan (concatenate 'string "(?i)" tag-regex) s)) (window-tags w))
          (move-window-to-group w current)
          (progn)))))

(defcommand select-by-title-regexp (regex) ((:rest "Title regex to select: "))
  (only)
  (fclear)
  (let*
      (
       (current (current-group (current-screen)))
       (tag-store (find-group (current-screen) ".tag-store")))
    (loop for w in (screen-windows (current-screen)) do
         (if (cl-ppcre:scan regex (window-title w))
             (move-window-to-group w current)
             (move-window-to-group w tag-store)))))

(defcommand pull-by-title-regexp (regex) ((:rest "Title regex to select: "))
  (only)
  (fclear)
  (let*
      (
       (current (current-group (current-screen)))
       (tag-store (find-group (current-screen) ".tag-store")))
    (loop for w in (screen-windows (current-screen)) do
         (if (cl-ppcre:scan regex (window-title w))
             (move-window-to-group w current)
             (progn)))))

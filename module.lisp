;; Copyright (C) 2008 Julian Stecklina, Shawn Betts, Ivy Foster
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
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; Use `set-contrib-dir' to set the location stumpwm searches for modules.

;; Code:

#+ ecl
(eval-when (load compile eval)
  (require :asdf))

(in-package #:stumpwm)

(export '(load-module
          list-modules
	  *contrib-dir*
	  set-contrib-dir
          find-module))

;; Handy functions from the unClog blog
;; http://metabang.com/unclogit/?p=98

;; FIXME: These functions are part of the current version of asdf so
;; at some point we should remove them when there's no chance anyone
;; is running an older version.
(defun asdf-system-source-directory (system-name)
  (make-pathname :name nil
                 :type nil
                 :defaults (asdf-system-source-file system-name)))

(defun asdf-system-source-file (system-name)
  (let ((system (asdf:find-system system-name)))
    (make-pathname 
     :type "asd"
     :name (asdf:component-name system)
     :defaults (asdf:component-relative-pathname system))))

(defun system-relative-pathname (system pathname &key name type)
  (let ((directory (pathname-directory pathname)))
    (when (eq (car directory) :absolute)
      (setf (car directory) :relative))
    (merge-pathnames
     (make-pathname :name (or name (pathname-name pathname))
                    :type (or type (pathname-type pathname))
                    :directory directory)
     (asdf-system-source-directory system))))

(defvar *contrib-dir* (system-relative-pathname
		       (asdf:find-system :stumpwm)
		       (make-pathname :directory '(:relative "contrib")))
  "The location of the contrib modules on your system.")

(defcommand set-contrib-dir (dir) ((:string "Directory: "))
  "Sets the location of the contrib modules"
  (unless (string= "/" (subseq dir (1- (length dir))))
    (setf dir (concat dir "/")))
  (setf *contrib-dir* (pathname dir)))

(define-stumpwm-type :module (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (list-modules))))

(defun list-modules ()
  "Return a list of the available modules."
  (mapcar 'pathname-name
          (directory (make-pathname :defaults *contrib-dir*
				    :name :wild
				    :type "lisp"))))

(defun find-module (name)
  (probe-file (make-pathname :defaults *contrib-dir*
                             :name name
                             :type "lisp")))

(defcommand load-module (name) ((:module "Load Module: "))
  "Loads the contributed module with the given NAME."
  ;; FIXME: This should use ASDF in the future. And maybe there should
  ;; be an extra stumpwm-contrib repository.
  (let ((module (find-module name)))
    (if module
        (load module)
        (error "No such module: ~a" name))))

;; End of file

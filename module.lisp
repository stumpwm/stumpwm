;; Copyright (C) 2008 John Li, Shawn Betts
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
;; FIXME: What happens when stumpwm is installed somewhere? The
;; contrib modules should probably be installed somewhere and this
;; code updated to load the modules from the correct place.

;; Code:

(in-package #:stumpwm)

(export '(load-module
          list-modules
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

(define-stumpwm-type :module (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (list-modules))))

(defun list-modules ()
  "Return a list of the available modules."
  (mapcar 'pathname-name
          (directory (system-relative-pathname
                      (asdf:find-system :stumpwm) 
                      (make-pathname :directory '(:relative "contrib")
                                     :name :wild
                                     :type "lisp")))))

(defun find-module (name)
  (system-relative-pathname (asdf:find-system :stumpwm)
                            (make-pathname :directory '(:relative "contrib")
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

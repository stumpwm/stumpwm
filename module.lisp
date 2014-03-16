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
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; Use `set-contrib-dir' to set the location stumpwm searches for modules.

;; Code:

(in-package #:stumpwm)

(export '(load-module
          list-modules
          *load-path*
          init-load-path
	  set-contrib-dir
          find-module
          add-to-load-path))

(defvar *contrib-dir*
  #.(asdf:system-relative-pathname (asdf:find-system :stumpwm)
                                   (make-pathname :directory
                                                  '(:relative "contrib")))
  "The location of the contrib modules on your system.")

(defun flatten (ls)
  (labels ( (mklist (x) (if (listp x) x (list x))) )
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))

(defun build-load-path (path)
  "Maps subdirectories of path, returning a list of all subdirs in the
  first two levels which contain any files ending in .asd"
  (flatten 
   (mapcar (lambda (dir) 
             (let ((asd-file (car (directory 
                                   (make-pathname :directory (directory-namestring dir) 
                                                  :name :wild 
                                                  :type "asd")))))
               (when asd-file
                 (directory (directory-namestring asd-file))))) 
           ;; TODO, make this truely recursive
           (directory (concat (if (stringp path) path
                                  (directory-namestring path))
                              "*/*")))))

(defvar *load-path* nil
  "A list of paths in which modules can be found, by default it is
  populated by any asdf systems found in the first two levels of
  `*contrib-dir*' set from the configure script when StumpWM was built, or
  later by the user using `set-contrib-dir'")

(defun sync-asdf-central-registry (load-path)
  "Sync `LOAD-PATH' with `ASDF:*CENTRAL-REGISTRY*'"
  (setf asdf:*central-registry*
        (union load-path asdf:*central-registry*)))

(defun init-load-path (path)
  (let ((load-path (build-load-path path)))
    (setf *load-path* load-path)
    ;(format t "~{~a ~%~}" *load-path*)
    (sync-asdf-central-registry load-path)))

(init-load-path *contrib-dir*)

(defcommand set-contrib-dir (dir) ((:string "Directory: "))
  "Sets the location of the contrib modules"
  (when (stringp dir)
    (setf dir (pathname (concat dir "/"))))
  (setf *contrib-dir* dir)
  (init-load-path *contrib-dir*))

(define-stumpwm-type :module (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (list-modules) :require-match t)))

(defun list-modules ()
  "Return a list of the available modules."
  (flet ((list-module (dir) 
           (mapcar 'pathname-name
                   (directory (make-pathname :defaults dir
                                             :name :wild
                                             :type "asd")))))
    (flatten (mapcar #'list-module *load-path*))))

(defun find-module (name)
  (if name
      (find name (list-modules) :test #'string=)
      nil))

(defun ensure-pathname (path)
  (if (stringp path) (first (directory path))
      path))

(defun add-to-load-path (path)
  "If `PATH' is not in `*LOAD-PATH*' add it, check if `PATH' contains
an asdf system, and if so add it to the central registry"
  (let* ((pathspec (find (ensure-pathname path)  *load-path*))
         (in-central-registry (find pathspec asdf:*central-registry*))
         (is-asdf-path (directory (make-pathname :defaults path
                                                 :name :wild
                                                 :type "asd"))))
    (cond ((and pathspec in-central-registry is-asdf-path) *load-path*)
          ((and pathspec is-asdf-path (not in-central-registry)) 
           (setf asdf:*central-registry* (append (list pathspec) asdf:*central-registry*)))
          ((and is-asdf-path (not pathspec)) 
           (setf asdf:*central-registry* 
                 (append (list (ensure-pathname path)) asdf:*central-registry*))
           (setf *load-path* (append (list (ensure-pathname path)) *load-path*)))
          (T *load-path*))))

(defcommand load-module (name) ((:module "Load Module: "))
  "Loads the contributed module with the given NAME."
  (let ((module (find-module name)))
      (when module
        (asdf:operate 'asdf:load-op module))))


;; End of file

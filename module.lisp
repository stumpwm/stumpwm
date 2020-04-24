;; Copyright (C) 2008 Julian Stecklina, Shawn Betts, Ivy Foster
;; Copyright (C) 2014 David Bjergaard
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
;; Use `set-module-dir' to set the location stumpwm searches for modules.

;; Code:

(in-package #:stumpwm)

(export '(load-module
          list-modules
          *load-path*
          *module-dir*
          init-load-path
          set-module-dir
          find-module
          add-to-load-path))

(defvar *module-dir*
  (pathname-as-directory (concat (getenv "HOME") "/.stumpwm.d/modules"))
  "The location of the contrib modules on your system.")

(defun build-load-path (path)
  "Maps subdirectories of path, returning a list of all subdirs in the
  path which contain any files ending in .asd"
  (map 'list #'directory-namestring
       (remove-if-not (lambda (file)
                        (equal "asd"
                               (nth-value 1 (uiop:split-name-type (file-namestring file)))))
                      (list-directory-recursive path t))))

(defvar *load-path* nil
  "A list of paths in which modules can be found, by default it is
  populated by any asdf systems found in `*module-dir*' set from the
  configure script when StumpWM was built, or later by the user using
  `add-to-load-path'")

(defun sync-asdf-central-registry (load-path)
  "Sync `LOAD-PATH' with `ASDF:*CENTRAL-REGISTRY*'"
  (setf asdf:*central-registry*
        (union load-path asdf:*central-registry*)))

(defun init-load-path (path)
  "Recursively builds a list of paths that contain modules.  This is
called each time StumpWM starts with the argument `*module-dir'"
  (let ((load-path (build-load-path path)))
    (setf *load-path* load-path)
    ;(format t "~{~a ~%~}" *load-path*)
    (sync-asdf-central-registry load-path)))

(defun set-module-dir (dir)
  "Sets the location of the for StumpWM to find modules"
  (when (stringp dir)
    (setf dir (pathname (concat dir "/"))))
  (setf *module-dir* dir)
  (init-load-path *module-dir*))

(define-stumpwm-type :module (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (list-modules) :require-match t)))

(defun find-asd-file (path)
  "Returns the first file ending with asd in `PATH', nil else."
  (first (remove-if-not (lambda (file)
                          (uiop:string-suffix-p (file-namestring file) ".asd"))
                        (list-directory path))))

(defun list-modules ()
  "Return a list of the available modules."
  (flet ((list-module (dir)
           (pathname-name
            (find-asd-file dir))))
    (flatten (mapcar #'list-module *load-path*))))

(defun find-module (name)
  (if name
      (find name (list-modules) :test #'string=)
      nil))

(defun ensure-pathname (path)
  (if (stringp path) (first (directory path))
      path))
(defcommand set-contrib-dir () (:rest)
  "Deprecated, use `add-to-load-path' instead"
  (message "Use add-to-load-path instead."))
(defcommand add-to-load-path (path) ((:string "Directory: "))
  "If `PATH' is not in `*LOAD-PATH*' add it, check if `PATH' contains
an asdf system, and if so add it to the central registry"
  (let* ((pathspec (find (ensure-pathname path)  *load-path*))
         (in-central-registry (find pathspec asdf:*central-registry*))
         (is-asdf-path (find-asd-file path)))
    (cond ((and pathspec in-central-registry is-asdf-path) *load-path*)
          ((and pathspec is-asdf-path (not in-central-registry))
           (push pathspec asdf:*central-registry*))
          ((and is-asdf-path (not pathspec))
           (push (ensure-pathname path) asdf:*central-registry*)
           (push (ensure-pathname path) *load-path*))
          (T *load-path*))))

(defcommand load-module (name) ((:module "Load Module: "))
  "Loads the contributed module with the given NAME."
  (let ((module (find-module (string-downcase name))))
    (if module
        (asdf:operate 'asdf:load-op module)
        (error "Could not load or find module: ~s" name))))
;; End of file

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
	  *contrib-dir*
	  set-contrib-dir
          find-module))

(defun module-string-as-directory (dir)
  (unless (string= "/" (subseq dir (1- (length dir))))
    (setf dir (concat dir "/")))
  (pathname dir))

(defvar *contrib-dir*
  #.(asdf:system-relative-pathname (asdf:find-system :stumpwm)
                                   (make-pathname :directory
                                                  '(:relative "contrib")))
  "The location of the contrib modules on your system.")

(defcommand set-contrib-dir (dir) ((:string "Directory: "))
    "Sets the location of the contrib modules"
  (setf *contrib-dir* (module-string-as-directory dir)))

(define-stumpwm-type :module (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (list-modules) :require-match t)))

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
  (when name
    (let ((module (find-module name)))
      (when module
          (load module)))))


;; End of file

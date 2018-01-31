;;; -*- Mode: Lisp -*-

;;; This code is taken from CL-FAD. Original copyright notice follows:

;;; $Header: /usr/local/cvsrep/cl-fad/fad.lisp,v 1.35 2009/09/30 14:23:10 edi Exp $

;;; Copyright (c) 2004, Peter Seibel.  All rights reserved.
;;; Copyright (c) 2004-2009, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :stumpwm)

(export '(list-directory
          pathname-as-directory))

(defun list-directory (dirname)
  (uiop:directory* dirname))

(defun pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
form."
  (uiop:ensure-directory-pathname pathspec)))



(defun list-directory-recursive (dirname &optional flatten-p)
  "Returns a list of pathnames corresponding to the truenames all
  files within the directory and in any subdirectories.  If
  `FLATTEN-P' is non-nil, flatten the list."
  (let ((files (map 'list (lambda (dir)
               (if (directory-pathname-p dir)
                   (list-directory-recursive dir)
                   dir)) (uiop:directory* dirname))))
    (if flatten-p
      (flatten files)
      files)))
;;; EOF

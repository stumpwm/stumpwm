;; Copyright (C) 2020 Javier Olaechea
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

;;; Commentary:
;;
;; This file contains the entry-point for the StumpWM executable.
;;
;;; Code:

(in-package #:stumpwm)

(export '(main))

(defun main ()
  (let ((argv (uiop:command-line-arguments)))
    (if (find "--generate-manual" argv :test #'string-equal)
        (generate-manual)
        (stumpwm))))

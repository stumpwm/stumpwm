;; package.lisp -- 
;; Copyright (C) 2003-2008 Shawn Betts
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

(defpackage #:config-system
  (:use :cl #:alexandria)
  (:export config-info
           config-info-name
           config-info-validator
           config-info-default
           config-info-doc
           config-info-value
           describe-all-config-info
           describe-config-info
           config-error
           config-not-found-error
           invalid-datum-error
           define-config-enum
           define-config-parameter
           define-config-var
           list-all-configurations
           get-configuration-info
           set-configuration
           set-configuration-atomic
           reset-configuration
           with-atomic-update))

(defpackage :stumpwm
  (:use :cl
        #:alexandria)
  (:shadow #:yes-or-no-p #:y-or-n-p))

(defpackage :stumpwm-user
  (:use :cl :stumpwm))

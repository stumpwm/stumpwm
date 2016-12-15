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

(defpackage :stumpwm
  (:use :cl)
  (:shadow #:yes-or-no-p #:y-or-n-p))

(defpackage :stumpwm-user
  (:use :cl :stumpwm))

(defpackage :stumpui
  (:use :cl)
  (:export
   ;; Window protocol
   #:window-p
   #:window-screen
   #:window-event-mask
   #:window-xwin
   #:window-gravity
   #:window-required-size
   #:window-show
   #:window-hide
   #:window-redraw
   ;; Text window protocol
   #:text-window-p
   #:text-window-ccontext
   #:text-window-text
   #:text-window-highlights
   #:text-window-padding
   #:text-window-foreground-color
   #:text-window-background-color
   #:text-window-border-color
   #:text-window-border-width
   ;; Window timeout protocol
   #:window-supports-timeout-p
   #:window-schedule-timeout
   #:window-cancel-timeout
   ;; Implementing classes
   #:window
   #:text-window
   #:window-timeout-mixin))

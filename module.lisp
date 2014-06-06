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
;; A Module must be an ASDF loadable system.

;; Code:

(in-package #:stumpwm)

(export '(load-module
          ))

(defcommand load-module (name) ((:string "Load Module: "))
  "Loads the contributed module with the given NAME."
  (when name
    (handler-case (asdf:load-system name)
      (asdf:SYSTEM-DEFINITION-ERROR (err)
        (message "~a" err)))))

;; End of file

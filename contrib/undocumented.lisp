;; Copyright (C) 2011 Ben Spencer
;;
;; This module is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This module is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; Look for stuff that should probably be in the manual that isn't
;;
;; Code:

(in-package :stumpwm)

(defun list-undocumented (&optional (manual #p"stumpwm.texi.in"))
  "List symbols that are exported from the stumpwm package and have
  documentation strings but do not appear in the manual"
  (let ((documented '()))
    (with-open-file (s manual :direction :input)
      (loop for line = (read-line s nil s)
         until (eq line s) do
           (ppcre:register-groups-bind (sym) ("^[@%#\\$!]{3} (.*)" line)
             (push sym documented))))
    (loop for sym being the external-symbols in :stumpwm
       when (and (or (documentation sym 'function)
                     (documentation sym 'variable))
                 (not (find sym documented :test #'string-equal)))
       collecting sym)))

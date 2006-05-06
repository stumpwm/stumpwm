;;; -*- Mode: Lisp -*-

(defpackage :stumpwm-system
  (:use :cl :asdf))
(in-package :stumpwm-system)

(ignore-errors (require :cmucl-clx))
(ignore-errors (require :clx))
;;(require :gray-streams)

#+sbcl (require 'sb-posix)

(defsystem :stumpwm
  :name "StumpWM"
  :author "Shawn Betts <sabetts@vcn.bc.ca>"
  :version "0.0.3"
  :maintainer "Shawn Betts <sabetts@vcn.bc.ca>"
  ;; :license "GNU General Public License"
  :description "A tiling, keyboard driven window manager" 
  :components ((:file "package")
	       (:file "keysyms" :depends-on ("package"))
	       (:file "keytrans" :depends-on ("keysyms"))
	       (:file "primitives" :depends-on ("package"))	 
	       (:file "kmap" :depends-on ("primitives" "keysyms" "keytrans"))
	       (:file "input" :depends-on ("primitives" "kmap"))
	       (:file "core" :depends-on ("primitives" "input"))
	       (:file "user" :depends-on ("primitives" "core" "input"))
	       (:file "stumpwm" :depends-on  ("primitives" "core"))))


;;; -*- Mode: Lisp -*-

(defpackage :stumpwm-system
  (:use :cl :asdf))
(in-package :stumpwm-system)

(defsystem :stumpwm
  :name "StumpWM"
  :author "Shawn Betts <sabetts@vcn.bc.ca>"
  :version "0.0.3"
  :maintainer "Shawn Betts <sabetts@vcn.bc.ca>"
;  :license "GNU General Public License"
  :description "A tiling, keyboard driven window manager" 
  :depends-on (:cmucl-clx :port)
  :components ((:file "package")
	       (:file "primitives" :depends-on ("package"))
	       (:file "input" :depends-on ("primitives"))
	       (:file "core" :depends-on ("primitives"
						  "input"))
	       (:file "user" :depends-on ("primitives"
						  "core"
						  "input"))
	       (:file "stumpwm" :depends-on  ("primitives"
					      "core"))))

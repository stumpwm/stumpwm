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
	       (:file "stumpwm-primitives")
	       (:file "stumpwm-input" :depends-on ("stumpwm-primitives"))
	       (:file "stumpwm-core" :depends-on ("stumpwm-primitives"
						  "stumpwm-input"))
	       (:file "stumpwm-user" :depends-on ("stumpwm-primitives"
						  "stumpwm-core"
						  "stumpwm-input"))
	       (:file "stumpwm" :depends-on  ("stumpwm-primitives"
					      "stumpwm-core"))))

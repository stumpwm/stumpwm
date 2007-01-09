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
	       (:file "primitives" :depends-on ("package"))	 
	       (:file "keysyms" :depends-on ("primitives"))
	       (:file "keytrans" :depends-on ("keysyms"))
	       (:file "kmap" :depends-on ("keytrans"))
	       (:file "input" :depends-on ("kmap"))
	       (:file "core" :depends-on ("input"))
	       (:file "user" :depends-on ("core"))
	       (:file "mode-line" :depends-on ("user"))
	       (:file "version" :depends-on ("mode-line"))
	       (:file "stumpwm" :depends-on  ("version"))))


;;; -*- Mode: Lisp -*-

(defpackage :stumpwm-system
  (:use :cl :asdf))
(in-package :stumpwm-system)

;; This is a hack for debian because it calls cmucl's clx
;; cmucl-clx. *very* annoying. I don't actually know if debian still
;; does this.
#+cmu (progn
	  (ignore-errors (require :cmucl-clx))
	  (ignore-errors (require :clx)))
;; Otherwise just load clx
#+sbcl(require :clx)

#+sbcl (require :sb-posix)

(defsystem :stumpwm
  :name "StumpWM"
  :author "Shawn Betts <sabetts@vcn.bc.ca>"
  :version "CVS"
  :maintainer "Shawn Betts <sabetts@vcn.bc.ca>"
  ;; :license "GNU General Public License"
  :description "A tiling, keyboard driven window manager" 
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:file "package")
	       (:file "primitives")
	       (:file "keysyms")
	       (:file "keytrans")
	       (:file "kmap")
	       (:file "input")
	       (:file "core")
	       (:file "user")
               (:file "fdump")
	       (:file "mode-line")
	       (:file "stumpwm")
	       ;; keep this last so it always gets recompiled if
	       ;; anything changes
	       (:file "version")))


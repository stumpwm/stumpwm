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

(defsystem :stumpwm
  :name "StumpWM"
  :author "Shawn Betts <sabetts@vcn.bc.ca>"
  :version "0.9.7"
  :maintainer "Shawn Betts <sabetts@vcn.bc.ca>"
  ;; :license "GNU General Public License"
  :description "A tiling, keyboard driven window manager" 
  :serial t
  :depends-on (:cl-ppcre #-(or cmu clisp) :clx :clx-truetype #+sbcl :sb-posix)
  :components ((:file "package")
	       (:file "primitives")
               (:file "workarounds")
	       (:file "wrappers")
               (:file "pathnames")
	       (:file "keysyms")
	       (:file "keytrans")
	       (:file "kmap")
	       (:file "input")
	       (:file "core")
               (:file "command")
               (:file "menu")
               (:file "screen")
               (:file "head")
               (:file "group")
               (:file "window")
               (:file "floating-group")
               (:file "tile-group")
               (:file "tile-window")
               (:file "window-placement")
               (:file "message-window")
               (:file "selection")
	       (:file "user")
               (:file "iresize")
               (:file "bindings")
               (:file "events")
               (:file "help")
               (:file "fdump")
	       (:file "time")
	       (:file "mode-line")
	       (:file "color")
               (:file "module")
	       (:file "stumpwm")
	       (:file "font-rendering")
	       ;; keep this last so it always gets recompiled if
	       ;; anything changes
	       (:file "version")))


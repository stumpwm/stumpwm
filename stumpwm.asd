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
  :version "0.9.9"
  :maintainer "David Bjergaard <dbjergaard@gmail.com>"
  ;; :license "GNU General Public License"
  :description "A tiling, keyboard driven window manager" 
  :serial t
  :depends-on (:cl-ppcre #-cmu :clx #+sbcl :sb-posix)
  :components ((:file "package")
               (:file "primitives")
               (:file "workarounds")
               (:file "wrappers")
               (:file "pathnames")
               (:file "font-rendering")
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
               (:file "bindings")
               (:file "events")
               (:file "window")
               (:file "floating-group")
               (:file "tile-group")
               (:file "tile-window")
               (:file "window-placement")
               (:file "message-window")
               (:file "selection")
               (:file "module")
               (:file "ioloop")
               (:file "stumpwm")
               (:file "user")
               (:file "iresize")
               (:file "help")
               (:file "fdump")
               (:file "time")
               (:file "mode-line")
               (:file "mode-line-formatters")
               (:file "color")
               (:file "wse")
               ;; keep this last so it always gets recompiled if
               ;; anything changes
               (:file "version")))

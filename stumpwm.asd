;;; -*- Mode: Lisp -*-

(defpackage :stumpwm-system
  (:use :cl :asdf))
(in-package :stumpwm-system)

(defsystem :stumpwm
  :name "StumpWM"
  :author "Shawn Betts <sabetts@vcn.bc.ca>"
  :version "1.0.1"
  :maintainer "David Bjergaard <dbjergaard@gmail.com>"
  ;; :license "GNU General Public License"
  :description "A tiling, keyboard driven window manager"
  :serial t
  :depends-on (#:alexandria
               #:cl-ppcre
               #:clx
               #:sb-posix
               #:sb-introspect)
  :components ((:file "package")
               (:file "debug")
               (:file "primitives")
               (:file "wrappers")
               (:file "pathnames")
               (:file "font-rendering")
               (:file "keysyms")
               (:file "keytrans")
               (:file "kmap")
               (:file "input")
               (:file "core")
               (:file "command")
               (:file "menu-declarations")
               (:file "menu-definitions")
               (:file "screen")
               (:file "head")
               (:file "group")
               (:file "bindings")
               (:file "events")
               (:file "window")
               (:file "floating-group")
               (:file "tile-window")
               (:file "tile-group")
               (:file "window-placement")
               (:file "message-window")
               (:file "selection")
               (:file "module")
               (:file "ioloop")
               (:file "timers")
               (:file "stumpwm")
               (:file "user")
               (:file "interactive-keymap")
               (:file "iresize")
               (:file "help")
               (:file "fdump")
               (:file "time")
               (:file "mode-line")
               (:file "mode-line-formatters")
               (:file "color")
               (:file "wse")
               (:file "dynamic-window")
               (:file "remap-keys")
               ;; keep this last so it always gets recompiled if
               ;; anything changes
               (:file "version"))
  :in-order-to ((test-op (test-op "stumpwm-tests"))))

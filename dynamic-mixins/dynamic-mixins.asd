(defpackage :dynamic-mixins.asdf
  (:use #:cl #:asdf))

(in-package :dynamic-mixins.asdf)

(defsystem :dynamic-mixins
  :description "Simple dynamic class mixing without manual permutations"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "0.0"

  :depends-on (:alexandria)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "dynamic-mixins")))

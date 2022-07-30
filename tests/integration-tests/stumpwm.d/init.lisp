;; Exit with non-zero exit code on error at top level, during loading this rc file, and during command execution.
(setf *top-level-error-action* :abort)

;; Configuration
(setf *timeout-frame-indicator-wait* 60)

;; Invariant testing
(load "~/.stumpwm.d/invariants.lisp")

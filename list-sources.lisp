(in-package #:cl-user)

(load "load-stumpwm.lisp")

(let ((components (asdf:component-children (asdf:find-system :stumpwm))))
  (loop for component in components
        do
        (let ((component-path (asdf:component-relative-pathname component)))
          (if (typep component 'asdf:module)
              (loop for module-child in (asdf:component-children component)
                    do
                    (format t "~a~a~%" component-path (asdf:component-relative-pathname module-child))))
          (format t "~a~%" component-path))))

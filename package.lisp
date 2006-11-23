(defpackage :stumpwm
  (:use :cl)
  (:export #:stumpwm
	   #:*top-level-error-action*
	   #:*screen-list*
	   #:getenv
	   #:split-string
	   #:*version*
	   ;; message bar
	   #:set-fg-color
	   #:set-bg-color
	   #:set-border-color
	   #:set-font
	   #:echo-string
	   #:*timeout-wait*
	   #:*timeout-frame-indicator-wait*
	   #:*message-window-padding*
	   #:*message-window-gravity*
	   ;; input bar
	   #:read-one-line
	   #:read-one-char
	   #:input-insert-string
	   #:input-insert-char
	   #:*input-map*
	   #:*input-window-gravity*
	   ;; keys
	   #:define-key
	   #:undefine-key
	   #:*root-map*
	   #:*top-map*
	   #:sync-keys
	   #:kbd
	   #:set-prefix-key
	   #:make-sparse-keymap
	   #:lookup-command
	   #:lookup-key
	   ;; commands
	   #:define-stumpwm-command
	   ;; hooks
	   #:run-hook-with-args
	   #:run-hook
	   #:add-hook
	   #:remove-hook
	   #:*map-window-hook*
	   #:*unmap-window-hook*
	   #:*new-window-hook*
	   #:*destroy-window-hook*
	   #:*focus-window-hook*
	   #:*unfocus-window-hook*
	   #:*start-hook*
	   #:*internal-loop-hook*
	   #:*focus-frame-hook*
	   ;; mode line
	   #:*mode-line-screen-position*
	   #:*mode-line-border-width*
	   #:*mode-line-pad-x*
	   #:*mode-line-pad-y*
	   #:*mode-line-background-color*
	   #:*mode-line-foreground-color*
	   #:*mode-line-border-color*
	   #:*screen-mode-line-format*
	   #:toggle-mode-line
	   ;; shell
	   #:run-shell-command
	   #:*shell-program*
	   ;; selection
	   #:set-x-selection
	   #:get-x-selection
	   ;; windows
	   #:*maxsize-border-width*
	   #:*transient-border-width*
	   #:*normal-border-width*
	   #:*focus-color*
	   #:*unfocus-color*
	   #:*window-formatters*
	   #:*window-format*
	   ;; frames
	   #:*frame-number-map*
	   ))

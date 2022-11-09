#!/bin/bash
set -ex

#Initialize and configure
start-xvfb-with-max-resolution 2000 2000
cat >> ~/.stumpwm.d/init.lisp <<EOF
(in-package :stumpwm)
(set-fg-color "#ffffbb")
(set-bg-color "#004000")
(set-border-color "#8080ff")
(set-win-bg-color "#ff00ff")
(set-focus-color "#00ff00")
(set-unfocus-color "#ff0000")
(set-float-focus-color "#00c000")
(set-float-unfocus-color "#c00000")
(set-msg-border-width 3)
(set-frame-outline-width 4)
(setf *maxsize-border-width* 5
      *message-window-padding* 6
      *message-window-y-padding* 7
      *mode-line-background-color* "#000040"
      *mode-line-border-color* "#80ff80"
      *mode-line-border-width* 8
      *mode-line-foreground-color* "#bbffff"
      *mode-line-pad-x* 9
      *mode-line-pad-y* 10
      *mode-line-position* :top
      *mode-line-timeout* 1
      *normal-border-width* 11
      *transient-border-width* 12
      *window-border-style* :tight
      )
EOF
start-stumpwm
set-resolution 640 480
screenshot 1

#Open 2 windows and test that it shows a difference
open-test-window
open-test-window
stumpwm-cmd pull-hidden-other
screenshot 2
screenshots-differ 1 2

#Test that splitting shows a difference
stumpwm-cmd hsplit
screenshot 3
screenshots-differ 2 3

#Test that moving focus shows a difference
stumpwm-cmd move-focus right
screenshot 4
screenshots-differ 3 4

#Test that moving focus back matches what we had
stumpwm-cmd move-focus left
screenshot 5
screenshots-match 3 5

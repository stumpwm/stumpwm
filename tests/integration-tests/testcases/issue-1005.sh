#!/bin/bash
set -ex

# Try to reproduce top-left pixel bug (see issue #1005)

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 427

stumpwm-cmd eval '(setf *window-border-style* :none)'
stumpwm-cmd eval '(setf *mode-line-border-color* "#ff00ff")'

screenshot 1
stumpwm-cmd mode-line
stumpwm-cmd mode-line
screenshot 2

open-test-window-with-font-size 3
screenshot 3
set-resolution 1280 854
screenshot 4
set-resolution 640 427
screenshot 5

screenshots-match 1 2
screenshots-differ 3 4
screenshots-match 3 5

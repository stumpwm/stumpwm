#!/bin/bash
set -ex

# Try to reproduce top-left pixel bug

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 427

stumpwm-cmd eval '(SETF *WINDOW-BORDER-STYLE* :NONE)'
open-test-window-with-font-size 3
screenshot 1
set-resolution 1280 854
screenshot 2
screenshots-differ 1 2
set-resolution 640 427
screenshot 3
screenshots-match 1 3

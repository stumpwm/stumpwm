#!/bin/bash
set -ex

# Test that frame indicator still renders correctly after a window opens

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 480
stumpwm-cmd eval '(SETF *TIMEOUT-FRAME-INDICATOR-WAIT* 60)'

open-test-window
stumpwm-cmd vsplit
screenshot 1
open-test-window
screenshot 2
screenshots-match 1 2

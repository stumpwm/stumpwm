#!/bin/bash
set -ex

# Test that the frame indicator is not drawn after `remove` results in an only frame

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 480
stumpwm-cmd eval '(SETF *TIMEOUT-FRAME-INDICATOR-WAIT* 60)'

screenshot 1
stumpwm-cmd hsplit
stumpwm-cmd remove
screenshot 2
screenshots-match 1 2

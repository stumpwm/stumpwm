#!/bin/bash
set -ex

# Test that the frame indicator does not remain in the previous position after `only`

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 480

stumpwm-cmd vsplit
stumpwm-cmd only
screenshot 1
stumpwm-cmd vsplit
stumpwm-cmd move-focus down
stumpwm-cmd only
screenshot 2
screenshots-match 1 2

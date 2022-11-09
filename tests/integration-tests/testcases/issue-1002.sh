#!/bin/bash
set -ex

# Try to reproduce frame indicator issues (see issue #1002)

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 480

# Test that frame indicator is not drawn after `remove` results in an only frame
screenshot 1
stumpwm-cmd hsplit
stumpwm-cmd remove
screenshot 2
screenshots-match 1 2

# Test that the frame indicator does not remain in the previous position after `only`
stumpwm-cmd vsplit
stumpwm-cmd only
screenshot 3
stumpwm-cmd vsplit
stumpwm-cmd move-focus down
stumpwm-cmd only
screenshot 4
screenshots-match 3 4

#!/bin/bash
set -ex

# Try to reproduce a configuration where a split cannot be moved by resizing any frame (see issue #1015)

start-xvfb-with-max-resolution 2000 2000
cat >> ~/.stumpwm.d/init.lisp <<EOF
(setf *suppress-frame-indicator* t)
(set-frame-outline-width 0)
EOF
start-stumpwm
set-resolution 640 480

# For reference, a window on half the screen.
open-test-window
stumpwm-cmd hsplit
screenshot 1

# For reference, a window smaller than that.
stumpwm-cmd only
stumpwm-cmd hsplit
stumpwm-cmd resize -50 0
screenshot 2

# Create four frames arranged as columns
stumpwm-cmd only
stumpwm-cmd hsplit
stumpwm-cmd hsplit
stumpwm-cmd move-focus right
stumpwm-cmd move-focus right
stumpwm-cmd hsplit
# Grow the two right-most frames and then join them.
stumpwm-cmd resize 50 0
stumpwm-cmd move-focus right
stumpwm-cmd resize 50 0
stumpwm-cmd remove
# Shrink the two left-most frames and then join them.
stumpwm-cmd move-focus left
stumpwm-cmd resize -50 0
stumpwm-cmd move-focus left
stumpwm-cmd resize -50 0
stumpwm-cmd remove

# The result should be two frames with a window to the left that is less than half width, but with issue #1015 it will be exactly half.
screenshot 3

screenshots-differ 1 3
screenshots-match 2 3

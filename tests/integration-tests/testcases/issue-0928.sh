#!/bin/bash
set -ex

# Try to break the tiling by resizing (see issue #928)

start-xvfb-with-max-resolution 1280 1024
start-stumpwm
stumpwm-cmd eval '
(setf *suppress-frame-indicator* t
      *resize-increment* 50)
'

for i in {1..4}; do open-test-window; done
stumpwm-cmd hsplit
stumpwm-cmd vsplit
stumpwm-cmd move-focus down
stumpwm-cmd vsplit
screenshot 1
stumpwm-cmd resize-direction left
screenshot 2
screenshots-differ 1 2
check-invariants 3

#!/bin/bash
set -ex

# Try to reproduce issues with *min-frame-width* and *min-frame-height* not respected (see issue #1016)

start-xvfb-with-max-resolution 2000 2000
cat >> ~/.stumpwm.d/init.lisp <<EOF
(set-frame-outline-width 0)
(setf *suppress-frame-indicator* t
      *min-frame-width* 50
      *min-frame-height* 50)
EOF

# Start at resolution 240x240.
start-stumpwm
set-resolution 240 240

# Open test windows
for i in {1..7}; do open-test-window; done

# Split each direction 3 times.
for i in {1..3}; do stumpwm-cmd hsplit; stumpwm-cmd vsplit; done
# This would create a frame of size 30x30 which is smaller than the minimum size 50x50.
# This should be detected and avoided by the split commands, so that the smallest frame ends up with size 60x60.
check-invariants 1

# Shrink the resolution.
set-resolution 120 120
# This would create a too small frame, which should be detected and avoided.
check-invariants 2

# Create as many horizontal splits as possible on one side and then grow the largest frame on the other side.
# This would shrink the small frames to become too small, which should be detected and avoided.
set-resolution 480 480
stumpwm-cmd only
for i in {1..5}; do stumpwm-cmd hsplit; done
for i in {1..5}; do stumpwm-cmd move-focus right; done
for i in {1..5}; do stumpwm-cmd resize 50 0; done
check-invariants 3
# Ditto vertically
stumpwm-cmd only
for i in {1..5}; do stumpwm-cmd vsplit; done
for i in {1..5}; do stumpwm-cmd move-focus down; done
for i in {1..5}; do stumpwm-cmd resize 0 50; done
check-invariants 4

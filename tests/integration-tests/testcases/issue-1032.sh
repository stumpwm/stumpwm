#!/bin/bash
set -ex

# Initialize and open two windows side by side.
start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 1000 1000
stumpwm-cmd eval '(setf *suppress-frame-indicator* t)'
open-test-window-with-font-size 1
open-test-window-with-font-size 1
stumpwm-cmd hsplit
check-invariants 1
stumpwm-cmd resize 3 0
check-invariants 2

# Check that changing and restoring the resolution doesn't change anything.
screenshot a
set-resolution 200 1000
screenshot b
check-invariants 4
set-resolution 1000 1000
screenshot c
check-invariants 5
screenshots-differ a b
screenshots-match a c

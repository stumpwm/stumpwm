#!/bin/bash
set -ex

# Try to reproduce crash when changing resolution (see issue #1031)

# Initialize and open two windows side by side.
start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 480
open-test-window
open-test-window
stumpwm-cmd hsplit

# Check that changing and restoring the resolution doesn't change anything.
screenshot 1
set-resolution 128 96
set-resolution 640 480
screenshot 2
screenshots-match 1 2

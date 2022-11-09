#!/bin/bash
set -ex

# Test that frame indicator still renders correctly after a window opens (see issue #1004)

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 480

open-test-window
stumpwm-cmd vsplit
screenshot 1
open-test-window
screenshot 2
screenshots-match 1 2

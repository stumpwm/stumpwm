#!/bin/bash
set -ex

# Test that some key sequences give the same result as the corresponding commands

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 480

open-test-window

stumpwm-cmd only
stumpwm-cmd vsplit
screenshot 1
stumpwm-cmd only
screenshot 2
screenshots-differ 1 2
send-keys C-t s
screenshot 3
screenshots-match 1 3

stumpwm-cmd only
stumpwm-cmd hsplit
screenshot 4
stumpwm-cmd only
screenshot 5
screenshots-differ 4 5
send-keys C-t S
screenshot 6
screenshots-match 4 6

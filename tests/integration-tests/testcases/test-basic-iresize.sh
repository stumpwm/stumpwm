#!/bin/bash
set -ex

# Test that iresize can successfully change the size of a frame in the correct direction

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 480

open-test-window
open-test-window

stumpwm-cmd only
stumpwm-cmd hsplit
send-keys C-t r Return # Just to show the message "IRESIZE finished", since that will be in screenshot 3
screenshot 1
send-keys C-t r Left Return # Send keys to use iresize to shrink left frame
screenshot 2
stumpwm-cmd resize-direction right # Restore frame size without using iresize
screenshot 3
screenshots-differ 1 2
screenshots-match 1 3

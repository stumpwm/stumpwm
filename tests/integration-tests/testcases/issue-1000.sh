#!/bin/bash
set -ex

# Test that frame indicator and frame outlines are updated correctly after resolution change (see issue #1000)

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 480

stumpwm-cmd hsplit
set-resolution 1280 960
screenshot 1
stumpwm-cmd only
stumpwm-cmd hsplit
screenshot 2
screenshots-match 1 2

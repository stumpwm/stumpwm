#!/bin/bash
set -ex

# Test check-invariants

start-xvfb-with-max-resolution 640 480
start-stumpwm
check-invariants 1
stumpwm-cmd hsplit-uniformly 3
check-invariants 2
stumpwm-cmd eval '(setf *resize-increment* -50)'
if check-invariants 3 >/dev/null; then
    echo "Some invariant should have been violated but was not."
    exit 1
else
    echo "Some invariant was violated, as expected."
fi

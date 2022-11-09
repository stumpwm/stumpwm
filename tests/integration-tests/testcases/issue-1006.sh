#!/bin/bash
set -ex

# Test that window-related color setting functions update correctly (see issue #1006)

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 480
stumpwm-cmd eval '
    (setf *suppress-frame-indicator* t
          *normal-border-width* 10
          *window-border-style* :thin)'

open-test-window-with-font-size 75
open-test-window-with-font-size 75
open-test-window-with-font-size 75
resetview() {
    stumpwm-cmd only
    stumpwm-cmd hsplit
    stumpwm-cmd vsplit
}
setcolorfuns="set-win-bg-color set-focus-color set-unfocus-color"
on() { stumpwm-cmd eval '('"$1"' "#FF00FF")'; }
off() { stumpwm-cmd eval '('"$1"' "#000000")'; }

for setcolor in $setcolorfuns; do
    off "$setcolor"
done
for setcolor in $setcolorfuns; do
    resetview
    screenshot "${setcolor}-before"
    on "$setcolor"
    screenshot "${setcolor}-after"
    resetview
    screenshot "${setcolor}-after2"
    off "$setcolor"
done
for setcolor in $setcolorfuns; do
    screenshots-differ "${setcolor}-before" "${setcolor}-after"
    screenshots-match "${setcolor}-after" "${setcolor}-after2"
done

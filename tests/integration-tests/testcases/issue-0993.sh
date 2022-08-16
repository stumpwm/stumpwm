#!/bin/bash
set -ex

# Try to reproduce issue with sending C-t to terminals (see issue #993)

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 480

open-test-window-with-bash
screenshot 1 # Should show ""
send-keys a b
screenshot 2 # Should show "ab|"
send-keys Left C-t t # Since `C-t' is the prefix key, this should send `Left C-t' to the terminal
screenshot 3 # Should show "ba|" but with issue #993 will show "a|b"

stumpwm-cmd eval '(set-prefix-key (kbd "C-q"))'
send-keys Right BackSpace BackSpace
screenshot 4 # Should show ""
send-keys a b
screenshot 5 # Should show "ab|"
send-keys Left C-t # Since `C-t' is not the prefix key, this should send `Left C-t' to the terminal
screenshot 6 # Should show "ba|"

screenshots-match 1 4
screenshots-match 2 5
screenshots-match 3 6
screenshots-differ 1 2
screenshots-differ 1 3

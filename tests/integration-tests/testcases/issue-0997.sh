#!/bin/bash
set -ex

# Try to reproduce issue with help reporting wrong keysym for apostrophe (see issue #997)

start-xvfb-with-max-resolution 2000 2000

# Start at resolution 640x480.
start-stumpwm
set-resolution 640 480

# Send keys to get help for apostrophe, greater and quoteright keys
send-keys C-t h k C-t apostrophe
screenshot apostrophe
send-keys C-t h k C-t quoteright
screenshot quoteright
send-keys C-t h k C-t greater
screenshot greater

# Create sceenshots for their respective expected messages
stumpwm-cmd echo 'C-t quoteright is bound to "select".
"SELECT" is an alias for the command "SELECT-WINDOW":
^5SELECT-WINDOW ^BQUERY^*
Switch to the first window that starts with @var{query}. 
Bound to:
"quoteright" in *GROUP-ROOT-MAP* invoking "select"'
screenshot apostrophe-expected
screenshot quoteright-expected
stumpwm-cmd echo 'C-t > is not bound.'
screenshot greater-expected

# Assert correct messages
screenshots-match apostrophe apostrophe-expected
screenshots-match greater greater-expected
screenshots-match quoteright quoteright-expected

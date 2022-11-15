#!/bin/bash
set -ex

# Try to reproduce issue with a crash when the window class has non-latin characters (see issue #972)

start-xvfb-with-max-resolution 2000 2000
start-stumpwm
set-resolution 640 480

stumpwm-cmd hsplit

testapp() {
    cp "$(which xterm)" $1
    stumpwm-cmd mode-line
    stumpwm-cmd echo $2
    ./$1 &
    sleep .5
    stumpwm-cmd mode-line
    screenshot $1
    rm ./$1
}

testapp a something
testapp b other
testapp c something
testapp ä something

screenshots-differ a b
screenshots-match a c
screenshots-match a ä

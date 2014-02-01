# The Stump Window Manager

Stumpwm is a window manager written entirely in Common Lisp. It
attempts to be highly customizable while relying entirely on the
keyboard for input. You will not find buttons, icons, title bars, tool
bars, or any of the other conventional GUI widgets.

These design decisions reflect the growing popularity of productive,
customizable lisp based systems.

## Philosophy 

Stumpwm is a "everything-and-the-kitchen-sink WM" or "the emacs of
WMs."

**StumpWM:Emacs::Text:Windows**

* StumpWM is
  * Hackable
  * A tileable window manager
  * Includes support for floats
  * Written in Common Lisp
  * Compatible with many lisp distributions
  * A Superior window managing experience 
* StumpWM is *not*
  * Minimalist
  * Narrow Scope
  * Configured by editing the source directly
  * A full blown desktop environment
If you want a minimalist tiling window manager, then StumpWM is *not*
what you're looking for.  The code base is ~15k sloc, the binaries
produced are ~60mb.  

StumpWM manages windows the way emacs manages buffers, or the way
screen manages terminals. If you want a flexible, customizable,
hackable desktop experience, look no further.

# Build & Start Stumpwm

## Prerequisites

* a common lisp distribution.  sbcl, clisp, ccl and ecl all work.
* quicklisp
* clx
* cl-ppcre
* cl-xembed

The recommended way to install the dependencies is using Quicklisp.
Follow the instructions at http://www.quicklisp.org/ to install it.
In short: 
```
$ curl -O http://beta.quicklisp.org/quicklisp.lisp
```

```
$ sbcl --load quicklisp.lisp
```
Or insert your favorite lisp distribution (clisp, ccl or ecl). 
Then at the REPL:
```lisp
(quicklisp-quickstart:install)
```
Make sure you have added it to your lisp init file using:
```lisp
 (ql:add-to-init-file)
```
Then, in a repl:
```lisp
 (ql:quickload "clx")
 (ql:quickload "cl-ppcre")
 (ql:quickload "xembed")
```
## Building

Building stumpwm from git requires that you build the configure script:
```
 autoconf
```
If there's already a configure script then just run it.
```
 ./configure
```
By default stumpwm elects sbcl.  If you have multiple lisps installed,
you can explicitly select clisp, ccl, or ecl like so:
```
 ./configure --with-lisp=clisp
```
If your lisps are in strange places you may need to tell the script
where to find them:
```
 ./configure --with-sbcl=/home/sabetts/opt/bin/sbcl
 ./configure --with-clisp=/usr/local/downstairs/to/the/left/clisp
```
Now build it:
```
 make
```
If all goes well, you should have a stumpwm binary now.  You can run
the binary from where it is or install it, along with the .info
documentation, with:
```
 make install
```
Now that you have a binary, call it from your ~/.xinitrc file:
```
 echo /path/to/stumpwm >> ~/.xinitrc
 startx
```
Hopefully that will put you in X running stumpwm!

# CCL And Virtual Memory


On 64bit platforms, CCL reserves a "very large" amount of virtual
memory. If this bothers you for some reason, you can pass the -R or
--heap-reserve option to the binary in your ~/.xinitrc file. See
http://ccl.clozure.com/manual/chapter15.1.html for an explanation.

# Help

There's a texinfo manual, stumpwm.texi.  The build scripts generate an
info file you can read in emacs or with the `info' program.  The
manual for the latest release is available to read online at:

http://www.nongnu.org/stumpwm/manual/stumpwm.html

For other stuff visit the stumpwm wiki:

https://github.com/sabetts/stumpwm/wiki

There's a #stumpwm channel on irc.freenode.net, too.

See http://stumpwm.nongnu.org/community.html for more information on
contacting stumpwm developers and users.

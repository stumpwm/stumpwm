![](http://www.kenanb.com/static/img/stumpwm-logo/stumpwm-logo-stripe.png)
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

**StumpWM:Windows::Emacs:Text**

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

* a common lisp distribution.  sbcl, clisp, ccl and ecl all work (ecl must have been built with clx support, must use version >= 13.5.1 [see here for discussion](https://github.com/sabetts/stumpwm/issues/55)).
* quicklisp (for obtaining the following dependencies, not needed if you use your distribution's package manager.)
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
Hopefully that will put you in X running stumpwm! See [StartUp on the
wiki](https://github.com/sabetts/stumpwm/wiki/StartUp) for more
examples.

## Requirements for multiple monitor setups

For stumpwm to work as intended with multiple monitors setups the 
`xdpyinfo` utility is needed.

# Contributing

Pull requests are always welcome! Here are some guidelines to ensure
that your contribution gets merged in a timely manner: 
* Do's 
  * Preserve comments or docstrings explaining what code does, and
    update them if your patch changes them in a significant way
  * Try to follow an "80 column rule." The current code base does not
    follow this all the time, so don't use it as an example
  * [Use lisp idioms](http://people.ace.ed.ac.uk/staff/medward2/class/moz/cm/doc/contrib/lispstyle.html)
  * If you are working on a major change to the internals, keep us
    informed on stumpwm-devel! Also, it will probably help if the
    changes are made and the incrementally applied to the codebase in
    order to avoid introducing show-stopping bugs.
* Do not's
  * Include emacs local variables 
  * Change whitespace 
  * Write lots of code without supporting comments/documentation
  * Delete comments or docstrings (yes this is a duplicate of above!)
  * Export symbols from packages that aren't widely useful (many times
    a little more thought will reveal how to implement your internal
    change without having to export/break encapsulation)
  * Make stylistic changes that suit your coding style/way of thinking 

Our wiki has fallen into disarray/disrepair, but it is shaping up.  If
you aren't a lisp hacker, you can contribute in the form of
documenting and organizing the wiki. There's a lot of information
floating around, if you find it where you didn't expect it, move or
link to it in a more logical place.

# CCL And Virtual Memory


On 64bit platforms, CCL reserves a "very large" amount of virtual
memory. If this bothers you for some reason, you can pass the -R or
--heap-reserve option to the binary in your ~/.xinitrc file. See
http://ccl.clozure.com/manual/chapter15.1.html for an explanation.

# Help

There's a texinfo manual, stumpwm.texi.  The build scripts generate an
info file you can read in emacs or with the `info' program.  The
manual for the 0.9.7 (slightly out of date) is available to read
online at: [The Manual](http://www.nongnu.org/stumpwm/manual/stumpwm.html)

And, as in emacs, you can always do "C-t h v,f,k,c,w" for docstrings
of Variable,Functions,Keys,Commands, and Where-is respectively.

For other stuff (tips tricks and examples) visit the [stumpwm wiki](https://github.com/sabetts/stumpwm/wiki)

There's a #stumpwm channel on irc.freenode.net, too.

Finally, there's our mailing list (click to sign up)
[stumpwm-devel@nongnu.org](https://lists.nongnu.org/mailman/listinfo/stumpwm-devel).

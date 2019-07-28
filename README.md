![](https://stumpwm.github.io/images/stumpwm-logo-stripe.png)

# The Stump Window Manager

![](https://travis-ci.org/stumpwm/stumpwm.svg)

Stumpwm is a window manager written entirely in Common Lisp. It
attempts to be highly customizable while relying entirely on the
keyboard for input. You will not find buttons, icons, title bars, tool
bars, or any of the other conventional GUI widgets.

These design decisions reflect the growing popularity of productive,
customizable Lisp based systems.

## Philosophy

Stumpwm is a "everything-and-the-kitchen-sink WM" or "the Emacs of
WMs."

**StumpWM:Windows::Emacs:Text**

* StumpWM is
  * Hackable
  * Written in Common Lisp
  * A multi paradigm window manager
  * A Superior window managing experience
* StumpWM is *not*
  * Minimalist
  * Narrow Scope
  * Configured by editing the source directly
  * A full blown desktop environment

If you want a minimalist tiling window manager, then StumpWM is *not*
what you're looking for.  The code base is ~15k sloc, the binaries
produced are ~60mb.

StumpWM manages windows the way Emacs manages buffers, or the way
screen manages terminals. If you want a flexible, customizable,
hackable desktop experience, look no further.

# Build & Start StumpWM

### Prerequisites

* [SBCL](http://www.sbcl.org/)
* [Quicklisp](https://www.quicklisp.org)
* [clx](https://cliki.net/clx)
* [cl-ppcre](https://edicl.github.io/cl-ppcre)
* [alexandria](https://common-lisp.net/project/alexandria)

### SBCL

The recommended way to install SBCL is by downloading one of their
pre-built binaries available in http://www.sbcl.org/platform-table.html or build it
it yourself from its source code.

Please do _not_ install SBCL using your distributions package manager, especially Ubuntu.

As most distro do not provide SBCL latest versions but its stable version, or even worse, outdated binaries.


### Dependencies

The recommended way to install the dependencies is using Quicklisp.

Download it in your $HOME:
``` sh
$ curl -O https://beta.quicklisp.org/quicklisp.lisp
```

Bootstraping Quicklisp with SBCL:

``` sh
$ sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'
```

Make sure you have added QuickLisp to your Lisp compiler init file:

```lisp
(ql:add-to-init-file)
```

Installing dependencies:

```lisp
(ql:quickload "clx")
(ql:quickload "cl-ppcre")
(ql:quickload "alexandria")
```
### Building StumpWM

Building StumpWM from source code requires that you build the configure script:

```
 autoconf
```

If the configure script exist, then just run it:

```
 ./configure
```

Now to build it:

```
 make
```

If everything goes correct, you should have a StumpWM binary at its top folder.

You can just run the StumpWM binary, `exec stumpwm`, or install it in your system,
along with the `.info` documentation:

```
 make install
```

NOTE: Depending on your system policies, it will probably require for admin privilegies.

### Starting StumpWM Up (Minimal Approach)

You can just pick StumpWM in your Login Manager, if you use one.

If you thinks its just to much to call a Window Manager you can use Xinit to boot StumpWM.

You will need an file called `.xinitrc` to be present in your $HOME folder(~/.xinitrc).

If there is no such a file, create it with:

``` sh
 echo "exec /path/to/stumpwm" >> ~/.xinitrc
```

Now we can call Xinit with:

``` sh
 startx
```

Hopefully, StumpWM will load using your graphical system, nowadays X.org and Wayland!

See [StartUp on the wiki](https://github.com/sabetts/stumpwm/wiki/StartUp) for more
examples.

# Contributing

Pull requests are always welcome!

Here are some guidelines to ensure that your contribution gets merged in a timely manner:

* **Do's**
  * Add your name to the list of AUTHORS with your pull request.
  * Preserve comments or docstrings explaining what code does, and
    update them if your patch changes them in a significant way
  * Try to follow an "80 column rule." The current code base does not
    follow this all the time, so don't use it as an example
  * If you export a symbol, you *must* add it to the manual.
  * [Use lisp idioms][lisp-idioms]
  * If you are working on a major change to the internals, keep us
    informed on stumpwm-devel! Also, it will probably help if the
    changes are made and then incrementally applied to the codebase in
    order to avoid introducing show-stopping bugs.

* **Do not's**
  * Include emacs local variables
  * Change whitespace
  * Write lots of code without supporting comments/documentation
  * Delete comments or docstrings (yes this is a duplicate of above!)
  * Export symbols from packages that aren't widely useful (many times
    a little more thought will reveal how to implement your internal
    change without having to export/break encapsulation)
  * Make stylistic changes that suit your coding style/way of thinking

### StumpWM Wiki

If you are not a Lisp Hacker, you can contribute in the form of
documenting and organizing the wiki:

https://github.com/stumpwm/stumpwm/wiki

###  Wishlist

Fancy yourself a Lisp hacker? Here's a wishlist of features for the
StumpWM universe (in no particular order):

* float-splits (ie allow floating windows over tiled ones)
* Float windows within parent applications (specifically dialogs in
  gimp or firefox).
* tab-list showing the contents of the current frame at the side, top,
  or bottom of the frame
* Emacs' iswitchb function implemented in emacs
  * Re-arranging windows between groups
  * Killing windows
  * Marking windows for batch operations
  * Deleting/adding groups
  * Import data from stumpwm to emacs, use an emacs minor mode to
    implement the above features, then export the data back to stumpwm
    and let stumpwm perform the appropriate actions
* Emacs' completing-read-multiple function
* Dynamic tiling
* Lock Screen (with support for leaving notes, bonus points if emacs
  is involved)
* Wallpapers! (support pulling from remote sources, changing based on
  timers, and other hacky features)
* Shutdown, restart, suspend, and hibernate functions that don't
  require root access
* Revamped, mouse-friendly mode-line.
  * Support fixed number of chars for window titles
  * Dynamically trim window titles to fit them all on the mode-line
  * Split the mode-line into multiple cells for containing different information
  * Implement widget icons to indicate system status (new mail, low
    battery, network etc)
  * Support raising windows when left-clicked, closing/killing when right-clicked

# More Information:

There's a Texinfo Manual, stumpwm.texi.

You can generate an info file to read inside Emacs
or with aterminal using the `info` program: `$ make stumpwm.texi`.

The manual for the latest git version is available to read online at:
[StumpWM Manual](https://stumpwm.github.io/)

As in Emacs, you can always call for information on StumpWM features with:

`C-t h v,f,k,c,w` for docstrings of Variable,Functions,Keys,Commands, and Where-is respectively.

Our Wiki has a lot of good information to get you going up easily:
[StumpWm Wiki](https://github.com/stumpwm/stumpwm/wiki)

There's a #stumpwm channel on [Freenode](irc.freenode.net).

Finally, there's our mailing list (click to sign up)
[stumpwm-devel@nongnu.org](https://lists.nongnu.org/mailman/listinfo/stumpwm-devel).

[lisp-idioms]: (http://web.archive.org/web/20160101153032/http://people.ace.ed.ac.uk/staff/medward2/class/moz/cm/doc/contrib/lispstyle.html)
[sbcl]: http://sbcl.org
[sbcl-platform-table]: http://sbcl.org/platform-table.html

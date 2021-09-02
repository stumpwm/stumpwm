# The Stump Window Manager

## Unofficial Branch: Dynamic Floating Group

This is an **unofficial branch** of StumpWM that supports dynamic
floating windows. Please see the [official
repo](https://github.com/stumpwm/stumpwm) for a proper readme
file of StumpWM.

For **building and installing**, follow the exact same procedure
as if you are using the official version. You can also find a
note [here](./tmp/usage.md).

All new lisp definitions are built in a new common lisp package
`stumpwm-dfg`. That means this repo **will not break your
existing StumpWM config**.

There is no official manual for this branch. However, you can see
what it offers quickly in the **short videos** and [an example
config](./tmp/example-config/init.lisp) file below.

The intention this repo is to let interested users to use dynamic
floating windows before it is merged. The author is **dedicated
to fix bugs and keep updated** with the official master branch.
Please feel free open an issue for any kind of requests
(explanation, usage, docstring requests, bugs.. etc). I am very
glad to help. Thank you!

## Features

+ Basic Movement

  ![Basic
  Movement](https://github.com/jcguu95/stumpwm--dynamic-floating-group/blob/dce224245e38c9c6cd6b04f43b8fabddb3ca9935/img/basic-movement.gif)

  + move focus
  + permute window
  + fullscreen
  + move single window

+ Float and Retile

  ![Float and
  Retile](https://github.com/jcguu95/stumpwm--dynamic-floating-group/blob/dce224245e38c9c6cd6b04f43b8fabddb3ca9935/img/float-and-retile.gif)

  + float and resize by `super` + cursor.
  + retile

+ Layout and Ratio

  ![Layout and
  Ratio](https://github.com/jcguu95/stumpwm--dynamic-floating-group/blob/dce224245e38c9c6cd6b04f43b8fabddb3ca9935/img/layout-and-ratio.gif)

  + fullscreen layout + switch focus
  + alter/toggle master ratio
  + left-vertical and horizontal layout

+ Gap

  ![Gap](https://github.com/jcguu95/stumpwm--dynamic-floating-group/blob/dce224245e38c9c6cd6b04f43b8fabddb3ca9935/img/gap.gif)

  + decrease/increase gap size
  + toggle gap
  + set default gap size

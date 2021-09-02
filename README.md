![](https://stumpwm.github.io/images/stumpwm-logo-stripe.png)
# The Stump Window Manager
![](https://travis-ci.org/stumpwm/stumpwm.svg)
[![Gitter](https://badges.gitter.im/stumpwm/community.svg)](https://gitter.im/stumpwm/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

## Unofficial Branch: Dynamic Floating Group

This is an unofficial branch of StumpWM that supports dynamic
floating windows. Please see the official repo for a proper
readme file of StumpWM.

The intention this repo is to let interested users to use dynamic
floating windows before it is merged. The author is dedicated to
fix bugs and keep updated with the official master branch. Please
feel free open an issue for any kind of requests. Thank you!

## Features
+ Basic Movement

  ![Basic Movement](https://github.com/jcguu95/stumpwm--dynamic-floating-group/blob/dce224245e38c9c6cd6b04f43b8fabddb3ca9935/img/basic-movement.gif)

  + move focus
  + permute window
  + fullscreen
  + move single window

+ Float and Retile

  ![Float and Retile](https://github.com/jcguu95/stumpwm--dynamic-floating-group/blob/dce224245e38c9c6cd6b04f43b8fabddb3ca9935/img/float-and-retile.gif)

  + float and resize by `super` + cursor.
  + retile

+ Layout and Ratio

  ![Layout and Ratio](https://github.com/jcguu95/stumpwm--dynamic-floating-group/blob/dce224245e38c9c6cd6b04f43b8fabddb3ca9935/img/layout-and-ratio.gif)

  + fullscreen layout + switch focus
  + alter/toggle master ratio
  + left-vertical and horizontal layout

+ Gap

  ![Gap](https://github.com/jcguu95/stumpwm--dynamic-floating-group/blob/dce224245e38c9c6cd6b04f43b8fabddb3ca9935/img/gap.gif)

  + decrease/increase gap size
  + toggle gap
  + set default gap size
  
## Usage

To build and install, please follow the [official
guide](https://github.com/stumpwm/stumpwm).

The difference in the user space of this branch and the official
repo is controlled in the files `dynamic-floating-group.lisp`,
`package.lisp`, and `AUTHORS`. In particular, all new lisp
definitions are built in a new common lisp package `stumpwm-dfg`.
That means this repo will not break your StumpWM config. 

## Config Example

### Bindings with features in this branch.

``` common-lisp
(in-package :stumpwm-dfg)

;; vim-like movements
(define-key *top-map* (stumpwm:kbd "s-j") "focus-next-window")
(define-key *top-map* (stumpwm:kbd "s-k") "focus-last-window")
(define-key *top-map* (stumpwm:kbd "s-J") "permute-window-list")
(define-key *top-map* (stumpwm:kbd "s-K") "permute-window-list--reverse")

(define-key *top-map* (stumpwm:kbd "s-_") "unfree-all")

(define-key *top-map* (stumpwm:kbd "s-+") "increase-master-ratio")
(define-key *top-map* (stumpwm:kbd "s--") "decrease-master-ratio")
(define-key *top-map* (stumpwm:kbd "s-=") "set-default-master-ratio")

(define-key *top-map* (stumpwm:kbd "s-f") "toggle-fullscreen-layout")
(define-key *top-map* (stumpwm:kbd "s-F") "select-next-layout")

;; 
(define-key *top-map* (stumpwm:kbd "s-F") "select-next-layout")
(define-key *top-map* (stumpwm:kbd "s-M-,") "window-size-increase")
(define-key *top-map* (stumpwm:kbd "s-M-m") "window-size-decrease")
;;
(define-key *top-map* (stumpwm:kbd "s-M-j") "window-move-down")
(define-key *top-map* (stumpwm:kbd "s-M-k") "window-move-up")
(define-key *top-map* (stumpwm:kbd "s-M-l") "window-move-right")
(define-key *top-map* (stumpwm:kbd "s-M-h") "window-move-left")
;;
(define-key *top-map* (stumpwm:kbd "s-[") "gap-toggle")
(define-key *top-map* (stumpwm:kbd "s-]") "gap-set-default")
(define-key *top-map* (stumpwm:kbd "s-}") "gap-increase")
(define-key *top-map* (stumpwm:kbd "s-{") "gap-decrease")
```

### Workspaces

``` common-lisp
;; This removes the default group (workspace),
;; and create 10 dynamic floating groups (workspaces) 
;; named 1, ,2, .., 9, 0.
(grename "1")
(loop for n in '(2 3 4 5 6 7 8 9 0)
      do (stumpwm-dfg::gnew-dyn-float-bg (format nil "~a" n)))
(gkill)
(stumpwm-dfg::gnew-dyn-float "1")

;; keybindings for selecting and moving groups.
(mapcar
 (lambda (n)
   (let ((shift-keys '(")" "!" "@" "#" "$"
                       "%" "^" "&" "*" "(")))
     (define-key *top-map*
         (kbd (format nil "s-~a" n))
       (format nil "gselect ~a" n))
     (define-key *top-map*
         (kbd (format nil "s-~a" (nth n shift-keys)))
       (format nil "gmove ~a" n))))
 '(1 2 3 4 5 6 7 8 9 0))
```

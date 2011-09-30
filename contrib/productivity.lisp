;; Productivity module for StumpWM.
;;
;; Copyright (C) 2008 Ivy Foster
;;
;; Maintainer: Ivy Foster
;;
;; This module is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This module is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; There are those for whom simply using a window manager like StumpWM
;; is enough to force them to buckle down and work, mostly because
;; they don't know how to play around with it (due to a lack of
;; frills). However, there are also those who know how to move around
;; in StumpWM and who may find themselves falling into the same bad
;; habits as those who use "normal" WMs; that is, switching between
;; windows a lot and fiddling with random functions the WM has
;; available. This fixes that by temporarily disabling StumpWM.
;;
;; Incidentally, I'm not positive, but I believe that this may be
;; StumpWM's first Minor Mode.

;;; Usage:
;;
;; Just add the following line to your .stumpwmrc file:
;;
;; (load-module "productivity")
;;
;; ...and then bind `productivity-mode-toggle' to a key in your
;; *top-map*. I recommend H-# (Hyper-shift-3).
;;
;; When you activate productivity-mode, you won't be able to access
;; any of your StumpWM keymaps. That's right--the window manager is
;; effectively disabled! If you try to leave the window, or display
;; the time, or go to another group, or control MPD, or switch frames,
;; or whatever, you will instead be presented with a curt reminder of
;; what you're supposed to be doing. To escape from this mode, just
;; hit the key you have `productivity-mode-toggle' bound to again.
;;
;; Absolutely DO NOT start productivity-mode from your .stumpwmrc file
;; unless you (a) are absolutely *sure* you have
;; productivity-mode-toggle bound to a key already, (b) automatically
;; switch to a window capable of controlling StumpWM (i.e., Emacs or a
;; terminal) in your .stumpwmrc, (c) want to convince your friends
;; your computer is broken, or (d) are crazy.

;;; Configuration:
;;
;; The following can be customized. If no example is given, please see
;; the code.
;;
;; `*productivity-keys*': Any and all keys you have bound to the
;;     *top-map* and their associated commands.
;; ex:
;;     (setf *productivity-keys*
;;           '(("H-t"   *root-map*)
;;             ("C-;"   *rat-map*)
;;             ("Print" "screenshot")))
;;
;; `*productivity-start-message*': What StumpWM should print when you
;;     start productivity-mode.
;;
;; `*productivity-stop-message*': What StumpWM should print when you
;;     stop productivity-mode
;;
;; `*productivity-back-to-work-message*': What StumpWM should print
;;     when you attempt to waste time (you lazy, lazy, um...lazy
;;     person).

;;; TODO:
;;
;; - [ ] Add an option to toggle the mode-line on and off.
;;
;; - [ ] Add an option to display a notifier for this mode, when on,
;;       in the mode-line (a la Emacs' minor modes).
;; 
;;       - Perhaps this could be integrated into a "minor modes"
;;         package? (Just in case somebody insinuates that StumpWM
;;         doesn't have *everything*.)
;;
;; - [ ] Add a timer feature (i.e., have it toggle itself at a given
;;       interval or hour, so you can have your work time and
;;       messing-around-on-the-computer time predetermined).
;;
;; - [ ] Add an option to disable stumpish, slime and/or
;;       stumpwm-mode.el (choose your poison/s).
;;
;; - [ ] Make it do your work for you.
;;
;;       - Note to self: Finish this one first, then have it write the
;;         rest for you.

;;; Code:

(defvar *productivity-mode-is-on* nil
  "Is productivity-mode on?
Do not customize by hand unless you're crazy.")

(defvar *productivity-keys* '(("C-t" *root-map*))
  "List of all the keys you have bound to your `*top-map*'
and their associated commands.")

(defvar *productivity-stop-message* "Break time!"
  "What should StumpWM print when you stop productivity-mode?")

(defvar *productivity-start-message* "Get to work!"
  "What should StumpWM print when you start productivity-mode?")

(defvar *productivity-back-to-work-message* "Get back to work!"
  "What should StumpWM print when you attempt to waste time?")

(defcommand productivity-back-to-work () ()
  (message *productivity-back-to-work-message*))

(defun productivity-mode-on ()
  "Turns on productivity mode. Do not call interactively."
  (setf *productivity-mode-is-on* t)
  (dolist (key *productivity-keys*)
    (define-key *top-map* (kbd (car key)) "productivity-back-to-work"))
  (message *productivity-start-message*))

(defun productivity-mode-off ()
  "Turns off productivity mode. Do not call interactively."
  (setf *productivity-mode-is-on* nil)
  (dolist (key *productivity-keys*)
    (define-key *top-map* (kbd (car key)) (cadr key)))
  (message *productivity-stop-message*))

(defcommand productivity-mode-toggle () ()
  "Toggles productivity mode."
  (if *productivity-mode-is-on*
      (productivity-mode-off)
    (productivity-mode-on)))

;;; End of file

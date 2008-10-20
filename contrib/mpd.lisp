;;; MPD client & formatters for stumpwm
;;;
;;; Copyright 2007 Morgan Veyret, Ivy Foster.
;;;
;;; Maintainer: Morgan Veyret
;;;
;;; This module is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This module is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA
;;;

;;; USAGE:
;;;
;;; Put:
;;;
;;;     (load-module "mpd")
;;;
;;; ...into your ~/.stumpwmrc
;;;
;;; Then you can use "%m" in your mode line format, as well as various commands
;;; defined at the end of the file.
;;;
;;; You can customize the modeline format (*mpd-modeline-fmt*), the status
;;; message displayed by the command mpd-status (*mpd-status-fmt*; note that
;;; this is also used on the modeline), and the status message displayed by the
;;; command mpd-current-song (*mpd-current-song-fmt*). See the documentation for
;;; *mpd-modeline-fmt* for more information.

;;; NOTES:
;;;
;;; See http://mpd.wikia.com/wiki/Protocol_Reference for full protocol

;;; TODO:
;;;
;;; - Implement optional shortening for formatting functions
;;; - Set-crossfade support

;;; CODE:

#-(or sbcl clisp) (error "unimplemented")

(in-package :stumpwm)

(export '(*mpd-timeout*
	  *mpd-collapse-album-length*
	  *mpd-collapse-all-length*
	  *mpd-current-song-fmt*
	  *mpd-formatters-alist*
	  *mpd-status-fmt*
	  *mpd-modeline-fmt*
	  *mpd-volume-step*
	  *mpd-port*
	  mpd-browse-playlist
	  select-song-from-playlist
	  mpd-play
	  mpd-toggle-repeat
	  mpd-toggle-random
	  mpd-toggle-pause
	  mpd-kill
	  mpd-disconnect
	  mpd-connect
	  mpd-browse-artists
	  mpd-browse-albums
	  mpd-update
	  mpd-clear
	  mpd-volume-down
	  mpd-volume-up
	  mpd-set-volume
	  mpd-prev
	  mpd-next
	  mpd-stop
	  mpd-play-track
	  mpd-search-genre
	  mpd-search-album
	  mpd-search-title
	  mpd-search-file
	  mpd-search-artist
	  mpd-search-and-add-genre
	  mpd-search-and-add-album
	  mpd-search-and-add-title
	  mpd-search-and-add-file
	  mpd-search-and-add-artist
	  mpd-add-file
	  mpd-playlist
	  mpd-status
	  mpd-current-song
	  *mpd-map*
	  *mpd-add-map*
	  *mpd-browse-map*
	  *mpd-search-map*))

;;mpd client
(defparameter *mpd-socket* nil)
(defparameter *mpd-server*
  #+clisp
  "localhost"
  #+sbcl
  #(127 0 0 1)
  )
(defparameter *mpd-port* 6600)

(defvar *mpd-timeout* 50)

(defparameter *mpd-timer* nil)

(defvar *mpd-collapse-album-length* nil)
(defvar *mpd-collapse-all-length* nil)

(defmacro with-mpd-connection (&body body)
  `(if *mpd-socket*
       (handler-case (progn ,@body)
                     (error (c) (progn
                                  (message "Error with mpd connection: ~a" c)
                                  (setf *mpd-socket* nil))))
     (message "Error: not connected to mpd~%")))

(defun mpd-send (command)
  "Send command to stream ending with newline"
  (with-mpd-connection
   (#+clisp
    ext:write-char-sequence
    #+sbcl
    write-sequence
    (concatenate  'string command (string #\Newline))
    *mpd-socket*)))

(defun mpd-send-command (cmd)
  (mpd-send cmd)
  (mpd-receive))

(defun mpd-format-command (fmt &rest args)
  (mpd-send-command (apply 'format nil fmt args)))

(defun mpd-termination-p (str)
  (or (mpd-error-p str)
      (mpd-ok-p str)))

(defun mpd-error-p (str)
  (when (>= (length str) 3)
    (equal (subseq str 0 3) "ACK")))

(defun mpd-ok-p (str)
  (equal str "OK"))

(defun mpd-tokenize (str)
  (let ((pos (position #\: str)))
    (list (read-from-string (concatenate 'string
                                         ":"
                                         (subseq str 0 pos)))
          (subseq str (+ pos 2)))))

(defun assoc-value (name list)
  (cadr (assoc name list)))

(defun mpd-receive ()
  "Returns a list containing all data sent by mpd."
  (with-mpd-connection
   (loop for i = (read-line *mpd-socket*)
         when (mpd-error-p i)
         do (message "Error sent back by mpd: ~a" i)
         until (mpd-termination-p i)
         collect (mpd-tokenize i))))

(defun init-mpd-connection ()
  "Connect to mpd server"
    (setf *mpd-socket*
          #+clisp
        (handler-case (socket:socket-connect *mpd-port* *mpd-server*
                                             :element-type 'character)
                      ((or system::simple-os-error error)
                       (err)
                         (format t  "Error connecting to mpd: ~a~%" err)))
          #+sbcl
          (handler-case (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                                                :type :stream :protocol :tcp)))
                          (sb-bsd-sockets:socket-connect s *mpd-server*
                                                         *mpd-port*)
                          (sb-bsd-sockets:socket-make-stream s
                                                             :input t
                                                             :output t
                                                             :buffering :none))
                        ((or simple-error error)
                         (err)
                       (format t  "Error connecting to mpd: ~a~%" err))))
  (when *mpd-socket*
    (when *mpd-timeout*
      (setf *mpd-timer*
            (run-with-timer *mpd-timeout* *mpd-timeout* 'mpd-ping)))
    (read-line *mpd-socket*)))

(defun mpd-ping ()
  (mpd-send-command "ping"))

(defun mpd-search (type what &optional (exact-search nil))
  (mpd-format-command "~a ~a \"~a\""
                      (if exact-search "find"
                          "search")
                      type what))

(defun mpd-add (files)
  (loop for i in files
     do (mpd-format-command "add \"~a\"" i)))

;;; ------------------------------------------------------------------
;;; Formatting
;;; ------------------------------------------------------------------

(dolist (a '((#\m mpd-modeline)))
  (pushnew a *screen-mode-line-formatters* :test 'equal))

(defparameter *mpd-current-song* nil)
(defparameter *mpd-status* nil)

(defun mpd-update-current-song ()
  (setf *mpd-current-song* (mpd-send-command "currentsong")))
(defun mpd-update-status ()
  (setf *mpd-status* (mpd-send-command "status")))

(defun mpd-get-artist ()
  (assoc-value :artist *mpd-current-song*))

(defun mpd-get-album ()
  (assoc-value :album *mpd-current-song*))

(defun mpd-get-date ()
  (assoc-value :date *mpd-current-song*))

(defun mpd-minutes-seconds (time)
  (let ((minutes) (seconds))
    (if (< time 60)
	(progn
	  (setf minutes 0)
	  (setf seconds time))
      (progn
	(setf minutes (write-to-string (floor time 60)))
	(setf seconds (rem time 60))))
    (when (< seconds 10)
      (setf seconds (concat "0" (write-to-string seconds))))
    (format nil "~a:~a" minutes seconds)))

(defun mpd-get-elapsed ()
  (let* ((total (assoc-value :time *mpd-current-song*))
	 (time (assoc-value :time *mpd-status*))
	 (elapsed (parse-integer (subseq time 0 (- (length time) (length total) 1)))))
    (mpd-minutes-seconds elapsed)))

(defun mpd-get-length ()
  (let ((time (parse-integer (assoc-value :time *mpd-current-song*)))
	(minutes) (seconds))
    (mpd-minutes-seconds time)))

(defun mpd-get-status ()
  (cond ((equal (assoc-value :state *mpd-status*) "play") "Playing")
	((equal (assoc-value :state *mpd-status*) "pause") "Paused")
	((equal (assoc-value :state *mpd-status*) "stop") "Stopped")))

(defun mpd-get-file ()
  (assoc-value :file *mpd-current-song*))

(defun mpd-get-volume ()
  (assoc-value :volume *mpd-status*))

(defun mpd-get-xfade ()
  (let ((xfade (assoc-value :xfade *mpd-status*)))
    (if (> (parse-integer xfade) 0)
	(format nil "F=~a" (assoc-value :xfade *mpd-status*)) "_")))

(defun mpd-get-genre ()
  (assoc-value :genre *mpd-current-song*))

(defun mpd-get-number ()
  (write-to-string (1+ (parse-integer (assoc-value :song *mpd-status*)))))

(defun mpd-get-playlistlength ()
  (assoc-value :playlistlength *mpd-status*))

(defun mpd-repeating-p ()
  (if (string= (assoc-value :repeat *mpd-status*) "1")
      t nil))

(defun mpd-get-repeat ()
  (if (mpd-repeating-p) "R" "_"))

(defun mpd-shuffle-p ()
  (if (string= (assoc-value :random *mpd-status*) "1")
      t nil))

(defun mpd-get-shuffle ()
  (if (mpd-shuffle-p) "S" "_"))

(defun mpd-get-title ()
  (assoc-value :title *mpd-current-song*))

(defun mpd-get-track ()
  (assoc-value :track *mpd-current-song*))

(defun mpd-get-song-name ()
  (let* ((artist (assoc-value :artist *mpd-current-song*))
        (album (assoc-value :album *mpd-current-song*))
        (title (assoc-value :title *mpd-current-song*))
         (file (assoc-value :file *mpd-current-song*))
         (song (if (or (null artist)
                      (null album)
                      (null title))
                  (format nil "~a" file)
                   (format nil "~a \"~a\" - ~a"
                          artist
                          (if (and *mpd-collapse-album-length*
                                   (> (length album) *mpd-collapse-album-length*))
                              (concatenate 'string
                                           (subseq album 0 *mpd-collapse-album-length*)
                                           "...")
                              album)
                          title))))
    (if (and *mpd-collapse-all-length*
            (> (length song) *mpd-collapse-all-length*))
        (concatenate 'string (subseq song 0 *mpd-collapse-all-length*) "...")
       song)))

(defun mpd-modeline (ml)
  (declare (ignore ml))
  (if *mpd-socket*
      (with-mpd-connection
       (mpd-update-status)
       (if (equal "Stopped" (mpd-get-status))
	   (format-expand *mpd-formatters-alist* *mpd-status-fmt*)
	 (progn
	   (mpd-update-current-song)
	   (format-expand *mpd-formatters-alist* *mpd-modeline-fmt*))))))

(defvar *mpd-formatters-alist*
  '((#\a mpd-get-artist)
    (#\A mpd-get-album)
    (#\d mpd-get-date)
    (#\e mpd-get-elapsed)
    (#\f mpd-get-file)
    (#\F mpd-get-xfade)
    (#\g mpd-get-genre)
    (#\l mpd-get-length)
    (#\n mpd-get-number)
    (#\p mpd-get-playlistlength)
    (#\r mpd-get-repeat)
    (#\s mpd-get-shuffle)
    (#\S mpd-get-status)
    (#\t mpd-get-title)
    (#\T mpd-get-track)
    (#\v mpd-get-volume)
    (#\m mpd-get-song-name)))

(defvar *mpd-current-song-fmt* "%a
%A
%t
(%n/%p)"
  "The default value for displaying the current song.
For more information on valid formatters, please see the documentation
for `*mpd-modeline-fmt*'")

(defvar *mpd-status-fmt* "%S [%s;%r;%F]"
  "The default value for displaying the current MPD status.
For more information on valid formatters, please see the documentation
for `*mpd-modeline-fmt*'")

(defvar *mpd-modeline-fmt* "%S [%s;%r;%F]: %a - %A - %t (%n/%p)"
  "The default value for displaying MPD information on the modeline.

@table @asis
@item %%
A literal '%'
@item %a
Artist
@item %A
Album
@item %d
Date
@item %e
Elapsed time
@item %f
Filename
@item %F
'F=#' if crossfade is set, '_' otherwise
@item %g
Genre
@item %l
Song length
@item %n
Position in playlist (song Number, for a mnemonic)
@item %p
Total playlist length
@item %r
'R' if repeat is on, '_' otherwise
@item %s
'S' if shuffle is on, '_' otherwise
@item %S
'Playing' if playing, 'Paused' if paused, else 'Stopped'
@item %t
Title
@item %T
Track number (relative to the album, not the playlist)
@item %v
Volume
@end table
")

;;; ------------------------------------------------------------------
;;; Misc. commands
;;; ------------------------------------------------------------------

(defvar *mpd-volume-step* 5)

(defcommand mpd-browse-playlist () ()
  (let ((status (mpd-send-command "status")))
    (labels ((pick (options)
                   (let ((selection
                          (select-from-menu (current-screen) options
                                            "Play song in playlist"
                                            (if (equal
                                                 (assoc-value :state status)
                                                 "play")
                                                (parse-integer
                                                 (assoc-value :song status))
                                              0))))
                     (cond
                      ((null selection)
                       (throw 'stumpwm::error "Abort."))
                      (t selection)))))
      (let* ((response (mpd-send-command "playlistinfo"))
             (result (mapcar #'cadr (remove-if (lambda (item)
                                                 (not (equal :file
                                                             (first item))))
                                               response))))
          (let* ((choice (pick result))
               (song-number (position choice result)))
          (mpd-send-command (format nil "play ~d" song-number)))))))

(defcommand-alias select-song-from-playlist browse-playlist)

(defcommand mpd-browse-albums () ()
  (labels ((pick (options)
             (let ((selection
                    (select-from-menu (current-screen) options
                                      "Play album" 0)))
               (cond
                 ((null selection)
                  (throw 'stumpwm::error "Abort."))
                 (t selection)))))
    (let* ((response (mpd-send-command "list album"))
           (result (mapcar #'cadr (remove-if (lambda (item)
                                               (not (equal :album
                                                           (first item))))
                                             response))))
      (let* ((choice (pick result)))
        (mpd-search-and-add-album choice t)))))

(defcommand mpd-browse-artists () ()
  (labels ((pick (options)
             (let ((selection
                    (select-from-menu (current-screen) options
                                      "Play artists" 0)))
               (cond
                 ((null selection)
                  (throw 'stumpwm::error "Abort."))
                 (t selection)))))
    (let* ((response (mpd-send-command "list artist"))
           (result (mapcar #'cadr (remove-if (lambda (item)
                                               (not (equal :artist
                                                           (first item))))
                                             response))))
      (let* ((choice (pick result)))
        (mpd-search-and-add-artist choice t)))))

(defcommand mpd-connect () ()
  (message "~a" (init-mpd-connection)))

(defcommand mpd-disconnect () ()
  "Disconnect from mpd server"
  (with-mpd-connection
   (close *mpd-socket*)
   (setf *mpd-socket* nil)))

(defcommand mpd-kill () ()
 (mpd-send-command "kill"))

(defcommand mpd-toggle-pause () ()
  (cond
   ((equal (mpd-get-status) "Playing")
    (mpd-send-command "pause 1"))
   ((equal (mpd-get-status) "Paused")
      (mpd-send-command "pause 0"))))

(defcommand mpd-toggle-random () ()
  (if (mpd-shuffle-p)
      (mpd-send-command "random 0")
    (mpd-send-command "random 1")))

(defcommand mpd-toggle-repeat () ()
  (if (mpd-repeating-p)
      (mpd-send-command "repeat 0")
    (mpd-send-command "repeat 1")))

(defcommand mpd-play () ()
  (mpd-send-command "play"))

(defcommand mpd-play-track (track) ((:number "Track: "))
  (mpd-send-command (concat "play " (write-to-string (1- track)))))

(defcommand mpd-stop () ()
  (mpd-send-command "stop"))

(defcommand mpd-next () ()
  (mpd-send-command "next"))

(defcommand mpd-prev () ()
  (mpd-send-command "previous"))

(defcommand mpd-set-volume (vol) ((rest "Set volume to: "))
  (mpd-send-command (format nil "setvol ~a" vol)))

(defcommand mpd-volume-up () ()
  (let* ((status (mpd-send-command "status"))
         (vol (read-from-string (assoc-value :volume status))))
    (mpd-send-command (format nil "setvol ~a" (+ vol *mpd-volume-step*)))))

(defcommand mpd-volume-down () ()
  (let* ((status (mpd-send-command "status"))
         (vol (read-from-string (assoc-value :volume status))))
    (mpd-send-command (format nil "setvol ~a" (- vol *mpd-volume-step*)))))

(defcommand mpd-clear () ()
  (mpd-send-command "clear"))

(defcommand mpd-update (&optional (path nil)) ()
  (if path
      (message "~a" (mpd-format-command "update ~a" path))
      (message "~a" (mpd-send-command "update"))))

(defcommand mpd-current-song () ()
  (mpd-update-current-song)
  (mpd-update-status)
  (message "~a"
	   (format-expand *mpd-formatters-alist* *mpd-current-song-fmt*)))

(defcommand mpd-status () ()
  (mpd-update-status)
  (mpd-update-current-song)
  (message "~a"
	   (format-expand *mpd-formatters-alist* *mpd-status-fmt*)))

(defcommand mpd-playlist () ()
  (let* ((response (mpd-send-command "playlistinfo"))
         (result (mapcar #'cadr (remove-if (lambda (item)
                                             (not (equal :file
                                                         (first item))))
                                           response))))
    (if (< (length result) 80)
        (message "Current playlist (~a): ~%^7*~{~a~%~}"
                 (length result)
                 result)
      (message "~a files in playlist" (length result)))))

(defcommand mpd-add-file (file) ((:rest "Add file to playlist: "))
  (mpd-send-command (format nil "add \"~a\"" file)))

;;search and add commands
(defcommand mpd-search-and-add-artist (what &optional (exact-search nil))
  ((:rest "Search & add artist to playlist: "))
  (let* ((response (mpd-search "artist" what exact-search))
         (result (mapcar #'cadr (remove-if (lambda (item)
                                             (not (equal :file
                                                         (first item))))
                                           response))))
    (mpd-add result)
    (if (< (length result) 80)
        (message "Added ~a files: ~%^7*~{~a~%~}"
                 (length result)
                 result)
      (message "~a files added" (length result)))))

(defcommand mpd-search-and-add-file (what &optional (exact-search nil))
  ((:rest "Search & add file to playlist: "))
  (let* ((response (mpd-search "file" what exact-search))
         (result (mapcar #'cadr (remove-if (lambda (item)
                                             (not (equal :file
                                                         (first item))))
                                           response))))
    (mpd-add result)
    (if (< (length result) 80)
        (message "Added ~a files: ~%^7*~{~a~%~}"
                 (length result)
                 result)
      (message "~a files added" (length result)))))

(defcommand mpd-search-and-add-title (what &optional (exact-search nil))
  ((:rest "Search & add title to playlist: "))
  (let* ((response (mpd-search "title" what exact-search))
         (result (mapcar #'cadr (remove-if (lambda (item)
                                             (not (equal :file
                                                         (first item))))
                                           response))))
    (mpd-add result)
    (if (< (length result) 80)
        (message "Added ~a files: ~%^7*~{~a~%~}"
                 (length result)
                 result)
      (message "~a files added" (length result)))))

(defcommand mpd-search-and-add-album (what &optional (exact-search nil))
  ((:rest "Search & add album to playlist: "))
  (let* ((response (mpd-search "album" what exact-search))
         (result (mapcar #'cadr (remove-if (lambda (item)
                                             (not (equal :file
                                                         (first item))))
                                           response))))
    (mpd-add result)
    (if (< (length result) 80)
        (message "Added ~a files: ~%^7*~{~a~%~}"
                 (length result)
                 result)
      (message "~a files added" (length result)))))

(defcommand mpd-search-and-add-genre (what &optional (exact-search nil))
  ((:rest "Search & add genre to playlist: "))
  (let* ((response (mpd-search "genre" what exact-search))
         (result (mapcar #'cadr (remove-if (lambda (item)
                                             (not (equal :file
                                                         (first item))))
                                           response))))
    (mpd-add result)
    (if (< (length result) 80)
        (message "Added ~a files: ~%^7*~{~a~%~}"
                 (length result)
                 result)
      (message "~a files added" (length result)))))

;;search commands
(defcommand mpd-search-artist (what) ((:rest "Search artist: "))
  (mpd-send-command (format nil "search artist \"~a\"" what)))
(defcommand mpd-search-file (what) ((:rest "Search file: "))
  (mpd-send-command (format nil "search file \"~a\"" what)))
(defcommand mpd-search-title (what) ((:rest "Search title: "))
  (mpd-send-command (format nil "search title \"~a\"" what)))
(defcommand mpd-search-album (what) ((:rest "Search album: "))
  (mpd-send-command (format nil "search album \"~a\"" what)))
(defcommand mpd-search-genre (what) ((:rest "Search genre: "))
  (mpd-send-command (format nil "search genre \"~a\"" what)))

;;Key map
;;FIXME: maybe some inferior mode would be a good idea (see resize in user.lisp)
(setf *mpd-search-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "a") "mpd-search-artist")
        (define-key m (kbd "A") "mpd-search-album")
        (define-key m (kbd "t") "mpd-search-title")
        (define-key m (kbd "f") "mpd-search-file")
        (define-key m (kbd "g") "mpd-search-genre")
        m))

(setf *mpd-browse-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "p") "mpd-browse-playlist")
        (define-key m (kbd "l") "mpd-browse-albums")
        (define-key m (kbd "a") "mpd-browse-artists")
        m))

(setf *mpd-add-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "a") "mpd-search-and-add-artist")
        (define-key m (kbd "A") "mpd-search-and-add-album")
        (define-key m (kbd "t") "mpd-search-and-add-title")
        (define-key m (kbd "f") "mpd-search-and-add-file")
        (define-key m (kbd "g") "mpd-search-and-add-genre")
        (define-key m (kbd "F") "mpd-add-file")
        m))

(setf *mpd-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "SPC") "mpd-toggle-pause")
        (define-key m (kbd "s") "mpd-toggle-random")
        (define-key m (kbd "r") "mpd-toggle-repeat")
        (define-key m (kbd "S") "mpd-current-song")
        (define-key m (kbd "p") "mpd-play")
        (define-key m (kbd "q") "mpd-browse-playlist")
        (define-key m (kbd "o") "mpd-stop")
        (define-key m (kbd "m") "mpd-next")
        (define-key m (kbd "l") "mpd-prev")
        (define-key m (kbd "c") "mpd-clear")
        (define-key m (kbd "x") "mpd-connect")
        (define-key m (kbd "k") "mpd-kill")
        (define-key m (kbd "u") "mpd-update")
        (define-key m (kbd "a") "mpd-search-and-add-artist")
        (define-key m (kbd "z") "mpd-playlist")
        (define-key m (kbd "v") "mpd-set-volume")
        (define-key m (kbd "e") "mpd-volume-up")
        (define-key m (kbd "d") "mpd-volume-down")
        (define-key m (kbd "S") '*mpd-search-map*)
        (define-key m (kbd "b") '*mpd-browse-map*)
        (define-key m (kbd "A") '*mpd-add-map*)
        m))

;;; MPD client & formatters for stumpwm
;;;
;;; Copyright 2007 Morgan Veyret.
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
;;;     (load "/path/to/mpd.lisp")
;;;
;;; In your ~/.stumpwmrc
;;;
;;; Then you can use "%m" in your mode line format.
;;; You can also use the various commands defined at the end of the file
;;;
;;; NOTES:
;;; see http://mpd.wikia.com/wiki/Protocol_Reference for full protocol
#-(or sbcl clisp) (error "unimplemented")

(in-package :stumpwm)

;;mpd client
(defparameter *mpd-socket* nil)
(defparameter *mpd-server* 
  #+clisp
  "localhost"
  #+sbcl
  #(127 0 0 1)
  )
(defparameter *mpd-port* 6600)

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

(defun mpd-connect ()
  "Connect to mpd server"
    (setq *mpd-socket*
	  #+clisp
        (handler-case (socket:socket-connect *mpd-port* *mpd-server*
                                             :element-type 'character)
                      ((or system::simple-os-error error)
                       (err)
			 (format t  "Error connecting to mpd: ~a~%" err)))
	  #+sbcl
	  (handler-case (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
			  (sb-bsd-sockets:socket-connect s *mpd-server* *mpd-port*)
			  (sb-bsd-sockets:socket-make-stream s :input t :output t :buffering :none))
			((or simple-error error) 
			 (err)
                       (format t  "Error connecting to mpd: ~a~%" err))))
  (when *mpd-socket*
    (read-line *mpd-socket*)))

(defun mpd-disconnect ()
  "Disconnect from mpd server"
  (with-mpd-connection
   (close *mpd-socket*)
   (setf *mpd-socket* nil)))


(defun mpd-send-command (cmd)
  (mpd-send cmd)
  (mpd-receive))


;;mpd formatter
(dolist (a '((#\m fmt-mpd-status)))
  (push a *screen-mode-line-formatters*))

(defparameter *mpd-current-song* nil)
(defparameter *mpd-status* nil)

(defun mpd-update-current-song ()
  (setf *mpd-current-song* (mpd-send-command "currentsong")))
(defun mpd-update-status ()
  (setf *mpd-status* (mpd-send-command "status")))

(defun format-mpd-current-song (current-song &optional (collapse-album nil) (collapse-all nil))
  (let* ((artist (assoc-value :artist current-song))
        (album (assoc-value :album current-song))
        (title (assoc-value :title current-song))
	 (file (assoc-value :file current-song))
	 (song (if (or (null artist)
            (null album)
            (null title))
        (format nil "~a" file)
		   (format nil "~a \"~a\" - ~a"
              artist
			   (if (and collapse-album *mpd-collapse-album-length* (> (length album) *mpd-collapse-album-length*))
			       (concatenate 'string (subseq album 0 *mpd-collapse-album-length*) "...")
			       album)
              title))))
    (if (and collapse-all *mpd-collapse-all-length* (> (length song) *mpd-collapse-all-length*))
	(concatenate 'string (subseq song 0 *mpd-collapse-all-length*) "...")
	song)))

(defun format-mpd-status (status)
  (let ((mpd-state (assoc-value :state status)))
     (cond
       ((equal mpd-state "play")
        (format nil "Playing [~a;~a] (~a%)"
                (if (equal (assoc-value :random *mpd-status*)
                           "0")
                    "_"
                  "S")
                (if (equal (assoc-value :repeat *mpd-status*)
                           "0")
                    "_"
                  "R")
                (assoc-value :volume *mpd-status*)))
       ((equal mpd-state "pause")
        (format nil "Paused [~a;~a]"
                (if (equal (assoc-value :random *mpd-status*)
                           "0")
                    "_"
                  "S")
                (if (equal (assoc-value :repeat *mpd-status*)
                           "0")
                    "_"
                  "R")))
       ((equal mpd-state "stop")
        (format nil "Stopped  [~a;~a]"
                (if (equal (assoc-value :random *mpd-status*)
                           "0")
                    "_"
                  "S")
                (if (equal (assoc-value :repeat *mpd-status*)
                           "0")
                    "_"
                  "R"))))))

;;FIXME: update status based on last current song time
;;FIXME: this means updating status on commands calls
(defun fmt-mpd-status (ml)
  (declare (ignore ml))
  (if *mpd-socket*
      (with-mpd-connection
       (mpd-update-status)
       (if (equal "stop" (assoc-value :state *mpd-status*))
           (format nil "~a"
                   (format-mpd-status *mpd-status*))
         (progn (mpd-update-current-song)
                (format nil "~a: ~a"
                        (format-mpd-status *mpd-status*)
                        (format-mpd-current-song *mpd-current-song* t t)))))
    "Not connected"))

;;mpd commands
(defparameter *mpd-volume-step* 5)

(define-stumpwm-command "mpd-select-song-in-playlist" ()
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

(define-stumpwm-command "mpd-connect" ()
  (message "~a" (mpd-connect)))

(define-stumpwm-command "mpd-disconnect" ()
  (mpd-disconnect))

(define-stumpwm-command "mpd-send-command" ((cmd :rest "Send mpd: "))
  (mpd-send cmd)
  (mpd-receive))

(define-stumpwm-command "mpd-toggle-pause" ()
  (let ((status (mpd-send-command "status")))
    (cond
     ((equal (assoc-value :state status)
             "play") (mpd-send-command "pause 1"))
     ((equal (assoc-value :state status)
             "pause") (mpd-send-command "pause 0")))))

(define-stumpwm-command "mpd-toggle-random" ()
  (let ((status (mpd-send-command "status")))
    (cond
     ((equal (assoc-value :random status) "0") (mpd-send-command "random 1"))
     ((equal (assoc-value :random status) "1") (mpd-send-command "random 0")))))

(define-stumpwm-command "mpd-toggle-repeat" ()
  (let ((status (mpd-send-command "status")))
    (cond
     ((equal (assoc-value :repeat status) "0") (mpd-send-command "repeat 1"))
     ((equal (assoc-value :repeat status) "1") (mpd-send-command "repeat 0")))))

(define-stumpwm-command "mpd-play" ()
  (mpd-send-command "play"))

(define-stumpwm-command "mpd-stop" ()
  (mpd-send-command "stop"))

(define-stumpwm-command "mpd-next" ()
  (mpd-send-command "next"))

(define-stumpwm-command "mpd-prev" ()
  (mpd-send-command "previous"))

(define-stumpwm-command "mpd-set-volume" ((vol :rest "Set volume to: "))
  (mpd-send-command (format nil "setvol ~a" vol)))

(define-stumpwm-command "mpd-volume-up" ()
  (let* ((status (mpd-send-command "status"))
         (vol (read-from-string (assoc-value :volume status))))
    (mpd-send-command (format nil "setvol ~a" (+ vol *mpd-volume-step*)))))

(define-stumpwm-command "mpd-volume-down" ()
  (let* ((status (mpd-send-command "status"))
         (vol (read-from-string (assoc-value :volume status))))
    (mpd-send-command (format nil "setvol ~a" (- vol *mpd-volume-step*)))))

(define-stumpwm-command "mpd-clear" ()
  (mpd-send-command "clear"))

(define-stumpwm-command "mpd-update" ()
  (message "~a" (mpd-send-command "update")))

(define-stumpwm-command "mpd-current-song" ()
  (message "~a" (format-mpd-current-song (mpd-send-command "currentsong"))))

(define-stumpwm-command "mpd-playlist" ()
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

(define-stumpwm-command "mpd-add-file" ((file :rest "Add file to playlist: "))
  (mpd-send-command (format nil "add \"~a\"" file)))
(define-stumpwm-command "mpd-search-and-add-artist"
  ((what :rest "Search & add artist to playlist: "))
  (let* ((response (mpd-send-command (format nil "search artist \"~a\"" what)))
         (result (mapcar #'cadr (remove-if (lambda (item)
                                             (not (equal :file
                                                         (first item))))
                                           response))))
    (loop for i in result
          do (mpd-send-command (format nil "add \"~a\"" i)))
    (if (< (length result) 80)
        (message "Added ~a files: ~%^7*~{~a~%~}"
                 (length result)
                 result)
      (message "~a files added" (length result)))))




(define-stumpwm-command "mpd-search-artist" ((what :rest "Search artist: "))
  (mpd-send-command (format nil "search artist \"~a\"" what)))
(define-stumpwm-command "mpd-search-file" ((what :rest "Search file: "))
  (mpd-send-command (format nil "search file \"~a\"" what)))
(define-stumpwm-command "mpd-search-title" ((what :rest "Search title: "))
  (mpd-send-command (format nil "search title \"~a\"" what)))
(define-stumpwm-command "mpd-search-album" ((what :rest "Search album: "))
  (mpd-send-command (format nil "search album \"~a\"" what)))
(define-stumpwm-command "mpd-search-genre" ((what :rest "Search genre: "))
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

(setf *mpd-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "SPC") "mpd-toggle-pause")
        (define-key m (kbd "s") "mpd-toggle-random")
        (define-key m (kbd "r") "mpd-toggle-repeat")
        (define-key m (kbd "p") "mpd-play")
        (define-key m (kbd "o") "mpd-stop")
        (define-key m (kbd "m") "mpd-next")
        (define-key m (kbd "l") "mpd-prev")
        (define-key m (kbd "c") "mpd-clear")
        (define-key m (kbd "u") "mpd-update")
        (define-key m (kbd "a") "mpd-search-and-add-artist")
        (define-key m (kbd "q") "mpd-select-song-in-playlist")
        (define-key m (kbd "z") "mpd-playlist")
        (define-key m (kbd "v") "mpd-set-volume")
        (define-key m (kbd "e") "mpd-volume-up")
        (define-key m (kbd "d") "mpd-volume-down")
        (define-key m (kbd "S") '*mpd-search-map*)
        m))
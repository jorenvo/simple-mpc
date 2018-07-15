;;; simple-mpc.el --- provides a simple interface to mpc

;; Copyright (C) 2015,2016 Joren Van Onder <joren.vanonder@gmail.com>
;; Copyright (C) 2016 Andriy Kmit' <dev@madand.net>

;; Author: Joren Van Onder <joren.vanonder@gmail.com>
;; Maintainer: Joren Van Onder <joren.vanonder@gmail.com>
;; URL: https://github.com/jorenvo/simple-mpc
;; Keywords: multimedia, mpd, mpc
;; Version: 1.0
;; Package-Requires: ((s "1.10.0"))

;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; See README.org

;;; Code:

(require 's)

(require 'simple-mpc-mode)
(require 'simple-mpc-current-playlist)
(require 'simple-mpc-query)
(require 'simple-mpc-vars)

(defun simple-mpc-quit ()
  "Quits simple-mpc."
  (interactive)
  (mapc (lambda (buf)
          (if (buffer-live-p (get-buffer buf))
              (kill-buffer buf)))
        (list simple-mpc-main-buffer-name
              simple-mpc-current-playlist-buffer-name
              simple-mpc-query-buffer-name)))

(defun simple-mpc-toggle ()
  (interactive)
  (simple-mpc-call-mpc nil "toggle"))

(defun simple-mpc-next ()
  (interactive)
  (simple-mpc-call-mpc nil "next")
  (simple-mpc-maybe-refresh-playlist t))

(defun simple-mpc-prev ()
  (interactive)
  (simple-mpc-call-mpc nil "prev")
  (simple-mpc-maybe-refresh-playlist t))

(defun simple-mpc-seek-forward ()
  "Does a relative seek forward by `simple-mpc-seek-time-in-s' seconds."
  (interactive)
  (simple-mpc-seek-internal simple-mpc-seek-time-in-s))

(defun simple-mpc-seek-backward ()
  "Does a relative seek backward by -`simple-mpc-seek-time-in-s' seconds."
  (interactive)
  (simple-mpc-seek-internal (- simple-mpc-seek-time-in-s)))

(defun simple-mpc-seek-internal (time-in-seconds)
  (let ((time-string (simple-mpc-convert-number-to-relative-string time-in-seconds)))
    (simple-mpc-call-mpc nil (list "seek" time-string))))

(defun simple-mpc-increase-volume ()
  "Increases volume by `simple-mpc-volume-step-size'."
  (interactive)
  (simple-mpc-modify-volume-internal simple-mpc-volume-step-size))

(defun simple-mpc-decrease-volume ()
  "Decreases volume by `simple-mpc-volume-step-size'."
  (interactive)
  (simple-mpc-modify-volume-internal (- simple-mpc-volume-step-size)))

(defun simple-mpc-modify-volume-internal (volume-change)
  (let ((volume-change-string (simple-mpc-convert-number-to-relative-string volume-change)))
    (simple-mpc-call-mpc nil (list "volume" volume-change-string)))
  (simple-mpc-message-current-volume))

(defun simple-mpc-clear-current-playlist ()
  (interactive)
  (simple-mpc-call-mpc nil "clear")
  (message "%s" "Cleared current playlist.")
  (simple-mpc-maybe-refresh-playlist))

(defun simple-mpc-shuffle-current-playlist ()
  (interactive)
  (simple-mpc-call-mpc nil "shuffle")
  (message "%s" "Shuffled current playlist.")
  (simple-mpc-maybe-refresh-playlist))

(defun simple-mpc-load-playlist (playlist-name)
  "Load an MPD playlist. Provides completion for playlists
through the lsplaylists command."
  (interactive
   (list
    (completing-read "Playlist: " (simple-mpc-call-mpc-strings "lsplaylists"))))
  (message "%s %s" "Loading playlist" playlist-name)
  (simple-mpc-call-mpc nil (list "load" playlist-name))
  (simple-mpc-maybe-refresh-playlist))

;;;###autoload
(defun simple-mpc (&optional ignore-auto noconfirm)
  "Starts simple-mpc. IGNORE-AUTO and NOCONFIRM are passed by `revert-buffer'."
  (interactive)
  (let ((buf (get-buffer-create simple-mpc-main-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (propertize "* simple-mpc *\n\n"
                          'face 'simple-mpc-main-name)
              (propertize "   * controls\n" 'face 'simple-mpc-main-headers)
              "      * [t]oggle\n"
              "      * [n]ext track\n"
              "      * [p]revious track\n"
              "      * seek [f]orward\n"
              "      * seek [b]ackward\n"
              "      * increase [V]olume\n"
              "      * decrease [v]olume\n"
              (propertize "\n   * playlist\n" 'face 'simple-mpc-main-headers)
              "      * view [c]urrent playlist\n"
              "      * [C]lear current playlist\n"
              "      * [S]huffle playlist\n"
              "      * [l]oad playlist\n"
              "      * [s]earch database\n"
              (propertize "\n   * misc\n" 'face 'simple-mpc-main-headers)
              "      * [q]uit")
      (simple-mpc-mode) ; start major mode
      (switch-to-buffer buf))))

(provide 'simple-mpc)
;;; simple-mpc.el ends here

;;; simple-mpc.el -- part of simple-mpc, providing a simple interface to mpc
;;
;; Copyright (C) 2015

;; Author: Joren Van Onder <joren.vanonder@gmail.com>
;; Maintainer: Joren Van Onder <joren.vanonder@gmail.com>
;; Keywords: multimedia, mpd, mpc
;; Version: 1.0

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

(require 'simple-mpc-current-playlist)
(require 'simple-mpc-query)
(require 'simple-mpc-vars)

(defvar simple-mpc-main-buffer-name "*simple-mpc-main*"
  "Name of the simple-mpc buffer.")
(defvar simple-mpc-current-playlist-buffer-name "*simple-mpc-current-playlist*"
  "Name of the simple-mpc buffer for the current playlist.")
(defvar simple-mpc-query-buffer-name "*simple-mpc-query*"
  "Name of the simple-mpc query buffer.")

(defvar simple-mpc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "t" 'simple-mpc-toggle)
    (define-key map "n" 'simple-mpc-next)
    (define-key map "p" 'simple-mpc-prev)
    (define-key map "f" 'simple-mpc-seek-forward)
    (define-key map "b" 'simple-mpc-seek-backward)
    (define-key map "c" 'simple-mpc-view-current-playlist)
    (define-key map "C" 'simple-mpc-clear-current-playlist)
    (define-key map "l" 'simple-mpc-load-playlist)
    (define-key map "s" 'simple-mpc-query)
    (define-key map "q" 'simple-mpc-quit)
    map)
  "Keymap for the *simple-mpc-main*.")

(define-derived-mode simple-mpc-mode special-mode "simple-mpc"
  "Major mode for the simple-mpc screen.
\\{simple-mpc-mode-map}."
  (use-local-map simple-mpc-mode-map)
  (setq truncate-lines t
        overwrite-mode 'overwrite-mode-binary)
  (set (make-local-variable 'revert-buffer-function) #'simple-mpc))

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
  (call-mpc nil "toggle"))

(defun simple-mpc-next ()
  (interactive)
  (call-mpc nil "next"))

(defun simple-mpc-prev ()
  (interactive)
  (call-mpc nil "prev"))

(defun simple-mpc-seek-forward ()
  "Does a relative seek forward by `simple-mpc-seek-time-in-s' seconds."
  (interactive)
  (simple-mpc-seek-internal simple-mpc-seek-time-in-s))

(defun simple-mpc-seek-backward ()
  "Does a relative seek backward by -`simple-mpc-seek-time-in-s' seconds."
  (interactive)
  (simple-mpc-seek-internal (- simple-mpc-seek-time-in-s)))

(defun simple-mpc-seek-internal (time-in-seconds)
  (let ((time-string (number-to-string time-in-seconds)))
    (if (> time-in-seconds 0)
	(setq time-string (concat "+" time-string)))
    (call-mpc nil "seek" time-string)))

(defun simple-mpc-clear-current-playlist ()
  (interactive)
  (call-mpc nil "clear")
  (message "%s" "Cleared current playlist."))

(defun simple-mpc-load-playlist (playlist-name)
  "Load an MPD playlist. Provides completion for playlists stored
in variable `simple-mpc-mpd-playlist-directory'."
  (interactive
   (list
    (completing-read "Playlist: "
		     (mapcar 'file-name-sans-extension
			     (directory-files simple-mpc-mpd-playlist-directory nil "[a-zA-Z]+")))))
  (message "%s %s" "Loading playlist" playlist-name)
  (call-mpc nil "load" playlist-name))

;;;###autoload
(defun simple-mpc ()
  "Starts simple-mpc."
  (interactive)
  (let ((buf (get-buffer-create simple-mpc-main-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (propertize "* simple-mpc mode *\n\n"
			  'face 'simple-mpc-main-name)
	      (propertize "   * controls\n" 'face 'simple-mpc-main-headers)
	      "      * [t]oggle\n"
	      "      * [n]ext track\n"
	      "      * [p]revious track\n"
	      "      * seek [f]orward\n"
	      "      * seek [b]ackward\n"
	      (propertize "\n   * playlist\n" 'face 'simple-mpc-main-headers)
	      "      * view [c]urrent playlist\n"
	      "      * [C]lear current playlist\n"
	      "      * [l]oad playlist\n"
	      "      * [s]earch database\n"
	      (propertize "\n   * misc\n" 'face 'simple-mpc-main-headers)
	      "      * [q]uit")
      (simple-mpc-mode) ; start major mode
      (switch-to-buffer buf))))

(provide 'simple-mpc)

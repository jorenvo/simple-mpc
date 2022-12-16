;;; simple-mpc-utils.el --- part of simple-mpc
;;
;; Copyright (C) 2015,2016 Joren Van Onder <joren@jvo.sh>
;; Copyright (C) 2020 Sean Farley <sean@farley.io>
;; Copyright (C) 2022 Joseph Turner <joseph@breatheoutbreathe.in>

;; Author: Joren Van Onder <joren@jvo.sh>
;; Maintainer: Joren Van Onder <joren@jvo.sh>
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

;;; Code:

(require 'simple-mpc-vars)

(defconst simple-mpc-status-re
  (rx
   (group (one-or-more not-newline))          ; Artist - Song title
   "\n"
   (group "[" (or "playing" "paused") "]")    ; Playing/paused status
   (one-or-more blank)
   "#" (group (one-or-more digit))            ; Playlist position
   "/"
   (group (one-or-more digit))                ; Playlist length
   (one-or-more blank)
   (group (one-or-more not-newline))          ; Song position
   "\n"
   (group "volume:" (zero-or-one blank) (one-or-more digit) "%") ; Volume
   (one-or-more blank)
   (group "repeat: " (or "on" "off"))         ; Repeat status
   (one-or-more blank)
   (group "random: " (or "on" "off"))         ; Random status
   (one-or-more blank)
   (group "single: " (or "on" "off"))         ; Single status
   (one-or-more blank)
   (group "consume: " (or "on" "off")))       ; Consume status
  "Regular expression to capture parts of `mpc status'. This regex will not match if `mpd' is not running or if the playlist is empty.")

(defconst simple-mpc-status-re-groups
  '((artist-song       . 1)
    (playing-paused    . 2)
    (playlist-position . 3)
    (playlist-length   . 4)
    (song-position     . 5)
    (volume            . 6)
    (repeat            . 7)
    (random            . 8)
    (single            . 9)
    (consume           . 10))
  "Alist mapping mpc data types to `simple-mpc-status-re' capture group.")

(defvar simple-mpc-arguments ""
  "Extra arguments that will be given to mpc.

This can be used to eg. make mpc connect to a UNIX socket with
--host.")

(defun simple-mpc-call-mpc (destination mpc-args)
  "Call mpc with `call-process'.

DESTINATION will be passed to `call-process' and MPC-ARGS will be
passed to the mpc program."
  (if (not (listp mpc-args))
      (setq mpc-args (list mpc-args)))
  (if (> (length simple-mpc-arguments) 0)
      (setq mpc-args (append (split-string simple-mpc-arguments " ") mpc-args)))
  (apply 'call-process "mpc" nil destination nil mpc-args))

(defun simple-mpc-call-mpc-strings (mpc-args)
  "Wrapper around `simple-mpc-call-mpc-string'.

Passes MPC-ARGS to `simple-mpc-call-mpc-string' and returns the
output as a list of strings created by splitting the output on
newlines."
  (split-string (simple-mpc-call-mpc-string mpc-args) "\n" t))

(defun simple-mpc-call-mpc-string (mpc-args)
  "Wrapper around `simple-mpc-call-mpc'.

Passes MPC-ARGS to `simple-mpc-call-mpc' and returns the
output as a string."
  (with-temp-buffer
    (simple-mpc-call-mpc t mpc-args)
    (buffer-string)))

(defun simple-mpc-extract-status (type)
  "Return the mpc data corresponding to TYPE of current song. TYPE may be one of the keys of `simple-mpc-status-re-groups'."
  (with-temp-buffer
    (simple-mpc-call-mpc t "status")
    (let ((status (buffer-string)))
      (string-match simple-mpc-status-re status)
      (match-string (alist-get type simple-mpc-status-re-groups) status))))

(defun simple-mpc-goto-line (line-number)
  "Go to beginning of line LINE-NUMBER.

Safe to be called from a Lisp program."
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun simple-mpc-switch-to-main-buffer ()
  "Switch to the main mpc buffer."
  (if (get-buffer simple-mpc-main-buffer-name)
      (switch-to-buffer simple-mpc-main-buffer-name)))

(defun simple-mpc-format-as-table (result)
  "Format RESULT as a table."
  (if simple-mpc-table-separator
      (with-temp-buffer
        (insert
         ;; Old versions of column (included in util-linux
         ;; <2.23) merge multiple adjacent delimiters into a
         ;; single delimiter. Make sure this doesn't occur by
         ;; inserting a "/" in those empty columns.
         (replace-regexp-in-string
          (concat simple-mpc-table-separator simple-mpc-table-separator)
          (concat simple-mpc-table-separator "/" simple-mpc-table-separator)
          result))
        (shell-command-on-region (point-min) (point-max) (format "column -ts '%s'" simple-mpc-table-separator) nil t)
        (buffer-string))
    result))

(defun simple-mpc-get-playlist-format ()
  "Return `simple-mpc-playlist-format' or %file%, if blank."
  (if (string= simple-mpc-playlist-format "")
      "%file%"
    simple-mpc-playlist-format))

(defun simple-mpc-convert-number-to-relative-string (number)
  "Convert an integer NUMBER to a prefixed string.

The prefix is either - or +. This is useful for mpc commands
like volume and seek."
  (let ((number-string (number-to-string number)))
    (if (> number 0)
        (concat "+" number-string)
      number-string)))

(provide 'simple-mpc-utils)
;;; simple-mpc-utils.el ends here

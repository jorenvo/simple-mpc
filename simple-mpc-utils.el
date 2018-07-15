;;; simple-mpc-utils.el --- part of simple-mpc
;;
;; Copyright (C) 2015,2016 Joren Van Onder <joren.vanonder@gmail.com>

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

;;; Code:

(require 'simple-mpc-vars)

(defvar simple-mpc-arguments ""
  "Extra arguments that will be given to mpc. This can be used to
eg. make mpc connect to a UNIX socket with --host.")

(defun simple-mpc-call-mpc (destination mpc-args)
  "Calls mpc with `call-process'. DESTINATION will be passed to
`call-process' and MPC-ARGS will be passed to the mpc program."
  (if (not (listp mpc-args))
      (setq mpc-args (list mpc-args)))
  (if (> (length simple-mpc-arguments) 0)
      (setq mpc-args (append (split-string simple-mpc-arguments " ") mpc-args)))
  (apply 'call-process "mpc" nil destination nil mpc-args))

(defun simple-mpc-call-mpc-strings (mpc-args)
  "Wrapper around `simple-mpc-call-mpc'. Returns the output from
it as a list of strings created by splitting the output on
newlines."
  (split-string (simple-mpc-call-mpc-string mpc-args) "\n" t))

(defun simple-mpc-call-mpc-string (mpc-args)
  "Wrapper around `simple-mpc-call-mpc'. Returns the output from
it as a string."
  (with-temp-buffer
    (simple-mpc-call-mpc t mpc-args)
    (buffer-string)))

(defun simple-mpc-get-current-playlist-position ()
  (with-temp-buffer
    (simple-mpc-call-mpc t '("current" "-f" "%position%"))
    (string-to-number (buffer-string))))

(defun simple-mpc-get-amount-of-songs-in-playlist ()
  (with-temp-buffer
    (simple-mpc-call-mpc t "playlist")
    (count-lines (point-min) (point-max))))

(defun simple-mpc-message-current-volume ()
  ;; Use "%s" as format-string. Otherwise message will
  ;; interpret the percent symbol in mpc's output as an
  ;; incomplete format specifier.
  (message "%s"
   (with-temp-buffer
     (simple-mpc-call-mpc t "volume")
     (delete-char -1)  ;; delete trailing \n
     (buffer-string))))

(defun simple-mpc-goto-line (line-number)
  "Go to beginning of line LINE-NUMBER. Safe to be called from
a Lisp program."
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun simple-mpc-switch-to-main-buffer ()
  "Switches to the main mpc buffer."
  (if (get-buffer simple-mpc-main-buffer-name)
      (switch-to-buffer simple-mpc-main-buffer-name)))

(defun simple-mpc-format-as-table (result)
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
  (if (string= simple-mpc-playlist-format "")
      "%file%"
    simple-mpc-playlist-format))

(defun simple-mpc-convert-number-to-relative-string (number)
  "Converts an integer NUMBER to a string prefixed with either -
or +. This is useful for mpc commands like volume and seek."
  (let ((number-string (number-to-string number)))
    (if (> number 0)
	(concat "+" number-string)
      number-string)))

(provide 'simple-mpc-utils)
;;; simple-mpc-utils.el ends here

;;; simple-mpc-utils.el --- part of simple-mpc
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

;;; Code:

(require 'simple-mpc-vars)

(defun simple-mpc-partial-mpc (destination action)
  (apply-partially 'call-process "mpc" nil destination nil action))

(defun call-mpc (destination action &optional mpc-args)
  "Calls mpc with `call-process'. DESTINATION will be passed to
`call-process' and MPC-ARGS will be applied to it."
  (if (listp mpc-args)
      (apply (simple-mpc-partial-mpc destination action) mpc-args)
    (funcall (simple-mpc-partial-mpc destination action) mpc-args)))

(defun simple-mpc-get-current-playlist-position ()
  (with-temp-buffer
    (call-mpc t "current" (list "-f" "%position%"))
    (string-to-number (buffer-string))))

(defun simple-mpc-get-amount-of-songs-in-playlist ()
  (with-temp-buffer
    (call-mpc t "playlist")
    (count-lines (point-min) (point-max))))

(defun simple-mpc-goto-line (line-number)
  "Go to beginning of line LINE-NUMBER. Safe to be called from
a Lisp program."
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun simple-mpc-switch-to-main-buffer ()
  "Switches to the main mpc buffer."
  (if (get-buffer simple-mpc-main-buffer-name)
      (switch-to-buffer simple-mpc-main-buffer-name)))

(provide 'simple-mpc-utils)
;;; simple-mpc-utils.el ends here

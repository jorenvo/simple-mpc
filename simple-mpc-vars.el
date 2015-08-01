;;; simple-mpc-vars.el --- part of simple-mpc
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

(defvar simple-mpc-main-buffer-name "*simple-mpc-main*"
  "Name of the simple-mpc buffer.")
(defvar simple-mpc-current-playlist-buffer-name "*simple-mpc-current-playlist*"
  "Name of the simple-mpc buffer for the current playlist.")
(defvar simple-mpc-query-buffer-name "*simple-mpc-query*"
  "Name of the simple-mpc query buffer.")

(defgroup simple-mpc nil
  "simple-mpc - provides a simple interface to mpc"
  :group 'multimedia)

(defcustom simple-mpc-playlist-format ""
  "Format string that will be given to mpc through --format."
  :group 'simple-mpc)

(defcustom simple-mpc-mpd-playlist-directory "~/.mpd/playlists/"
  "The directory `simple-mpc-load-playlist' will look for
playlists."
  :group 'simple-mpc
  :type 'directory)

(defcustom simple-mpc-seek-time-in-s 5
  "The time in seconds that will be used to do relative seeking
with `simple-mpc-seek-forward' and `simple-mpc-seek-backward'."
  :group 'simple-mpc
  :type 'integer)

(defface simple-mpc-main-name
  '((t :inherit font-lock-type-face :bold t))
  "For the title in the main view."
  :group 'simple-mpc)

(defface simple-mpc-main-headers
  '((t :inherit font-lock-type-face))
  "For the different headers in the main view."
  :group 'simple-mpc)

(provide 'simple-mpc-vars)
;;; simple-mpc-vars.el ends here

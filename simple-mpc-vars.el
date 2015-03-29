;;; simple-mpc-vars.el -- part of simple-mpc
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

(defgroup simple-mpc nil
  "simple-mpc - provides a simple interface to mpc"
  :group 'multimedia)

(defcustom simple-mpc-mpd-playlist-directory "~/.mpd/playlists/"
  "The directory `simple-mpc-load-playlist' will look for
playlists."
  :group 'simple-mpc
  :type 'directory)

(provide 'simple-mpc-vars)

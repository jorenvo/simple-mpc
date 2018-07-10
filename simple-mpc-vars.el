;;; simple-mpc-vars.el --- part of simple-mpc
;;
;; Copyright (C) 2015,2016 Joren Van Onder <joren.vanonder@gmail.com>
;; Copyright (C) 2016 Andriy Kmit' <dev@madand.net>

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
  :group 'simple-mpc
  :type 'string)

(defcustom simple-mpc-seek-time-in-s 5
  "The time in seconds that will be used to do relative seeking
with `simple-mpc-seek-forward' and `simple-mpc-seek-backward'."
  :group 'simple-mpc
  :type 'integer)

(defcustom simple-mpc-volume-step-size 5
  "The change in volume used for e.g. `simple-mpc-increase-volume'
and `simple-mpc-decrease-volume'."
  :group 'simple-mpc
  :type 'integer)

(defcustom simple-mpc-playlist-auto-refresh nil
  "Automatic refresh of the playlist buffer.

If nil, automatic refresh is disabled.

Integer value specifies the interval in seconds. The interval is
not strict and the actual refresh may be delayed, if Emacs is
busy. See `run-with-idle-timer'."
  :group 'simple-mpc
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Number of seconds" 5)))

(defcustom simple-mpc-table-separator nil
  "When non-nil this will be used to format the playlist as a table.

This value is given directly to the -s flag of column(1). When
using this option it is important that
`simple-mpc-playlist-format' contains a format that uses this
separator.

\\t should be a good choice for this. You can insert literal tab
characters in GNU Emacs by pressing C-q <TAB>."
  :group 'simple-mpc
  :type 'string)

(defface simple-mpc-main-name
  '((t :inherit font-lock-type-face :bold t))
  "For the title in the main view."
  :group 'simple-mpc)

(defface simple-mpc-main-headers
  '((t :inherit font-lock-type-face))
  "For the different headers in the main view."
  :group 'simple-mpc)

(defface simple-mpc-current-track-face
  '((t :inherit font-lock-keyword-face :bold t))
  "For the current track in the current playlist view."
  :group 'simple-mpc)

(provide 'simple-mpc-vars)
;;; simple-mpc-vars.el ends here

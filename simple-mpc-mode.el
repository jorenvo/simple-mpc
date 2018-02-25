;;; simple-mpc-mode.el --- part of simple-mpc
;;
;; Copyright (C) 2015,2016 Joren Van Onder <joren.vanonder@gmail.com>

;; Author: Joren Van Onder <joren.vanonder@gmail.com>
;; URL: https://github.com/jorenvo/simple-mpc
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

(defvar simple-mpc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "t" 'simple-mpc-toggle)
    (define-key map "n" 'simple-mpc-next)
    (define-key map "p" 'simple-mpc-prev)
    (define-key map "f" 'simple-mpc-seek-forward)
    (define-key map "b" 'simple-mpc-seek-backward)
    (define-key map "V" 'simple-mpc-increase-volume)
    (define-key map "v" 'simple-mpc-decrease-volume)
    (define-key map "c" 'simple-mpc-view-current-playlist)  ;; autoload this
    (define-key map "C" 'simple-mpc-clear-current-playlist)
    (define-key map "S" 'simple-mpc-shuffle-current-playlist)
    (define-key map "l" 'simple-mpc-load-playlist)
    (define-key map "s" 'simple-mpc-query) ;; autoload this
    (define-key map "q" 'simple-mpc-quit)
    map)
  "Keymap for simple-mpc-mode.")

(define-derived-mode simple-mpc-mode special-mode "simple-mpc"
  "Major mode for the simple-mpc screen.
\\{simple-mpc-mode-map}."
  (setq truncate-lines t overwrite-mode 'overwrite-mode-binary)
  (set (make-local-variable 'revert-buffer-function) #'simple-mpc))

(provide 'simple-mpc-mode)
;;; simple-mpc-mode.el ends here

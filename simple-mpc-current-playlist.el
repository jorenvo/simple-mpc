;;; simple-mpc-current-playlist.el --- part of simple-mpc
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

(require 'simple-mpc-utils)

(define-minor-mode simple-mpc-current-playlist-mode
  "Minor mode for the simple-mpc-current-playlist screen.
\\{simple-mpc-current-playlist-mode-map}."
  :lighter " simple-mpc-current-playlist"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<return>") 'simple-mpc-play-current-line)
	    (define-key map "d" 'simple-mpc-delete)
	    (define-key map "q" 'simple-mpc-current-playlist-quit)
	    map)
  (set (make-local-variable 'revert-buffer-function) #'simple-mpc-view-current-playlist))

(defun simple-mpc-current-playlist-quit ()
  "Quits the current playlist mode and goes back to main."
  (interactive)
  (kill-buffer simple-mpc-current-playlist-buffer-name)
  (switch-to-buffer simple-mpc-main-buffer-name))

(defun simple-mpc-view-current-playlist (&optional ignore-auto noconfirm)
  "Views the current playlist."
  (interactive)
  (let ((buf (get-buffer-create simple-mpc-current-playlist-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (call-mpc buf "playlist")
      (simple-mpc-goto-line (simple-mpc-get-current-playlist-position))
      (switch-to-buffer buf)
      (simple-mpc-mode)
      (simple-mpc-current-playlist-mode)
      (hl-line-mode))))

(defun simple-mpc-play-current-line ()
  "Plays the song on the current line."
  (interactive)
  (call-mpc nil "play" (number-to-string (line-number-at-pos (point)))))

(defun simple-mpc-delete ()
  "Deletes the song on the current line from the playlist. When a
region is active, it deletes all the tracks in the region."
  (interactive)
  (if (use-region-p)
      (let ((first-line-region (line-number-at-pos (region-beginning)))
	    (last-line-region (1- (line-number-at-pos (region-end))))) ; usually point is on the next line so 1-
	(call-mpc nil "del" (mapcar 'number-to-string (number-sequence first-line-region
								       last-line-region))))
    (call-mpc nil "del" (number-to-string (line-number-at-pos (point)))))
  (simple-mpc-view-current-playlist))

(provide 'simple-mpc-current-playlist)
;;; simple-mpc-current-playlist.el ends here
;;; simple-mpc-current-playlist.el --- part of simple-mpc
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

(require 'simple-mpc-mode)
(require 'simple-mpc-vars)
(require 'simple-mpc-utils)

(defvar simple-mpc--playlist-refresh-timer nil)

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
  (simple-mpc-playlist-refresh-timer-stop)
  (kill-buffer simple-mpc-current-playlist-buffer-name)
  (simple-mpc-switch-to-main-buffer))

(defun simple-mpc-view-current-playlist (&optional ignore-auto noconfirm keep-point)
  "Views the current playlist.

If optional argument KEEP-POINT is t, try to keep point in its current
position. Otherwise, move it to the current track in the playlist."
  (interactive)
  (let ((buf (get-buffer-create simple-mpc-current-playlist-buffer-name)))
    (with-current-buffer buf
      (let ((original-line (line-number-at-pos))
            (original-column (current-column))
            (window-start (window-start))
            (window-hscroll (window-hscroll)))
        (read-only-mode -1)
        (erase-buffer)
        (insert
         (simple-mpc-format-as-table (simple-mpc-call-mpc-string
                                      (append (list "--format" (simple-mpc-get-playlist-format)) '("playlist")))))
        (save-excursion
          (simple-mpc-goto-line (simple-mpc-get-current-playlist-position))
          (put-text-property
           (line-beginning-position) (line-end-position)
           'face 'simple-mpc-current-track-face))
        (if keep-point
            (progn
              (simple-mpc-goto-line original-line)
              (move-to-column original-column)
              (set-window-start nil window-start t)
              (set-window-hscroll nil window-hscroll))
          (simple-mpc-goto-line (simple-mpc-get-current-playlist-position)))
        (switch-to-buffer buf)
        (simple-mpc-mode)
        (simple-mpc-current-playlist-mode)
        (hl-line-mode)
        (when simple-mpc-playlist-auto-refresh
          (simple-mpc-playlist-refresh-timer-start))))))

(defun simple-mpc-play-current-line ()
  "Plays the song on the current line."
  (interactive)
  (simple-mpc-call-mpc nil (list "play" (number-to-string (line-number-at-pos (point)))))
  (simple-mpc-view-current-playlist nil nil t))

(defun simple-mpc-delete ()
  "Deletes the song on the current line from the playlist. When a
region is active, it deletes all the tracks in the region."
  (interactive)
  (if (use-region-p)
      (let ((first-line-region (line-number-at-pos (region-beginning)))
	    (last-line-region (1- (line-number-at-pos (region-end))))) ; usually point is on the next line so 1-
	(simple-mpc-call-mpc nil (cons "del" (mapcar 'number-to-string (number-sequence first-line-region
                                                                                        last-line-region)))))
    (simple-mpc-call-mpc nil (list "del" (number-to-string (line-number-at-pos (point))))))
  (simple-mpc-view-current-playlist))

(defun simple-mpc-maybe-refresh-playlist (&optional keep-point)
  "If the current buffer is a simple mpc playlist buffer, refresh its contents.

If optional argument KEEP-POINT is t, try to keep point in its current
position. Otherwise, move it to the current track in the playlist."
  (when (string= (buffer-name) simple-mpc-current-playlist-buffer-name)
    (simple-mpc-view-current-playlist nil nil keep-point)))

(defun simple-mpc-playlist-refresh-timer-start ()
  "Start a timer that will refresh the playlist buffer every
 `simple-mpc-playlist-auto-refresh' seconds.

Do nothing if the timer is already running."
  (unless simple-mpc--playlist-refresh-timer
    (setq simple-mpc--playlist-refresh-timer (run-with-idle-timer
                                              simple-mpc-playlist-auto-refresh
                                              t
                                              (lambda ()
                                                (simple-mpc-maybe-refresh-playlist t))))))

(defun simple-mpc-playlist-refresh-timer-stop ()
  "Stop a timer that refreshes the playlist buffer."
  (when simple-mpc--playlist-refresh-timer
    (cancel-timer simple-mpc--playlist-refresh-timer)
    (setq simple-mpc--playlist-refresh-timer nil)))

(provide 'simple-mpc-current-playlist)
;;; simple-mpc-current-playlist.el ends here

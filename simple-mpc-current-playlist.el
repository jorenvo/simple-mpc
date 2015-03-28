(require 'simple-mpc-utils "~/code/github/simple-mpc/simple-mpc-utils.el")

(setq simple-mpc-current-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'simple-mpc-current-playlist-quit)
    (define-key map (kbd "<return>") 'simple-mpc-play-current-line)
    (define-key map "d" 'simple-mpc-delete)
    map))

(define-derived-mode simple-mpc-current-playlist-mode special-mode "simple-mpc-current-playlist"
  "Major mode for the simple-mpc-current-playlist screen.
\\{simple-mpc-current-playlist-mode-map}."
  (use-local-map simple-mpc-current-playlist-mode-map)
  (setq truncate-lines t
        overwrite-mode 'overwrite-mode-binary)
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
      (call-mpc nil "playlist")
      (goto-line (simple-mpc-get-current-playlist-position))
      (switch-to-buffer buf)
      (simple-mpc-current-playlist-mode))))

(defun simple-mpc-play-current-line ()
  "Plays the song on the current line."
  (interactive)
  (call-mpc "play" (number-to-string (line-number-at-pos (point)))))

(defun simple-mpc-delete ()
  "Deletes the song on the current line from the playlist. When a
region is active, it deletes all the tracks in the region."
  (interactive)
  (if (use-region-p)
      (let ((first-line-region (line-number-at-pos (region-beginning)))
	    (last-line-region (1- (line-number-at-pos (region-end))))) ; usually point is on the next line so 1-
	(call-mpc nil "del" (mapcar 'number-to-string (number-sequence first-line-region last-line-region))))
    (call-mpc nil "del" (number-to-string (line-number-at-pos (point)))))
  (simple-mpc-view-current-playlist))

(provide 'simple-mpc-current-playlist)

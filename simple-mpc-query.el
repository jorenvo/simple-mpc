(require 'simple-mpc-utils "~/code/github/simple-mpc/simple-mpc-utils.el")

(setq simple-mpc-query-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'simple-mpc-query-quit)
    (define-key map (kbd "<return>") (lambda () (interactive) (simple-mpc-query-add)))
    (define-key map (kbd "<S-return>") (lambda () (interactive) (simple-mpc-query-add t)))
    map))

(define-derived-mode simple-mpc-query-mode special-mode "simple-mpc-query"
  "Major mode for the simple-mpc-query screen.
\\{simple-mpc-query-mode-map}."
  (use-local-map simple-mpc-query-mode-map)
  (setq truncate-lines t
        overwrite-mode 'overwrite-mode-binary))

(defun simple-mpc-query (search-type search-query)
  "Perform an mpc search. SEARCH-TYPE is a tag type, SEARCH-QUERY
is the actual query."
  (interactive
   (list
    (completing-read "Search type: " '("artist" "album" "title" "track"
				       "name" "genre" "date" "composer"
				       "performer" "comment" "disc" "filename"
				       "any"))
    (read-string "Query: ")))
  (let ((buf (get-buffer-create simple-mpc-query-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (call-mpc t "search" (list search-type search-query))
      (beginning-of-buffer)
      (simple-mpc-query-mode)
      (switch-to-buffer buf))))

(defun simple-mpc-query-add (&optional play)
  "Add the song on the current line to the current playlist. When
a region is active, add all the songs in the region to the
current playlist. When PLAY is non-nil, immediately play them."
  (interactive)
  (if (use-region-p)
      (let ((first-line-region (line-number-at-pos (region-beginning)))
	    (last-line-region (1- (line-number-at-pos (region-end))))) ; usually point is on the next line so 1-
	(insert "do region"))
    (call-mpc nil "add" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun simple-mpc-query-quit ()
  "Quits the current playlist mode and goes back to main."
  (interactive)
  (kill-buffer simple-mpc-query-buffer-name)
  (switch-to-buffer simple-mpc-main-buffer-name))

(provide 'simple-mpc-query)

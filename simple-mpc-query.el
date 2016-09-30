;;; simple-mpc-query.el --- part of simple-mpc
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

(require 'simple-mpc-mode)
(require 'simple-mpc-vars)
(require 'simple-mpc-utils)

(defvar simple-mpc-query-current-result-alist nil
  "An association list containing a (`simple-mpc-playlist-format'
. %file%) pair for every result in the latest query.")

(define-minor-mode simple-mpc-query-mode
  "Minor mode for the simple-mpc-query screen.
\\{simple-mpc-query-mode-map}."
  :lighter " simple-mpc-query-mode"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map "q" 'simple-mpc-query-quit)
	    (define-key map "S" 'simple-mpc-query-sort)
	    (define-key map (kbd "<return>") 'simple-mpc-query-add)
	    (define-key map (kbd "<S-return>") 'simple-mpc-query-add-and-play)
	    map)
  (set (make-local-variable 'require-final-newline) nil))

(defun simple-mpc-query-quit ()
  "Quits the current playlist mode and goes back to main."
  (interactive)
  (kill-buffer simple-mpc-query-buffer-name)
  (simple-mpc-switch-to-main-buffer))

(defun simple-mpc-query-get-%file%-for-result (result)
  (cdr (assoc result simple-mpc-query-current-result-alist)))

(defun simple-mpc-query-build-result-alist (search-type search-query)
  "Builds `simple-mpc-query-current-result-alist' according to
SEARCH-TYPE and SEARCH-QUERY."
  (setq simple-mpc-query-current-result-alist
        (let* ((file-metadata-delimiter "(simple-mpc)")
               (query-format (concat (simple-mpc-get-playlist-format) file-metadata-delimiter "%file%" file-metadata-delimiter)))
          (mapcar (lambda (mpc-result)
                    (let* ((matches (s-match (format "^\\(.*\\)%s\\(.*\\)%s" file-metadata-delimiter file-metadata-delimiter) mpc-result))
                           (full-match (nth 0 matches))
                           (user-specified-format (nth 1 matches))
                           (file (nth 2 matches)))
                      (cons user-specified-format file)))
                  (split-string
                   (simple-mpc-format-as-table (simple-mpc-call-mpc-string
                                                (list "--format" query-format "search" search-type search-query)))
                   "\n" t)))))

(defun simple-mpc-query (search-type search-query)
  "Perform an mpc search. SEARCH-TYPE is a tag type, SEARCH-QUERY
is the actual query."
  (interactive
   (list
    (completing-read "Search type: " '("artist" "album" "title" "track"
				       "name" "genre" "date" "composer"
				       "performer" "comment" "disc" "filename"
				       "any") nil t)
    (read-string "Query: ")))
  (simple-mpc-query-build-result-alist search-type search-query)
  (let ((buf (get-buffer-create simple-mpc-query-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (mapconcat (lambda (result) (car result)) simple-mpc-query-current-result-alist "\n"))
      (goto-char (point-max))
      (goto-char (point-min))
      (simple-mpc-mode)
      (simple-mpc-query-mode)
      (hl-line-mode)
      (switch-to-buffer buf))))

(defun simple-mpc-query-add-and-play ()
  "Wrapper for (`simple-mpc-query-add' t)."
  (interactive)
  (simple-mpc-query-add t))

(defun simple-mpc-query-add (&optional play)
  "Add the song on the current line to the current playlist. When
a region is active, add all the songs in the region to the
current playlist. When PLAY is non-nil, immediately play them."
  (interactive)
  (let ((current-amount-in-playlist (simple-mpc-get-amount-of-songs-in-playlist)))
    (if (use-region-p)
	(let ((first-line-region (line-number-at-pos (region-beginning)))
	      (last-line-region (if (eq (region-end) (point-max))
				    (line-number-at-pos (region-end))
				  (1- (line-number-at-pos (region-end))))) ; usually point is on the next line so 1-
	      (beginning-first-line-region)
	      (end-last-line-region))
	  (save-excursion
	    (simple-mpc-goto-line first-line-region)
	    (setq beginning-first-line-region (line-beginning-position))
	    (simple-mpc-goto-line last-line-region)
	    (setq end-last-line-region (line-end-position)))
	  (simple-mpc-call-mpc nil (cons "add" (mapcar (lambda (result)
                                                         (simple-mpc-query-get-%file%-for-result result))
                                                       (split-string (buffer-substring-no-properties beginning-first-line-region
                                                                                                     end-last-line-region)
                                                                     "\n" t))))
          (deactivate-mark))
      (simple-mpc-call-mpc nil (list "add" (simple-mpc-query-get-%file%-for-result
                                            (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
      (forward-line))
    (if play
	(simple-mpc-call-mpc nil (list "play" (number-to-string (1+ current-amount-in-playlist)))))))

(defun simple-mpc-query-sort (&optional reverse)
  "Sorts all query results alphabetically."
  (interactive)
  (read-only-mode -1)
  (sort-lines reverse (point-min) (point-max))
  (read-only-mode 1))

(provide 'simple-mpc-query)
;;; simple-mpc-query.el ends here

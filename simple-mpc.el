(require 'simple-mpc-current-playlist "~/code/github/simple-mpc/simple-mpc-current-playlist.el")

(defconst simple-mpc-main-buffer-name "*simple-mpc-main*"
  "*internal* Name of the simple-mpc buffer.")
(defconst simple-mpc-current-playlist-buffer-name "*simple-mpc-current-playlist*"
  "*internal* Name of the simple-mpc buffer for the current playlist.")

;; `setq' always assigns a value. `defvar' assigns a value only the
;; first time it is evaluated, normally, bu
(setq simple-mpc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'simple-mpc-quit)
    (define-key map "c" 'simple-mpc-view-current-playlist)
    map))

(define-derived-mode simple-mpc-mode special-mode "simple-mpc"
  "Major mode for the simple-mpc screen.
\\{simple-mpc-mode-map}."
  (use-local-map simple-mpc-mode-map)
  (setq truncate-lines t
        overwrite-mode 'overwrite-mode-binary)
  (set (make-local-variable 'revert-buffer-function) #'simple-mpc))

(defun simple-mpc-quit ()
  "Quits simple-mpc."
  (interactive)
  (mapc 'kill-buffer
	(list simple-mpc-main-buffer-name simple-mpc-current-playlist-buffer-name)))

;;;###autoload
(defun simple-mpc ()
  "Starts simple-mpc."
  (interactive)
  (let ((buf (get-buffer-create simple-mpc-main-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "* simple-mpc mode *\n\n"
	      "\t* view [c]urrent playlist\n"
	      "\t* do [Q]uery\n"
	      "\n\t* [q]uit")
      (simple-mpc-mode) ; start major mode
      (switch-to-buffer buf))))

(provide 'simple-mpc)

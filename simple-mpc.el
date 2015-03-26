(defconst simple-mpc-main-buffer-name "*simple-mpc*"
  "*internal* Name of the simple-mpc buffer.")

(defvar simple-mpc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'simple-mpc-quit)
    map) "Keymap for the *simple-mpc* buffer.")

(define-derived-mode simple-mpc-mode special-mode "simple-mpc"
  "Major mode for the simple-mpc screen.
\\{simple-mpc-mode-map}."
  (use-local-map simple-mpc-mode-map)
  (setq truncate-lines t
        overwrite-mode 'overwrite-mode-binary)
  (set (make-local-variable 'revert-buffer-function) #'simple-mpc-main))

(defun simple-mpc-quit ()
  "Quits simple-mpc."
  (interactive)
  (kill-buffer simple-mpc-main-buffer-name))

(defun simple-mpc-main ()
  "Starts simple-mpc."
  (interactive)
  (let ((buf (get-buffer-create simple-mpc-main-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "* simple-mpc mode *\n\n")
      (simple-mpc-mode) ; start major mode
      (switch-to-buffer buf))))

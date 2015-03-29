(defun partial-mpc (destination action)
  (apply-partially 'call-process "mpc" nil destination nil action))

(defun call-mpc (destination action &optional mpc-args)
  "Calls mpc with `call-process'. DESTINATION will be passed to
`call-process' and MPC-ARGS will be applied to it."
  (if (listp mpc-args)
      (apply (partial-mpc destination action) mpc-args)
    (funcall (partial-mpc destination action) mpc-args)))

(defun simple-mpc-get-current-playlist-position ()
  (with-temp-buffer
    (call-mpc t "current" (list "-f" "%position%"))
    (string-to-number (buffer-string))))

(defun simple-mpc-get-amount-of-songs-in-playlist ()
  (with-temp-buffer
    (call-mpc t "playlist")
    (count-lines (point-min) (point-max))))

(provide 'simple-mpc-utils)

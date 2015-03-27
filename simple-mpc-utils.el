(defun partial-mpc (destination action)
  (apply-partially 'call-process "mpc" nil destination nil action))

(defun call-mpc (destination action mpc-args)
  "Calls mpc with `call-process'. DESTINATION will be passed to
`call-process' and MPC-ARGS will be applied to it."
  (if (listp mpc-args)
      (apply (partial-mpc destination action) mpc-args)
    (funcall (partial-mpc destination action) mpc-args)))

(provide 'simple-mpc-utils)

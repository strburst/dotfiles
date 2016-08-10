;;; base-lib.el --- For functions independent of external packages

(defun base-lib/echo-init-time ()
  "Print the value of emacs-init-time to the minibuffer."
  (message "Init time: %s" (emacs-init-time)))

(defun base-lib/echo-major-mode ()
  "Print the name of the current major mode to the minibuffer."
  (interactive)
  (message "Current major mode is: %s" mode-name))

(defun base-lib/open-emacs-config ()
  "Open the .emacs.d directory."
  (interactive)
  (find-file user-emacs-directory))

(provide 'base-lib)

;;; base-lib.el ends here

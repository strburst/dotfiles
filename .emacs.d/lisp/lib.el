;;; lib.el --- For functions independent of external packages
;;; Commentary:
;;; Code:

(defun lib/echo-init-time ()
  "Print the value of `emacs-init-time' to the minibuffer."
  (interactive)
  (message "Init time: %s" (emacs-init-time)))

(defun lib/echo-major-mode ()
  "Print the name of the current major mode to the minibuffer."
  (interactive)
  (message "Current major mode is: %s" mode-name))

(defun lib/open-emacs-config ()
  "Open the .emacs.d directory."
  (interactive)
  (find-file user-emacs-directory))

(provide 'lib)

;;; lib.el ends here

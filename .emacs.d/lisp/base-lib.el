;;; base-lib.el --- For functions independent of external packages

(defun base-lib/print-major-mode ()
  "Print the name of the current major mode to the minibuffer."
  (interactive)
  (message "Current major mode is: %s" mode-name))

(defun base-lib/open-emacs-config ()
  "Open ~/.emacs.d/init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun base-lib/source-emacs-config ()
  "Load ~/.emacs.d/init.el."
  (interactive)
  (load "~/.emacs.d/init.el"))

(provide 'base-lib)

;;; base-lib.el ends here

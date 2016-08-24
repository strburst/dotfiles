;;; lib.el --- For functions independent of external packages
;;; Commentary:
;;; Code:

(defmacro lib/add-minor-mode-hooks (mode-hook-list minor-mode-fn)
  "For hooks in MODE-HOOK-LIST, call MINOR-MODE-FN to load a minor mode."
  `(dolist (hook ,mode-hook-list)
     (add-hook hook (lambda ()
                      (funcall ,minor-mode-fn 1)))))

(defun lib/config-path (path)
  "Take PATH and prepend the Emacs config directory."
  (concat user-emacs-directory path))

(defun lib/echo-init-time ()
  "Print the value of `emacs-init-time' to the minibuffer."
  (interactive)
  (message "Init time: %s" (emacs-init-time)))

(defun lib/echo-major-mode ()
  "Print the name of the current major mode to the minibuffer."
  (interactive)
  (message "Current major mode is: %s" mode-name))

(defun lib/open-emacs-config ()
  "Open the ~/.emacs.d directory."
  (interactive)
  (find-file user-emacs-directory))

(defun lib/save-and-kill ()
  "Save the current buffer if associated with a file and kill the buffer."
  (interactive)
  (save-buffer)
  (kill-this-buffer))

(defmacro lib/setq-same (value &rest vars)
  "Take VALUE and assign each VAR in VARS to VALUE."
  (let ((eval-value value))
    `(progn
       ,@(mapcar (lambda (var)
                   `(setq ,var ,eval-value))
                 vars))))

(provide 'lib)

;;; lib.el ends here

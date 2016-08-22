;;; ido-use.el --- Intelligent auto-completion (builtin package)
;;; Commentary:
;;; Code:

(require 'key)

(use-package ido
  :init
  (setq ido-enable-flex-matching t
        ido-everywhere t)

  :config
  (ido-mode 1)
  (ido-everywhere 1)

  (setq ido-create-new-buffer 'always))  ; Let ido make new files

(use-package ido-grid-mode
  :config
  (ido-grid-mode 1)

  (setq ido-grid-mode-prefix-scrolls t     ; Arrow points to selected column
        ido-grid-mode-start-collapsed t))  ; Show one line only until tab used

(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode 1))

(use-package recentf
  :config
  (recentf-mode 1)

  (setq recentf-max-saved-items 50)

  (defun ido-use/recentf-ido-find-file ()
    "Open list of most recently used files with ido."
    (interactive)
    (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
      (when file
        (find-file file))))

  (key/leader-map "f r" 'ido-use/recentf-ido-find-file))

(use-package smex
  :config
  (smex-initialize)

  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(provide 'ido-use)

;;; ido-use.el ends here

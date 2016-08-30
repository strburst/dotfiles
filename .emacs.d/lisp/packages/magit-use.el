;;; magit-use.el --- Git porcelain/interface
;;; Commentary:
;;; Code:

(require 'key)

(use-package magit
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)

  (defun magit-use/magit-save-stage-buffer-file ()
    "Save the current file and stage it with `magit-stage-file'."
    (interactive)
    (save-buffer)
    (magit-stage-file buffer-file-name)
    (message "Staged %s" buffer-file-name))

  (defun magit-use/magit-unstage-buffer-file ()
    "Save the current file and stage it with `magit-stage-file'."
    (interactive)
    (magit-unstage-file buffer-file-name)
    (message "Unstaged %s" buffer-file-name))

  (key/leader-map "g b" 'magit-blame
                  "g c" 'magit-clone
                  "g d" 'magit-diff-buffer-file
                  "g f" 'magit-fetch-from-upstream
                  "g F" 'magit-pull-from-upstream
                  "g g" 'magit-status
                  "g i" 'magit-init
                  "g l" 'magit-log-buffer-file
                  "g p" 'magit-push-implicitly
                  "g s" 'magit-use/magit-save-stage-buffer-file
                  "g u" 'magit-use/magit-unstage-buffer-file))

(use-package evil-magit)

(use-package gitattributes-mode
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

(provide 'magit-use)

;;; magit-use.el ends here

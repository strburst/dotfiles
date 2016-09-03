;;; org-use.el --- Note taking and todo list (builtin package)
;;; Commentary:
;;; Code:

(use-package org
  :config
  (setq org-completion-use-ido t)

  (setq org-startup-folded nil       ; Headings are unfolded when file is opened
        org-startup-indented t       ; Display sections/outlines indented
        org-startup-truncated nil))  ; Wrap long lines instead of truncating

(require 'evil-core)
(use-package evil-org
  :config
  ;; Unbind some evil keys that evil-org overrides
  (evil-define-key 'normal evil-org-mode-map
    "o" nil
    "O" nil))

(provide 'org-use)

;;; org-use.el ends here

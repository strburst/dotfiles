;;; org-cfg.el --- Note taking and todo list (builtin package)
;;; Commentary:
;;; Code:

(use-package org
  :config
  (setq org-completion-use-ido t)

  (setq org-startup-folded nil      ; Headings are unfolded when file is opened
        org-startup-indented t      ; Display sections/outlines indented
        org-startup-truncated nil)  ; Wrap long lines instead of truncating

  (setq org-pretty-entities t)

  (setq org-M-RET-may-split-line nil))  ; Make new heading after current heading

(require 'evil-core)
(use-package evil-org
  :config
  ;; Unbind some evil keys that evil-org overrides
  (evil-define-key 'normal evil-org-mode-map
    "g j" nil
    "g k" nil
    "o" nil
    "O" nil
    "J" nil
    "K" nil)

  ;; Now bind our own keys
  (evil-define-key 'normal evil-org-mode-map
    "[" 'outline-previous-visible-heading
    "]" 'outline-next-visible-heading))

(provide 'org-cfg)

;;; org-cfg.el ends here

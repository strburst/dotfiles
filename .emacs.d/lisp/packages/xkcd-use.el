;;; xkcd-use.el --- Load xkcd browser
;;; Commentary:
;;; Code:

(require 'evil)

(use-package xkcd
  :config
  (evil-set-initial-state 'xkcd-mode 'emacs)
  (add-hook 'xkcd-mode-hook  ; Clear visual distractions
            (lambda ()
              (linum-mode -1)
              (blink-cursor-mode -1)
              (setq cursor-type nil)))

  (global-set-key (kbd "<f8>") 'xkcd)
  (general-define-key :keymaps 'xkcd-mode-map
                      "h" 'xkcd-prev
                      "j" 'xkcd-next
                      "k" 'xkcd-prev
                      "l" 'xkcd-next))

(provide 'xkcd-use)

;;; xkcd-use.el ends here

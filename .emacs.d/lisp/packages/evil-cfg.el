;;; evil-cfg.el --- Load evil and related packages
;;; Commentary:
;;; Code:

(require 'key)

(use-package evil
  :init
  ;; Allow evil to override more Emacs keybindings
  (setq evil-search-module 'evil-search
        evil-want-C-i-jump t
        evil-want-C-u-scroll t
        evil-want-C-w-in-emacs-state t
        evil-want-Y-yank-to-eol t)  ; More consistent with C and D

  :config
  ;; Workaround: evil-tabs breaks evil initial states if activated after evil
  (use-package evil-tabs
    :config
    (global-evil-tabs-mode 1))

  (evil-mode 1)

  ;; Swap ^ and 0 (^ is more useful)
  (evil-redirect-digit-argument evil-motion-state-map "0" 'evil-first-non-blank)
  (define-key evil-normal-state-map (kbd "^") 'evil-beginning-of-line)

  ;; K joins previous line (defined as a keyboard macro)
  (fset 'evil-join-previous [?k ?J])
  (define-key evil-normal-state-map (kbd "K") 'evil-join-previous)

  (defun evil-use/evil-read (insert-file)
    "Interactively call evil-read interactively."
    (interactive "fInsert file: ")
    (evil-read nil insert-file))

  (key/leader-map "f i" 'evil-use/evil-read))

(use-package evil-commentary
  :config
  (evil-commentary-mode)
  :diminish evil-commentary-mode)

(use-package evil-leader
  :config
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "SPC SPC"))

(use-package evil-numbers
  :config
  (define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(provide 'evil-cfg)

;;; evil-cfg.el ends here

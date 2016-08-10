;;; setup-packages.el --- Set up package.el and load essential packages

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap by installing use-package, which can auto-install everything else
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)  ; Always auto-install use-package packages

;; Essential packages
(use-package bind-key)  ; Used by use-package to bind keys
(use-package dash)      ; List-manipulation utility
(use-package diminish)  ; Hide minor modes from the statusline
(use-package general)   ; Keybinding package that is aware of evil states

(provide 'setup-packages)

;;; setup-packages.el ends here

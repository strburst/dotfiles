;; Package configuration
(require 'package)
(setq package-archives `(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t
      evil-want-C-i-jump t)

(require 'evil)
(require 'evil-commentary)
(require 'evil-leader)
(require 'evil-org)
(require 'evil-surround)
(require 'evil-tabs)

(evil-mode)
(evil-commentary-mode)
(global-evil-surround-mode)

(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(require 'flycheck)
(require 'magit)

(require 'undo-tree)
(global-undo-tree-mode)

(setq tls-checktrust t)

;; Disable unecessary gui toolbars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable splash screen and startup message
(setq inhibit-startup-screen t
      inhibit-startup-area-message "allen.zheng")

(global-linum-mode 1)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

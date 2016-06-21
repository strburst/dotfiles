;; Package configuration
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(require 'editorconfig)

;; Allow evil to override more Emacs keybindings
(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t
      evil-want-C-i-jump t)
(require 'evil)
(evil-mode)

;; Swap ; and : to enter ex commands faster
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)
(define-key evil-visual-state-map (kbd ":") 'evil-repeat-find-char)

;; Swap ^ and 0 (^ is more useful)
(evil-redirect-digit-argument evil-motion-state-map "0" 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "^") 'evil-beginning-of-line)

;; Must come after requiring evil
(require 'alda-mode)
(define-key evil-motion-state-map "gp" 'alda-evil-play-region)

(require 'evil-commentary)
(evil-commentary-mode)

(require 'evil-leader)
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)

(require 'evil-org)

(require 'evil-surround)
(global-evil-surround-mode)

(require 'evil-tabs)
(global-evil-tabs-mode)

(unless (boundp 'init-complete)
    (elscreen-toggle-display-tab))
(define-key evil-motion-state-map (kbd "C-f") nil)
(setq elscreen-prefix-key (kbd "C-f"))

(require 'flycheck)
(require 'magit)

(require 'undo-tree)
(global-undo-tree-mode)

(setq tls-checktrust t)

;; Garbage collection occurs every 10 MB
(setq gc-cons-threshold 10000000)

;; Disable unecessary gui toolbars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable splash screen and startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "allen")

;; Don't clutter init.el with settings from custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Set browser to chromium
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

;; Configure backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t     ;; Use copying (doesn't clobber symlinks)
      version-control t       ;; Keep multiple numbered backups
      kept-new-versions 5
      kept-old-versions 0
      delete-old-versions t   ;; Don't prompt to delete old versions
      vc-make-backup-files t) ;; Backup files in version control too

(set-face-font 'default "-*-terminus-medium-r-*-*-16-*-*-*-*-*-*-*")
(load-theme 'solarized-dark t)

;; Enable line numbers
(global-linum-mode 1)

;; Subtly highlight the line the cursor is on
(hl-line-mode 1)

;; Replace yes/no prompts with simpler y/n ones
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keymaps to open/source init.el
(defun open-emacs-config ()
  "Open ~/.emacs.d/init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f9>") 'open-emacs-config)

(defun source-emacs-config ()
  "Loads ~/.emacs.d/init.el"
  (interactive)
  (load "~/.emacs.d/init.el"))

(global-set-key (kbd "<f10>") 'source-emacs-config)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

(setq init-complete t)

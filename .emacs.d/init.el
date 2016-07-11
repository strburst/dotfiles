;; Garbage collection occurs every 20 MB
(setq gc-cons-threshold 20000000)

;; Disable unecessary gui toolbars (must come early to avoid momentary display)
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
      backup-by-copying t    ;; Use copying (doesn't clobber symlinks)
      delete-old-versions t  ;; Don't prompt to delete old versions
      kept-new-versions 5
      kept-old-versions 0
      version-control t)     ;; Keep multiple numbered backups

(set-face-font 'default "-*-terminus-medium-r-*-*-16-*-*-*-*-*-*-*")

;; Keep lines at 80 characters when autoformatting
(setq fill-column 80)

;; Configure version control settings
(setq vc-follow-symlinks t     ;; Don't prompt to follow symlinks to vc'd files
      vc-handled-backends '(Git Hg SVN) ;; Only check for modern vcs's
      vc-make-backup-files t)  ;; Backup files in version control too

;; Show partially completed key sequences sooner
(setq echo-keystrokes 0.1)

;; Allow copy/paste with the X clipboard
(setq select-enable-clipboard t)

;; Enable line numbers
(global-linum-mode 1)

;; Subtly highlight the line the cursor is on
(global-hl-line-mode 1)

;; Make mousewheel scrolling less jumpy
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

;; Highlight tabs, trailing spaces, and long lines
(setq whitespace-style '(face tabs trailing lines-tail tab-mark))
(global-whitespace-mode 1)

;; Replace yes/no prompts with simpler y/n ones
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keymaps to open/source init.el
(defun open-emacs-config ()
  "Open ~/.emacs.d/init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f9>") 'open-emacs-config)

(defun source-emacs-config ()
  "Load ~/.emacs.d/init.el."
  (interactive)
  (load "~/.emacs.d/init.el"))

(global-set-key (kbd "<f10>") 'source-emacs-config)

;; Package configuration
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(load-theme 'solarized-dark t)

(require 'editorconfig)
(editorconfig-mode)

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

(require 'flycheck)
(require 'magit)

(require 'nyan-mode)
(nyan-mode 1)

(defun toggle-max-nyan ()
  "Toggle the music and animation in nyan mode."
  (interactive)
  (if nyan-animate-nyancat
      (progn
	(nyan-stop-animation)
	(nyan-stop-music))
      (nyan-start-animation)
      (nyan-start-music)))

(global-set-key (kbd "<f12>") 'toggle-max-nyan)

(require 'undo-tree)
;; Persist undo history between sessions
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
      undo-tree-auto-save-history t)
(global-undo-tree-mode 1)

(require 'vimrc-mode)

(require 'xkcd)
(evil-set-initial-state 'xkcd 'emacs)

(add-hook 'xkcd-mode-hook
	  (lambda ()
	    (linum-mode -1)
	    (setq cursor-type nil)))

;; Add some xkcd keybindings
(define-key xkcd-mode-map (kbd "h") 'xkcd-prev)
(define-key xkcd-mode-map (kbd "j") 'xkcd-next)
(define-key xkcd-mode-map (kbd "k") 'xkcd-prev)
(define-key xkcd-mode-map (kbd "l") 'xkcd-next)

;; Ergonomic tetris is important
(add-hook 'tetris-mode-hook
	  (lambda ()
	     (define-key tetris-mode-map (kbd "h") 'tetris-move-left)
	     (define-key tetris-mode-map (kbd "j") 'tetris-rotate-next)
	     (define-key tetris-mode-map (kbd "k") 'tetris-rotate-prev)
	     (define-key tetris-mode-map (kbd "l") 'tetris-move-right)))

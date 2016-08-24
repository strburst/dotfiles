;;; base.el --- Miscellaneous  configuration independent of external packages
;;; Commentary:
;;; Code:

(setq gc-cons-threshold 20000000)  ; Garbage collection occurs every 20 MB

;; Disable unecessary gui toolbars (must come early to avoid momentary display)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)  ; Disable splash screen

;; Don't clutter init.el with settings from custom
(setq custom-file (init/config-path "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file))

;; Set the window title
(lib/setq-same '("%b, %m mode -- " invocation-name "@" system-name)
               frame-title-format icon-title-format)

;; Set browser to chromium
(require 'browse-url)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

;; Configure backup settings
(setq backup-directory-alist `(("." . ,(init/config-path "backup")))
      backup-by-copying t    ; Use copying (doesn't clobber symlinks)
      delete-old-versions t  ; Don't prompt to delete old versions
      kept-new-versions 5
      kept-old-versions 0
      version-control t)     ; Keep multiple numbered backups

(setq-default indent-tabs-mode nil)  ; Stick to spaces for now

(setq-default fill-column 80)  ; Keep lines at 80 characters when autoformatting

(set-face-font 'default "-*-terminus-medium-r-*-*-16-*-*-*-*-*-*-*")

;; Configure version control settings
(setq vc-follow-symlinks t     ; Don't prompt to follow symlinks to vc'd files
      vc-handled-backends '(Git Hg SVN) ; Only check for modern vcs's
      vc-make-backup-files t)  ; Backup files in version control too

(setq echo-keystrokes 0.1)  ; Show partially completed key sequences sooner

(global-linum-mode 1)    ; Enable line numbers
(global-hl-line-mode 1)  ; Subtly highlight the line the cursor is on

(require 'paren)
(show-paren-mode 1)        ; Visually highlight matching parens
(setq show-paren-delay 0)  ; Do it immediately

(setq column-number-mode t)  ; Enable column number display

;; Make mousewheel scrolling less jumpy
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

(setq scroll-margin 8
      scroll-step 1
      scroll-preserve-screen-position t
      scroll-conservatively 10000)

(setq initial-scratch-message nil)  ; Scratch buffer is strictly empty

(defalias 'yes-or-no-p 'y-or-n-p)  ; Replace yes/no prompts with y/n

(provide 'base)

;;; base.el ends here
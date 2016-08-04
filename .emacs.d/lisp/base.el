;;; base.el --- For basic configuration independent of external packages

(setq gc-cons-threshold 20000000)  ; Garbage collection occurs every 20 MB

;; Disable unecessary gui toolbars (must come early to avoid momentary display)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)  ; Disable splash screen

;; Don't clutter init.el with settings from custom
(setq custom-file (config-relative-path "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file))

;; Set browser to chromium
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

;; Configure backup settings
(setq backup-directory-alist `(("." . ,(config-relative-path "backup")))
      backup-by-copying t    ; Use copying (doesn't clobber symlinks)
      delete-old-versions t  ; Don't prompt to delete old versions
      kept-new-versions 5
      kept-old-versions 0
      version-control t)     ; Keep multiple numbered backups

;; Stick to spaces for now
(setq-default indent-tabs-mode nil)

(set-face-font 'default "-*-terminus-medium-r-*-*-16-*-*-*-*-*-*-*")

(setq fill-column 80)  ; Keep lines at 80 characters when autoformatting

;; Configure version control settings
(setq vc-follow-symlinks t     ; Don't prompt to follow symlinks to vc'd files
      vc-handled-backends '(Git Hg SVN) ; Only check for modern vcs's
      vc-make-backup-files t)  ; Backup files in version control too

(setq echo-keystrokes 0.1)  ; Show partially completed key sequences sooner

(setq select-enable-clipboard t)  ; Allow copy/paste with the X clipboard

(global-linum-mode 1)    ; Enable line numbers
(global-hl-line-mode 1)  ; Subtly highlight the line the cursor is on

(show-paren-mode 1)        ; Visually highlight matching parens
(setq show-paren-delay 0)  ; Do it immediately

;; Highlight tabs, trailing spaces, and long lines
(setq whitespace-style '(face tabs trailing lines-tail tab-mark))
(global-whitespace-mode 1)

;; Make mousewheel scrolling less jumpy
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

(defalias 'yes-or-no-p 'y-or-n-p)  ; Replace yes/no prompts with y/n

(defun print-major-mode ()
  "Print the name of the current major mode to the minibuffer."
  (interactive)
  (message "Current major mode is: %s" mode-name))

(global-set-key (kbd "<f2>") 'print-major-mode)

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

(global-set-key (kbd "<f12>") 'list-packages)

(provide 'base)

;;; base.el ends here
;;; init.el --- Set up the load path and invoke base config and package setup

;; (package-initialize)  ; prevent package.el from helpfully adding this

;; Disable startup message (must be in init.el)
(setq inhibit-startup-echo-area-message "allen.zheng")

(defun init/config-path (path)
  "Take a path and prepend the Emacs config directory."
  (concat user-emacs-directory path))

;; Add ~/.emacs.d/lisp and all subdirectories to load-path
(add-to-list 'load-path (init/config-path "lisp"))
(let ((default-directory (init/config-path "lisp")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'base-config)
(require 'base-lib)
(require 'setup-packages)
(require 'keybinds)
(require 'require-packages)

(if (file-exists-p (init/config-path "local.el"))
    (load (init/config-path "local.el")))

;; Make sure init time is echoed after post-init messages
(run-with-timer 1 nil 'base-lib/echo-init-time)

;;; init.el ends here

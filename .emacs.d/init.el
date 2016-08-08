;;; init.el --- Set up the load path and invoke base config and package setup

;; (package-initialize)  ; prevent package.el from helpfully adding this

;; Disable startup message (must be in init.el)
(setq inhibit-startup-echo-area-message "allen")

(defun config-relative-path (path)
  "Take a path and prepend the Emacs config directory."
  (concat user-emacs-directory path))

;; Add ~/.emacs.d/lisp and all subdirectories to load-path
(add-to-list 'load-path (config-relative-path "lisp"))
(let ((default-directory (config-relative-path "lisp")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'base-config)
(require 'base-lib)
(require 'base-keybinds)
(require 'package-setup)

(if (file-exists-p (config-relative-path "local.el"))
    (load (config-relative-path "local.el")))

(message "Init time: %s" (emacs-init-time))

;;; init.el ends here

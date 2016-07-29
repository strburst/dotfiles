;;; init.el --- Set up the load path and invoke base config and package setup

;; (package-initialize)  ; prevent package.el from helpfully adding this

;; Disable startup message (must be in init.el)
(setq inhibit-startup-echo-area-message "allen")

(defun config-relative-path (path)
  (concat user-emacs-directory path))

(add-to-list 'load-path (config-relative-path "lisp"))
(add-to-list 'load-path (config-relative-path "lisp/packages"))

(require 'base)
(require 'package-setup)

(if (file-exists-p (config-relative-path "local.el"))
    (load (config-relative-path "local.el")))

(message "Init time: %s" (emacs-init-time))

;;; init.el ends here

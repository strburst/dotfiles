;;; init.el --- Set up the load path and invoke base config and package setup
;;; Commentary:
;;; Code:

;; (package-initialize)  ; prevent package.el from helpfully adding this

;; Disable startup message (must be in init.el)
(setq inhibit-startup-echo-area-message "allen")

;; Add ~/.emacs.d/lisp and all subdirectories to load-path
(let* ((lisp-dir (concat user-emacs-directory "lisp"))
       (default-directory lisp-dir))
  (add-to-list 'load-path lisp-dir)
  (normal-top-level-add-subdirs-to-load-path))

(require 'lib)
(require 'base)
(require 'setup-packages)
(require 'key)
(require 'load-packages)

(let ((local-config-path (concat user-emacs-directory "local.el")))
  (if (file-exists-p local-config-path)
      (load local-config-path)))

;; Make sure init time is echoed after post-init messages
(run-with-timer 1 nil 'lib/echo-init-time)

;;; init.el ends here

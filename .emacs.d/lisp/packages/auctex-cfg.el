;;; auctex-cfg.el --- Environment for LaTeX editing
;;; Commentary:
;;; Code:

(require 'ensure)

;; Workaround for auctex using signum instead of cl-signum
(defalias 'signum 'cl-signum)
(ensure/load-library 'auctex)

;; Compile to preview whenever buffer is saved
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      (lambda ()
                        (TeX-command-run-all nil))
                      nil t)))

(provide 'auctex-cfg)

;;; auctex-cfg.el ends here

;;; markdown-use.el --- Configuration for editing markdown files
;;; Commentary:
;;; Code:

(require 'key)

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package gh-md
  :config
  (key/leader-map "j j" 'gh-md-render-buffer))

(provide 'markdown-use)

;;; markdown-use.el ends here

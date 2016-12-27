;;; lorem-ipsum-cfg.el --- Functions to insert lorem ipsum text
;;; Commentary:
;;; Code:

(require 'key)

(use-package lorem-ipsum
  :config
  (setq-default lorem-ipsum-list-bullet "- "
                lorem-ipsum-list-sentence-separator "- ")

  (key/leader-map "e l" 'lorem-ipsum-insert-list
                  "e p" 'lorem-ipsum-insert-paragraphs
                  "e s" 'lorem-ipsum-insert-sentences))

(provide 'lorem-ipsum-cfg)

;;; lorem-ipsum-cfg.el ends here

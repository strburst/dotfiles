;;; load-packages.el --- Load all packages
;;; Commentary:
;;; Code:

(require 'dash)

;; Note: files don't necessarily correspond to single packages
(defconst +configs-nice-1+ '(evil flycheck ido magit)
  "Essential/base packages and/or dependency for other config bundles.")
(defconst +configs-nice-2+ '(alda-mode
                             auctex
                             elscreen
                             lorem-ipsum
                             magit-annex
                             markdown
                             nyan-mode
                             org
                             paradox
                             ripgrep
                             rust-mode
                             smart-mode-line
                             solarized-theme
                             undo-tree
                             uniquify
                             vimrc-mode
                             which-key
                             whitespace
                             winner
                             xkcd
                             yasnippet)
  "Other miscellaneous packages.")

(defconst +config-require-list+
  (-flatten (list +configs-nice-1+ +configs-nice-2+))
  "List of all configs to be loaded.")

(defun load-packages/load-use-config (sym)
  "Load conventially-named configuration from SYM.

Specifically, require the symbol SYM-cfg and handle any errors that occur by
redirecting error messages to named *Init Errors*."
  (condition-case err
      (require (intern (concat (symbol-name sym) "-cfg")))
    ('error (pop-to-buffer "*Init Errors*")
            (insert (error-message-string err)))))

(mapc #'load-packages/load-use-config +config-require-list+)

(provide 'load-packages)

;;; load-packages.el ends here

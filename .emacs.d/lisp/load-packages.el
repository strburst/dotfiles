;;; load-packages.el --- Load all packages
;;; Commentary:
;;; Code:

(require 'dash)

;; Note: files don't necessarily correspond to single packages
(defconst +packages-nice-1+ '(evil))
(defconst +packages-nice-2+ '(alda-mode
                              auctex
                              elscreen
                              flycheck
                              ido
                              magit
                              markdown
                              nyan-mode
                              org
                              paradox
                              solarized-theme
                              smart-mode-line
                              undo-tree
                              winner
                              yasnippet
                              xkcd
                              vimrc-mode
                              whitespace))

(defconst +package-require-list+
          (mapcar (lambda (sym)
                    "Take a symbol and append \"-use\" to it."
                    (intern (concat (symbol-name sym) "-use")))
                  (-flatten (list +packages-nice-1+ +packages-nice-2+))))

(mapc 'require +package-require-list+)

(provide 'load-packages)

;;; load-packages.el ends here

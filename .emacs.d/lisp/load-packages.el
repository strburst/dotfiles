;;; require-packages.el --- Load all packages

(defconst +packages-nice-1+ '(evil))
(defconst +packages-nice-2+ '(alda-mode
                              elscreen
                              ido
                              magit
                              nyan-mode
                              org
                              solarized-theme
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

(provide 'require-packages)

;;; require-packages.el ends here

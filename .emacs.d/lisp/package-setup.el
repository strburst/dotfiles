;;; package-setup.el --- Set up package.el and load all packages

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap by installing use-package, which can auto-install everything else
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)  ; Always auto-install use-package packages

(use-package dash)      ; List-manipulation utility
(use-package diminish)  ; Hide minor modes from the statusline
(use-package bind-key)  ; Used by use-package to bind keys

(defconst +packages-nice-1+ '(evil))
(defconst +packages-nice-2+ '(alda-mode
                              elscreen
                              ido
                              nyan-mode
                              org
                              solarized-theme
                              undo-tree
                              winner
                              yasnippet
                              xkcd))

(defun symbol-append-use (sym)
  "Take a symbol and append \"-use\" to it."
  (intern (concat (symbol-name sym) "-use")))

(defconst +package-require-list+
          (mapcar 'symbol-append-use
                  (-flatten (list +packages-nice-1+ +packages-nice-2+))))

(mapc 'require +package-require-list+)

(provide 'package-setup)

;;; package-setup.el ends here

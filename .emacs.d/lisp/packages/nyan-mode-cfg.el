;;; nyan-mode-cfg.el --- Load nyan-mode
;;; Commentary:
;;; Code:

(require 'nyan-mode)

(use-package nyan-mode
  :config
  (nyan-mode 1)

  (defun toggle-max-nyan ()
    "Toggle the music and animation in nyan mode."
    (interactive)
    (if nyan-animate-nyancat
        (progn
          (nyan-stop-animation)
          (nyan-stop-music))
      (nyan-start-animation)
      (nyan-start-music))))

(provide 'nyan-mode-cfg)

;;; nyan-mode-cfg.el ends here

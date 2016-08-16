;;; nyan-mode-use.el --- Load nyan-mode
;;; Commentary:
;;; Code:

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

(provide 'nyan-mode-use)

;;; nyan-mode-use.el ends here

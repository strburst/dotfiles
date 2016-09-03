;;; key.el --- For miscellaneous keybindings that don't fit elsewhere
;;; Commentary:
;;; Code:

(defvar key/leader "SPC"
  "Leader key used in evil normal mode.")
(defvar key/leader-double "SPC SPC"
  "Leader key variant used in evil normal mode.")
(defvar key/fallback-leader "C-SPC"
  "Fallback leader key used in insert mode or Emacs state.")
(defvar key/fallback-leader-double "C-SPC SPC"
  "Fallback leader key variant used in insert mode or Emacs state.")

(require 'general)
(setq general-default-states '(normal insert emacs))

(defun key/leader-map (&rest maps)
  "Assign the given MAPS with my preferred prefixes.

These are `key/leader' in evil normal mode and `key/fallback-leader' elsewhere."
  (apply 'general-define-key
         :prefix key/leader
         :non-normal-prefix key/fallback-leader
         maps))

(defun key/leader-double-map (&rest maps)
  "Assign the given MAPS with doubled variants of my preferred prefixes.

These are `key/leader-double' in evil normal mode and
`key/fallback-leader-double' elsewhere."
  (apply 'general-define-key
         :prefix key/leader-double
         :non-normal-prefix key/fallback-leader-double
         maps))

(defun key/move-binding (keymap old-key new-key)
  "Take KEYMAP and unbind OLD-KEY, then bind it to NEW-KEY instead.

OLD-KEY and NEW-KEY are implicitly wrapped in kbd."
  (let* ((old-key-kbd (kbd old-key))
         (new-key-kbd (kbd new-key))
         (func (lookup-key keymap old-key-kbd)))
    (define-key keymap old-key-kbd nil)
    (define-key keymap new-key-kbd func)))

(defun key/swap-bindings (keymap key1 key2)
  "In KEYMAP, swap the bindings for KEY1 and KEY2."
  (let* ((key1-kbd (kbd key1))
         (key2-kbd (kbd key2))
         (func1 (lookup-key keymap key1-kbd))
         (func2 (lookup-key keymap key2-kbd)))
    (define-key keymap key1-kbd func2)
    (define-key keymap key2-kbd func1)))

;; Files/buffers
(key/leader-map "f d" 'vc-delete-file
                "f f" 'find-file
                "f j" 'switch-to-buffer
                "f k" 'kill-this-buffer
                "f q" 'save-buffers-kill-terminal
                "f m" 'vc-rename-file
                "f s" 'save-buffer
                "f S" 'save-some-buffers
                "f z" 'lib/save-and-kill)

;; Emacs Lisp
(key/leader-map "e e" 'eval-last-sexp
                "e i" 'eval-print-last-sexp)

;; Toggles
(key/leader-map "t h" 'evil-ex-nohighlight
                "t m" 'menu-bar-mode
                "t s" 'flyspell-mode)

;; Miscellaneous
(key/leader-map "u" 'universal-argument)

(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)

(global-set-key (kbd "<f2>") 'lib/echo-major-mode)
(global-set-key (kbd "<f9>") 'lib/open-emacs-config)
(global-set-key (kbd "<f12>") 'list-packages)

(provide 'key)

;;; key.el ends here

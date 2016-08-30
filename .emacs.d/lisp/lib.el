;;; lib.el --- For miscellaneous functions independent of external packages
;;; Commentary:
;;; Code:

(defmacro lib/add-mode-hooks (mode-hook-list mode-fn)
  "For hooks in MODE-HOOK-LIST, call MODE-FN to load a minor mode."
  `(dolist (hook ,mode-hook-list)
     (add-hook hook (lambda ()
                      (funcall ,mode-fn 1)))))

(defun lib/config-path (path)
  "Take PATH and prepend the Emacs config directory."
  (concat user-emacs-directory path))

(defmacro lib/declare-funcs (&rest func-file-cons-list)
  "Call `declare-function' with each FUNC and FILE in FUNC-FILE-CONS-LIST.

Useful for silencing byte-compiler warnings about undefined functions."
  (lib/macro-map (lambda (func-file)
                   (let ((func (car func-file))
                         (file (cdr func-file)))
                     `(declare-function ,func ,file)))
                 func-file-cons-list))

(defmacro lib/declare-vars (&rest vars)
  "Call `defvar' without a value for each VAR in VARS.

Useful for silencing byte-compiler warnings about undefined variables."
  (lib/macro-map (lambda (var)
                   `(defvar ,var))
                 vars))

(defun lib/echo-init-time ()
  "Print the value of `emacs-init-time'."
  (interactive)
  (message "Init time: %s" (emacs-init-time)))

(defun lib/echo-major-mode ()
  "Print the name and symbol for the current major mode."
  (interactive)
  (message "Major mode name: %s, symbol: %s" mode-name major-mode))

(defun lib/macro-map (func list)
  "Take FUNC, apply to each element in LIST, and wrap result in a progn.

Useful for implementing a common pattern for macros where each element in a list
expands to one sexpr."
  `(progn
     ,@(mapcar func list)))

(defun lib/open-emacs-config ()
  "Open the ~/.emacs.d directory."
  (interactive)
  (find-file user-emacs-directory))

(defun lib/save-and-kill ()
  "Save the current buffer if associated with a file and kill the buffer."
  (interactive)
  (save-buffer)
  (kill-this-buffer))

(defmacro lib/setq-same (value &rest vars)
  "Take VALUE and assign each VAR in VARS to VALUE."
  (let ((eval-value value))
    (lib/macro-map (lambda (var)
                     `(setq ,var ,eval-value))
                   vars)))

(provide 'lib)

;;; lib.el ends here

;;; ensure.el --- Functions to install the given package if not present
;;; Commentary:
;;; Code:

(defmacro ensure/install (pkg)
  "Install PKG if it isn't installed yet."
  `(unless (package-installed-p ,pkg)
     (package-install ,pkg)))

(defmacro ensure/load-library (pkg)
  "Install PKG if it isn't installed yet, then call `load-library' immediately."
  `(progn
     (ensure/install ,pkg)
     (load-library (symbol-name ,pkg))))

(defmacro ensure/require (pkg)
  "Install PKG if it isn't installed yet, then `require' it immediately."
  `(progn
     (ensure/install ,pkg)
     (require ,pkg)))

(defalias 'ensure 'ensure/require)

(provide 'ensure)

;;; ensure.el ends here

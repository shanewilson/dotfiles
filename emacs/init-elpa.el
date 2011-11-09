(defun require-package (package &optional min-version)
  "Ask elpa to install given PACKAGE."
  (unless (package-installed-p package min-version)
    (package-install package)))

;; When switching between Emacs 23 and 24, we always use the bundled package.el in Emacs 24
(let ((package-el-site-lisp-dir (expand-file-name "~/.emacs.d/site-lisp/package")))
  (when (and (file-directory-p package-el-site-lisp-dir)
             (> emacs-major-version 23))
    (message "Removing local package.el from load-path to avoid shadowing bundled version")
    (setq load-path (remove package-el-site-lisp-dir load-path))))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Starter-kit
(require-package 'starter-kit)
(require-package 'starter-kit-bindings)
(require-package 'starter-kit-eshell)
(require-package 'starter-kit-js)
(require-package 'starter-kit-lisp)

;; Utils
(require-package 'mic-paren) ;; advanced highlighting of matching parentheses
(require-package 'autopair) ;; automatically pairs braces and quotes
(require-package 'whole-line-or-region) ;; operate on current line if region undefined
(require-package 'diminish) ;; lets you fight modeline clutter by removing or abbreviating minor mode indicators.

;; Solarized
(when (< emacs-major-version 24)
  (require-package 'color-theme))
(require-package 'color-theme-solarized)

;; Git

;; Python


(provide 'init-elpa)

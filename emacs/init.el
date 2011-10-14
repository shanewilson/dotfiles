(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings clojure-mode slime-repl paredit slime)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("0174d99a8f1fdc506fa54403317072982656f127" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path (concat user-specific-dir "/color-theme-solarized"))
(require 'solarized-definitions)
(add-to-list 'custom-theme-load-path (concat user-specific-dir "/color-theme-solarized"))
(set-frame-font "Mono-10")
(load-theme 'solarized-dark)

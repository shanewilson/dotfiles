;; -*- coding: utf-8 -*-
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/site-lisp/color-theme-solarized/"))

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *macbook-pro-support-enabled* t)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

;;----------------------------------------------------------------------------
;; Make elisp more civilised
;;----------------------------------------------------------------------------
(require 'cl)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)
(require 'init-exec-path) ;; Set up $PATH
(require 'init-themes)
(require 'init-misc)
(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-windows)
(require 'init-editing-utils)
(require 'init-osx-keys)
(require 'init-evil)
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

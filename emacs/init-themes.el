;;------------------------------------------------------------------------------
;; Old-style color theming support (via color-theme.el)
;;------------------------------------------------------------------------------
(defcustom window-system-color-theme 'color-theme-solarized-dark
  "Color theme to use in window-system frames.
If Emacs' native theme support is available, this setting is
ignored: use `custom-enabled-themes' instead."
  :type 'symbol)

(defcustom tty-color-theme 'color-theme-terminal
  "Color theme to use in TTY frames.
If Emacs' native theme support is available, this setting is
ignored: use `custom-enabled-themes' instead."
  :type 'symbol)

(unless (boundp 'custom-enabled-themes)
  (defun color-theme-terminal ()
    (interactive)
    (color-theme-solarized-dark))

  (defun apply-best-color-theme-for-frame-type (frame)
    (with-selected-frame frame
      (if window-system
          (funcall window-system-color-theme)
        (funcall tty-color-theme))))

  (defun reapply-color-themes ()
    (interactive)
    (mapcar 'apply-best-color-theme-for-frame-type (frame-list)))

  (set-variable 'color-theme-is-global nil)
  (add-hook 'after-make-frame-functions 'apply-best-color-theme-for-frame-type)
  (add-hook 'after-init-hook 'reapply-color-themes)
  (apply-best-color-theme-for-frame-type (selected-frame)))


;;------------------------------------------------------------------------------
;; New-style theme support, in which per-frame theming is not possible
;;------------------------------------------------------------------------------

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(solarized-dark))


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (if (boundp 'custom-enabled-themes)
      (custom-set-variables '(custom-enabled-themes '(solarized-light)))
    (color-theme-solarized-light)))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (if (boundp 'custom-enabled-themes)
      (custom-set-variables '(custom-enabled-themes '(solarized-dark)))
    (color-theme-solarized-dark)))


(require 'solarized-definitions)
(load-theme 'solarized-dark)
(set-frame-font "Mono-10")

(provide 'init-themes)

;; Native UI Tweaks
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(use-package doom-themes)
(load-theme 'doom-one)
;;(load-theme 'leuven-dark)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'leuven t))
    ('dark (load-theme 'leuven-dark t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;; icons
(use-package all-the-icons)

(use-package nerd-icons)

(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

(use-package doom-modeline
  :init (doom-modeline-mode))

(use-package doom-themes)

(load-theme 'doom-one)

(set-frame-font "Inconsolata 16" nil t)

(provide 'codester-ui)

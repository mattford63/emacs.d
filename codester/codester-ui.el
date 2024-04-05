;;; codester-ui.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(use-package doom-themes)
(load-theme 'doom-one)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(save-place-mode)

;; icons
(use-package all-the-icons)

(use-package nerd-icons)

(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

(use-package doom-modeline
  :init (doom-modeline-mode))

(use-package doom-themes)

(load-theme 'doom-one)

(use-package multiple-cursors)

(provide 'codester-ui)

;;; codester-ui.el ends here

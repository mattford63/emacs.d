;;; codester-ui.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t)
(setq default-frame-alist '((font . "JetBrainsMono Nerd Font 14")
			    (vertical-scroll-bars . nil)))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(save-place-mode)
(setq left-fringe-width 12)

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t))

(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode)
  :config
  (setq doom-modeline-highlight-modified-buffer-name nil))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode
    '("=>" "->" "->>" "<=>" "==" "!=" ">=" "<=" "&&" "||"
      "::" "..." ".." "##" "###" "####" "++" "--"
      "<<" ">>" ">>>" "/*" "*/" "//" "/**"))
  (global-ligature-mode t))

(use-package multiple-cursors)

(winner-mode 1)

(provide 'codester-ui)

;;; codester-ui.el ends here




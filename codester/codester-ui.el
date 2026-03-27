;;; codester-ui.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(push '(font . "Menlo 14") default-frame-alist)
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




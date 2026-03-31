;;; codester-ui.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(save-place-mode)

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t))

(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode)
  :config
  (setq doom-modeline-highlight-modified-buffer-name nil
        doom-modeline-buffer-encoding nil)
  (advice-add #'vc-mode-line :after
              (lambda (&rest _)
                (when (stringp vc-mode)
                  (setq vc-mode (propertize vc-mode
                                            'local-map
                                            (let ((map (make-sparse-keymap)))
                                              (define-key map [mode-line mouse-1] #'magit-status)
                                              map)))))))

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




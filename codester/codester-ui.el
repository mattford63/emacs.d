;; Performance
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;; Modifier keys
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Native UI Tweaks
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(load-theme 'doom-one)
;;(load-theme 'leuven-dark)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;;(add-to-list 'default-frame-alist '(undecorated-round . t))
(setq-default set-mark-command-repeat-pop t)
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'leuven t))
    ('dark (load-theme 'leuven-dark t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
(save-place-mode)

;; icons
(use-package all-the-icons)

(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode))

(use-package doom-themes)

(provide 'codester-ui)

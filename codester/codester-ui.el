;; Performance
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;; Native UI Tweaks
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(load-theme 'leuven)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(undecorated-round . t))
(setq-default set-mark-command-repeat-pop t)
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'leuven t))
    ('dark (load-theme 'leuven-dark t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
(save-place-mode)

(provide 'codester-ui)

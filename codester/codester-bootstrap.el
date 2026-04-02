;;; codester-bootstrap.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Reset GC threshold after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))))

;; macOS modifier keys and niceties
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
)

(setq frame-title-format "%b")
(setq ring-bell-function 'ignore)

;; Smooth scrolling
(pixel-scroll-precision-mode)

(provide 'codester-bootstrap)

;;; codester-bootstrap.el ends here

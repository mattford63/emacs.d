;;; codester-bootstrap.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Reset GC threshold after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))))

;; Modifier keys
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)

;; macOS niceties
(setq ns-use-proxy-icon nil)
(setq frame-title-format "%b")

;; Smooth trackpad scrolling
(pixel-scroll-precision-mode)

(provide 'codester-bootstrap)

;;; codester-bootstrap.el ends here

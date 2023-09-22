(use-package adaptive-wrap)

;; Couple text mode settings to this package

(use-package visual-fill-column
  :init
  (setq fill-column 110)
  (setq-default visual-fill-column-center-text t))

(use-package lsp-grammarly
  :ensure t)

(defun text-modes ()
  (progn (flyspell-mode)
	 (visual-line-mode)
	 (visual-fill-column-mode)
	 (adaptive-wrap-prefix-mode)
	 (lsp)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.mdx" . markdown-mode))
  :custom
  (markdown-enable-highlighting-syntax t)
  (markdown-hide-urls t)
  :hook
  ((gfm-mode markdown-mode) . text-modes))

(provide 'codester-text)

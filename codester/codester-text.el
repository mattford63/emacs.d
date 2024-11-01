;;; codester-text.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(provide 'codester-text)

(use-package adaptive-wrap)

(use-package visual-fill-column
  :init
  (setq fill-column 110)
  (setq-default visual-fill-column-center-text nil))

;; (use-package jinx
;;   :bind (("M-$" . jinx-correct)
;;          ("C-M-$" . jinx-languages)))

;;(use-package flymake-proselint)

(defun text-modes ()
  "Load text modes."
  (progn
;;    (jinx-mode)
    (visual-line-mode)
    (visual-fill-column-mode)
    (adaptive-wrap-prefix-mode)
    (flymake-mode)
    (flymake-proselint-setup)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.mdx" . markdown-mode))
  :custom
  (markdown-enable-highlighting-syntax t)
  (markdown-hide-urls t)
  :hook
  ((gfm-mode markdown-mode) . text-modes))

(use-package pdf-tools)

(provide 'codester-text)

;;; codester-text.el ends here


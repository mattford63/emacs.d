;;; codester-text.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package adaptive-wrap)

(use-package visual-fill-column
  :init
  (setq fill-column 90)
  (setq-default visual-fill-column-center-text t))

(use-package jinx
  :init
  (setenv "LANG" "en_GB.UTF-8")
  :custom
  (jinx-languages "en_GB.UTF-8")
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package flymake-proselint)

(defun text-modes ()
  "Load text modes."
  (progn
    (jinx-mode)
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

;; (use-package pdf-tools) ;; causes face_for_font crash on macOS 26.3

(provide 'codester-text)

;;; codester-text.el ends here


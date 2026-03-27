;;; codester-tools.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Tramp
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; News Reader
(use-package elfeed
  :bind
  ("C-c e" . elfeed))

(use-package elfeed-org
  :config
  (elfeed-org))

(provide 'codester-tools)

;;; codester-tools.el ends here

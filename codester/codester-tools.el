;;; codester-tools.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Bamboo HR
(use-package ts)
(use-package a)
(use-package tablist)
(use-package bhr
  :vc (:fetcher github :repo elken/bhr.el)
  :config
  (setq bhr-org-name "juxtpro"))

;; Tramp
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; News Reader
(use-package elfeed)
(use-package elfeed-org
  :config
  (elfeed-org))

;; Pintentry
(use-package pinentry
  :config
  (pinentry-start))

(provide 'codester-tools)

;;; codester-tools.el ends here

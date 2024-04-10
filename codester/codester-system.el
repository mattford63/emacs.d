;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Auth sources
(setq auth-sources '("~/.authinfo.gpg"))
(require 'epg)
(setq epg-pinentry-mode 'loopback)

;; Shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(pgtk))
    (exec-path-from-shell-initialize)))

;; Backups/Files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(global-auto-revert-mode t)

;; Package System Setup
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package diminish)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Terminal
(use-package vterm)
(use-package vterm-toggle
  :bind (("C-`" . 'vterm-toggle)
	 ("<C-return>" . 'vterm-toggle-insert-cd)))

;; Dired
(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies t
	dired-recursive-deletes t
	dired-dwim-target t
	delete-by-moving-to-trash t)
  (setf dired-kill-when-opening-new-dired-buffer t)
  :bind
  (:map dired-mode-map
	("e" . wdired-change-to-wdired-mode)))

;; Wgrep
(use-package wgrep)

;; Search
(use-package ripgrep)

;; Browse
(add-hook 'window-configuration-change-hook
	  (lambda ()
	    (when (equal major-mode 'xwidget-webkit-mode)
	      (xwidget-webkit-adjust-size-dispatch)
	      (set-xwidget-query-on-exit-flag (xwidget-webkit-current-session) nil))))
(setq browse-url-browser-function 'browse-url-chromium)
(setq browse-url-secondary-browser-function 'xwidget-webkit-browse-url)
(setq browse-url-generic-program "chromium")
(require 'xwidget)
(define-key xwidget-webkit-mode-map (kbd "C-c C-=") #'xwidget-webkit-zoom-in)
(define-key xwidget-webkit-mode-map (kbd "C-c C--") #'xwidget-webkit-zoom-out)


(use-package direnv
  :config
  (direnv-mode)
  (setq direnv-always-show-summary nil))

(provide 'codester-system)
;;; codester-system.el ends here


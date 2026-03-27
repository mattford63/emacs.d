;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Auth sources
(setq auth-sources '("~/.authinfo.gpg"))
(require 'epa)
(setq epg-pinentry-mode 'loopback)

;; Backups/Files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(global-auto-revert-mode t)

;; Package System Setup
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package diminish)

;; Terminal
(use-package vterm
  :config
  (setq vterm-timer-delay 0.05))

(with-eval-after-load 'vterm
  (defun my/vterm-replace-bullet (orig-fun process input)
    "Replace ⏺ with ● before vterm renders it.
Uses raw UTF-8 bytes because vterm--filter receives unibyte strings."
    (funcall orig-fun process
             (string-replace (unibyte-string #xe2 #x8f #xba)
                             (unibyte-string #xe2 #x97 #x8f)
                             input)))
  (advice-add 'vterm--filter :around #'my/vterm-replace-bullet))

(use-package vterm-toggle
  :bind (("C-`" . vterm-toggle)
	 ("<C-return>" . vterm-toggle-insert-cd)))

;; Dired
(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'always
	dired-dwim-target t
	delete-by-moving-to-trash t)
  (setf dired-kill-when-opening-new-dired-buffer t)
  :bind
  (:map dired-mode-map
	("e" . wdired-change-to-wdired-mode)))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Wgrep
(use-package wgrep)

;; Browse
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(use-package project
  :ensure nil
  :bind (:map project-prefix-map
	      ("m" . project-magit-dir))
  :config
  (defun project-magit-dir ()
    "Run Magit in the current project's root."
    (interactive)
    (magit (project-root (project-current t))))
  (add-to-list 'project-switch-commands '(project-magit-dir "Magit")))

;; Automatice Environment
(use-package direnv
  :config
  (direnv-mode)
  (setq direnv-always-show-summary nil))

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(provide 'codester-system)
;;; codester-system.el ends here


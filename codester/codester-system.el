;; Auth sources
(setq auth-sources '("~/.authinfo.gpg"))

;; Backups/Files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(global-auto-revert-mode t)

;; Package System Setup
(setq treesit-extra-load-path '("~/src/tree-sitter-module/dist"))

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package diminish)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; Terminal
(use-package vterm
  :vc (:fetcher github :repo akermu/emacs-libvterm))

(use-package vterm-toggle
  :vc (:fetcher github :repo jixiuf/vterm-toggle)
  :bind (("C-c #" . 'vterm-toggle)
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

(setq browse-url-browser-function 'xwidget-webkit-browse-url)

(provide 'codester-system)


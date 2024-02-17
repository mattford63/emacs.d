;; Version Control
(use-package magit)

(use-package git-link
  :bind (("C-c g l" . git-link)
	 ("C-c g c" . git-link-commit)
	 ("C-c g h" . git-link-homepage)))

;; YAML
(use-package yaml-ts-mode
  :mode (("\\.yaml\\'" . yaml-ts-mode)
	 ("\\.yml\\'" . yaml-ts-mode)))

;; Clojure
(use-package clojure-mode
  :config
  (setq clojure-docstring-fill-column 80)
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-overlays-use-font-lock t
	cider-mode-line-show-connection t
	cider-eldoc-display-for-symbol-at-point nil ; prefer lsp
	cljr-add-ns-to-blank-clj-files nil ; prefer lsp
	cider-use-tooltips nil ; prefer lsp
	)
  (cider-repl-toggle-pretty-printing)
  :hook (cider-mode . (lambda () (add-to-list 'completion-at-point-functions #'cape-cider-lsp))))

;; Java
(use-package jarchive
  :config (jarchive-setup))

;; Language Server Frameworks
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "<f5>"
	lsp-headerline-breadcrumb-enable nil
	lsp-completion-provider :none
	lsp-clojure-custom-server-command "~/.local/bin/clojure-lsp"
	lsp-enable-completion-at-point nil ;; we use cape
	)
  (defun lsp-mode-setup-completion () ;; Configure orderless
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) 
  :hook
  (((clojure-mode yaml-ts-mode) . lsp)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-completion-mode . lsp-mode-setup-completion)
   (lsp-help-mode . visual-line-mode))
  :bind
  (:map lsp-mode-map
	;;("C-c C-a" . lsp-execute-code-action)
	("s-." . xref-find-references)
	;;("m-g n" . flymake-goto-next-error)
	;;("M-g p" . flymake-goto-prev-error)
	)
  :commands
  (lsp))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; Code Helpers
(use-package paredit
  :hook
  ((emacs-lisp-mode
    clojure-mode)
   . paredit-mode)
  :diminish)

(use-package rainbow-mode
  :hook
  ((prog-mode . rainbow-mode))
  :diminish)

(use-package rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode))
  :diminish)

(use-package emacs
  :ensure nil
  :diminish eldoc-mode
  :hook
  ((prog-mode-hook . hs-minor-mode)))

(use-package expand-region
  :bind (("C-=" . 'er/expand-region)))

(provide 'codester-coding)

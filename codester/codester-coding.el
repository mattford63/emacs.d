;;; codester-coding.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Compilation
(global-set-key ["C-c C-c"] 'compile)

(setq tab-width 4)

;; Version Control
(use-package magit)
(use-package forge)
(use-package git-link
  :bind (("C-c g l" . git-link)
	 ("C-c g c" . git-link-commit)
	 ("C-c g h" . git-link-homepage)))

(use-package igist
  :defines igist-current-user-name igist-auth-marker
  :config
  (setq igist-current-user-name "mattford63"
	igist-auth-marker 'igist))

;; Treesitter
(use-package treesit-auto
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Clojure
(use-package clojure-ts-mode
  :mode (("\\.clj\\'" . clojure-ts-mode)
	 ("\\.edn\\'" . clojure-ts-mode)))

(use-package clj-refactor
  :hook ((clojure-mode clojure-ts-mode) . clj-refactor-mode))

(use-package cider
  :preface
  (defun my/cider-capf ()
  (when (eglot-managed-p)
    (setq-local completion-at-point-functions (list (cape-capf-super
						     #'eglot-completion-at-point
						     #'cider-complete-at-point
						     #'cape-file)))))
  :init
  (setq	cider-xref-fn-depth 90)
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock nil
        cider-prompt-save-file-on-load 'always-save
        ;;cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-overlays-use-font-lock t
	cider-mode-line-show-connection t
	cider-eldoc-display-for-symbol-at-point nil ; prefer lsp
	cljr-add-ns-to-blank-clj-files nil ; prefer lsp
	cider-use-tooltips nil ; prefer lsp
	)
  (cider-repl-toggle-pretty-printing)
  (custom-set-faces
   `(cider-fringe-good-face
     ((((class color) (background light)) :foreground "darkgreen")
      (((class color) (background dark)) :foreground "darkgreen")))
   ;;(remove-hook 'eldoc-documentation-functions #'cider-eldoc)
   )
  :hook
  (cider-mode . my/cider-capf))

(use-package eglot
  :ensure nil
  :hook (((clojure-mode clojurec-mode clojurescript-mode
	   clojure-ts-mode clojurescript-ts-mode clojurec-ts-mode
	   scala-mode go-mode java-mode java-ts-mode)
          . eglot-ensure))
  :bind
  (("C-c C-a" . 'eglot-code-actions)
   ("C-c C-r" . 'eglot-rename))
  :custom
  (eglot-ignored-server-capabilities
   '(;;:hoverProvider
     ;;:documentHighlightProvider
     ;;:documentRangeFormattingProvider
     ;;:documentOnTypeFormattingProvider
     ;;:colorProvider
     ;;:foldingRangeProvider
     :signatureHelpProvider
     ))
  )

(use-package eglot-java
  :hook
  ((java-mode . eglot-java-mode)
   (java-ts-mode . eglot-java-mode))
  :bind
  (("C-c l n" . eglot-java-file-new)
   ("C-c l x" . eglot-java-run-main)
   ("C-c l t" . eglot-java-run-test)
   ("C-c l N" . eglot-java-project-new)
   ("C-c l T" . eglot-java-project-build-task)
   ("C-c l R" . eglot-java-project-build-refresh))
  :config
  (setq eglot-java-user-init-opts-fn
        (lambda (server eglot-java-eclipse-jdt)
          '(:settings
            (:java
             (:import (:gradle (:wrapper (:enabled t)))))))))

;; Java
(use-package jarchive
  :config (jarchive-mode))

;; Go
(use-package go-mode
  :hook
  ((go-mode . (lambda ()
		(if (not (string-match "go" compile-command))
		    (set (make-local-variable 'compile-command)
			 "go build -v && go test -v && go vet"))
		(setq tab-width 4)))))

;; Code Helpers
(use-package smartparens
  :hook
  ((prog-mode text-mode markdown-mode emacs-lisp-mode lisp-mode)
   . smartparens-strict-mode)
  :diminish
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  ;; tree-sitter intergration for clojure
  (sp-local-pair '(clojure-ts-mode clojurec-ts-mode clojurescript-ts-mode) "'" "'" :actions nil)
  (sp-local-pair '(clojure-ts-mode clojurec-ts-mode clojurescript-ts-mode) "`'" "`'" :actions nil))

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
  ;;:hook
  ;;((prog-mode . hs-minor-mode))
  )

(use-package expand-region
  :bind (("C-=" . 'er/expand-region)))

(use-package flymake
  :ensure nil
  :hook
  ((prog-mode . flymake-mode))
  :bind
  (("M-g M-n" . 'flymake-goto-next-error)
   ("M-g M-p" . 'flymake-goto-prev-error)))

(use-package docker-compose-mode)
(use-package dockerfile-mode)
(use-package docker
  :bind ("C-c d" . docker))


(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (setq claude-code-ide-cli-path "~/.local/bin/claude")
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(provide 'codester-coding)

;;; codester-coding.el ends here

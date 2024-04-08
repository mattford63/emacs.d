;;; package --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Compilation
(global-set-key ["C-c C-c"] 'compile)

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
(require 'treesit)
(setq treesit-extra-load-path '("~/src/tree-sitter-module/dist"))

(use-package combobulate
    :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")
    :hook
      ((python-ts-mode . combobulate-mode)
       (js-ts-mode . combobulate-mode)
       (html-ts-mode . combobulate-mode)
       (css-ts-mode . combobulate-mode)
       (yaml-ts-mode . combobulate-mode)
       (typescript-ts-mode . combobulate-mode)
       (json-ts-mode . combobulate-mode)
       (tsx-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("~/src/combobulate"))

;; YAML
(use-package yaml-ts-mode
  :mode (("\\.yaml\\'" . yaml-ts-mode)
	 ("\\.yml\\'" . yaml-ts-mode)))

;; Clojure
(use-package clojure-mode)

(use-package clojure-ts-mode
  :config
  (push '(clojure-mode . clojure-ts-mode) major-mode-remap-alist)
  (push '(clojurec-mode . clojurec-ts-mode) major-mode-remap-alist)
  (push '(clojurescript-mode . clojurescript-ts-mode) major-mode-remap-alist)
  :mode (("\\.clj\\'" . clojure-ts-mode)
	 ("\\.edn\\'" . clojure-ts-mode)))

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
  (defface cider-fringe-good-face
    '((((class color) (background light)) :foreground "darkgreen")
      (((class color) (background dark)) :foreground "darkgreen"))
    "Face used on the fringe indicator for successful evaluation."
  :group 'cider)

  :hook
  (cider-mode . my/cider-capf))

(use-package nix-mode
  :hook ((nix-mode . (lambda ()
		       (set (make-local-variable 'compile-command)
			    "home-manager switch")))))

(use-package eglot
  :ensure t
  :preface
  :hook (((clojure-mode clojurec-mode clojurescript-mode
	   clojure-ts-mode clojurescript-ts-mode clojurec-ts-mode
	   java-mode scala-mode go-mode)
          . eglot-ensure))
  :bind
  (("C-c C-a" . 'eglot-code-actions)
   ("C-c C-r" . 'eglot-rename))
  :custom
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :documentHighlightProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider)))
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
  :hook
  ((prog-mode . hs-minor-mode)))

(use-package expand-region
  :bind (("C-=" . 'er/expand-region)))

(use-package flymake
  :hook
  ((prog-mode . flymake-mode))
  :bind
  (("M-g M-n" . 'flymake-goto-next-error)
   ("M-g M-p" . 'flymake-goto-prev-error)))

(provide 'codester-coding)

;;; codester-coding.el ends here

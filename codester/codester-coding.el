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
;; (setq treesit-language-source-alist
;;       '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
;;         (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
;;         (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
;;         (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
;;         (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
;;         (markdown . "https://github.com/ikatyang/tree-sitter-markdown")
;;         (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
;;         (rust . "https://github.com/tree-sitter/tree-sitter-rust")
;;         (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
;;         (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
;;         (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
;;         (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))

;; (dolist (mapping
;;          '((python-mode . python-ts-mode)
;;            (css-mode . css-ts-mode)
;;            (typescript-mode . typescript-ts-mode)
;;            (js2-mode . js-ts-mode)
;;            (bash-mode . bash-ts-mode)
;;            (conf-toml-mode . toml-ts-mode)
;;            (go-mode . go-ts-mode)
;;            (css-mode . css-ts-mode)
;;            (json-mode . json-ts-mode)
;;            (js-json-mode . json-ts-mode)))
;;   (add-to-list 'major-mode-remap-alist mapping)

;; Clojure
(use-package clojure-mode)

;; (use-package clojure-ts-mode
;;   :config
  ;; (push '(clojure-mode . clojure-ts-mode) major-mode-remap-alist)
  ;; (push '(clojurec-mode . clojurec-ts-mode) major-mode-remap-alist)
  ;; (push '(clojurescript-mode . clojurescript-ts-mode) major-mode-remap-alist)
  ;; :mode (("\\.clj\\'" . clojure-ts-mode)
  ;; 	    ("\\.edn\\'" . clojure-ts-mode)))

(use-package cider
  ;;:preface
  ;; (defun my/cider-capf ()
  ;;   (when (eglot-managed-p)
  ;;     (setq-local completion-at-point-functions (list (cape-capf-super
  ;; 						       #'eglot-completion-at-point
  ;; 						       #'cider-complete-at-point
  ;; 						       #'cape-file)))))
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
  (remove-hook 'eldoc-documentation-functions #'cider-eldoc))
  ;;:hook
  ;;(cider-mode . my/cider-capf)
  )

(use-package nix-mode
  :hook ((nix-mode . (lambda ()
		       (set (make-local-variable 'compile-command)
			    "home-manager switch")))))

;; (use-package eglot
;;   :hook (((clojure-mode clojurec-mode clojurescript-mode
;; 	   clojure-ts-mode clojurescript-ts-mode clojurec-ts-mode
;; 	   scala-mode go-mode java-mode java-ts-mode)
;;           . eglot-ensure))
;;   :bind
;;   (("C-c C-a" . 'eglot-code-actions)
;;    ("C-c C-r" . 'eglot-rename))
;;   ;; :custom
;;   ;; (eglot-ignored-server-capabilities
;;   ;;  '(;;:hoverProvider
;;   ;;    ;;:documentHighlightProvider
;;   ;;    ;;:documentRangeFormattingProvider
;;   ;;    ;;:documentOnTypeFormattingProvider
;;   ;;    :colorProvider
;;   ;;    :foldingRangeProvider
;;   ;;    ;;:signatureHelpProvider
;;   ;;    ))
;;   )

;; (use-package eglot-java
;;   :hook
;;   ((java-mode . eglot-java-mode)
;;    (java-ts-mode .eglot-java-mode))
;;   :bind
;;   ("C-c l n" . 'eglot-java-file-new)
;;   ("C-c l x" . 'eglot-java-run-main)
;;   ("C-c l t" . 'eglot-java-run-test)
;;   ("C-c l N" . 'eglot-java-project-new)
;;   ("C-c l T" . 'eglot-java-project-build-task)
;;   ("C-c l R" . 'eglot-java-project-build-refresh))

;; Java
(use-package jarchive
  :config (jarchive-mode))

;; Go
(use-package go-mode
  :hook
  ((go-mode . (lambda ()
		(if (not (string-match "go" compile-command))
		    (set (make-local-variable 'compile-command)
			 "go build -v && go test -v && go vet"))))))

;; Code Helpers
(use-package smartparens
  :hook
  ((prog-mode text-mode markdown-mode emacs-lisp-mode lisp-mode)
   . smartparens-strict-mode)
  :diminish
  :config
  ;; This is fixed on master but not made into melpa yet
  (defalias 'sp--syntax-class-to-char 'syntax-class-to-char)
  (when (version< emacs-version "28.1")
    ;; Ripped from Emacs 27.0 subr.el.
    ;; See Github Issue#946 and Emacs bug#31692.
    (defun sp--syntax-class-to-char (syntax)
      "Return the syntax char of CLASS, described by an integer.
For example, if SYNTAX is word constituent (the integer 2), the
character w (119) is returned.

This is a compat function for `syntax-class-to-char'."
      (let ((spec " .w_()'\"$\\/<>@!|"))
        (if (and (fixnump syntax)
                 (>= syntax 0)
                 (< syntax 16))
            (aref spec syntax)
          (error "Args out of range")))))
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
  :hook
  ((prog-mode . flymake-mode))
  :bind
  (("M-g M-n" . 'flymake-goto-next-error)
   ("M-g M-p" . 'flymake-goto-prev-error)))

(use-package docker-compose-mode)
(use-package dockerfile-mode)
(use-package docker
  :bind ("C-c d" . docker))

(provide 'codester-coding)

;;; codester-coding.el ends here

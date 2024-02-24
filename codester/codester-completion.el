;;; codester-completion.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Key Chord helper
(use-package which-key
  :config
  (which-key-mode)
  :diminish)

;; In buffer completion
(use-package corfu
  :custom
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-commit-predicate nil)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-echo-documentation t)
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  :init
  (defalias 'cape-cider-lsp
    (cape-capf-super #'cider-complete-at-point #'lsp-completion-at-point))
  (add-to-list 'completion-at-point-functions #'cape-cider-lsp)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Mini-buffer completion
(use-package vertico
  :config
  (vertico-mode))

;; Advanced completion styles
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; Search and Navigation functions (use Vertico)
(use-package consult
  :config
  (setq consult-preview-key nil)
  (recentf-mode)
  :bind
  (("C-c r" . consult-recent-file)
   ("C-c f" . consult-ripgrep)
   ("C-x b" . consult-buffer)
   ("M-g i" . consult-imenu-multi)
   ("C-c i" . consult-imenu)))

;; Adds annotations to minibuffer 
(use-package marginalia
  :config
  (marginalia-mode))

;; Provides minibuffer actions and context menu
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Project management
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (require 'tramp)
  (setq projectile-project-search-path '(("~/src/" . 2) ("~/.emacs.d")))
  (projectile-mode +1)
  :bind (("<f6>" . projectile-ripgrep)
         ("C-<f6>" . projectile-replace)
         ("<f7>" . projectile-find-file)
         ("<f8>" . projectile-run-vterm)
         ("C-c p" . projectile-command-map))
  :diminish)

;; Templating
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)


(provide 'codester-completion)

;;; codester-completion.el ends here

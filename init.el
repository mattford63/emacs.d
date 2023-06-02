;; brew install tree-sitter
;; brew install emacs-plus@29 --with-native-comp --with-xwidgets
;; git clone git@github.com:casouri/tree-sitter-module.git
;; Run ./batch.sh to grab the grammars.

(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/codester")
(setq custom-file "~/.emacs.d/custom.el")

(require 'codester-ui)
(require 'codester-system)
(require 'codester-text)
(require 'codester-completion) 
(require 'codester-coding)
(require 'codester-org)
(require 'codester-tools)
(load custom-file)

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Personal Emacs configuration for macOS, using Emacs 29+ with native compilation, tree-sitter, and xwidgets. Package management uses `use-package` with packages from MELPA.

## Architecture

The config is split into a modular system loaded from `init.el`:

- **`early-init.el`** — Pre-GUI setup: GC threshold, disables toolbar/scrollbars/menubar
- **`init.el`** — Entry point, loads `custom.el` then requires all `codester-*` modules
- **`custom.el`** — Emacs customise system (auto-generated, avoid hand-editing)
- **`codester/`** — The core config, organised by concern:
  - `codester-bootstrap` — Post-init GC reset, macOS key mappings (Cmd=Meta), pixel scrolling
  - `codester-system` — Package repos, auth (GPG), backups, vterm, dired, direnv, project.el integration
  - `codester-ui` — Theme (Catppuccin Mocha), font (Menlo 14pt), doom-modeline, ligatures, winner-mode
  - `codester-completion` — Vertico + Orderless + Consult + Embark + Corfu + Cape + YASnippet
  - `codester-coding` — Language support (Clojure/CIDER, Go, Java, Scala via eglot), magit/forge, smartparens, treesit-auto, claude-code-ide
  - `codester-text` — Prose editing: visual-fill-column (90 chars), jinx spell-check, flymake-proselint, markdown/GFM modes
  - `codester-tools` — Tramp, elfeed (RSS reader with org-based config in `elfeed.org`)
- **`elisp/`** — Custom elisp libraries (currently `ox-confluence.el`)
- **`snippets/`** — YASnippet snippet files

## Key Design Decisions

- **Eglot over lsp-mode** — Uses built-in eglot for LSP, configured for Clojure, Java, Go, Scala
- **Completion stack** — Vertico (minibuffer) + Corfu (in-buffer) + Orderless (matching) + Consult (search commands) — no Helm/Ivy
- **Smartparens in strict mode** — Enabled globally for prog/text/lisp modes with paredit-style bindings
- **CIDER + eglot coexistence** — Custom `my/cider-capf` function merges eglot, cider, and file completions
- **Eglot signature help disabled** — `(:documentationFormat ["plaintext"])` preferred over popups

## Testing Changes

Evaluate individual forms with `C-x C-e` or reload a module buffer with `M-x eval-buffer`. For a full reload, restart Emacs. There is no automated test suite.

## Notable Keybindings

- `C-c f` — consult-ripgrep
- `C-c r` — recent files
- `C-c i` — consult-imenu
- `C-c d` — docker
- `C-c e` — elfeed
- `C-.` / `C-;` — embark-act / embark-dwim
- `C-=` — expand-region
- `` C-` `` / `C-return` — vterm-toggle
- `M-$` — jinx-correct


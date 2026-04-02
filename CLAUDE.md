# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Personal Emacs configuration for macOS, using Emacs 29+ with native compilation, tree-sitter, and xwidgets. Package management uses `use-package` with packages from MELPA.

## Conventions

- Use **British spelling** throughout comments, docstrings, and UI strings (e.g. "organised", "colour", "customise").
- All elisp files must use `lexical-binding: t` in the first line.

## Architecture

The config is split into a modular system loaded from `init.el`:

- **`early-init.el`** — Pre-GUI setup: GC threshold, disables toolbar/scrollbars/menubar, sets Catppuccin Mocha bg/fg colours before theme loads to prevent white flash on startup
- **`init.el`** — Entry point, loads `custom.el` then requires all `codester-*` modules in order
- **`custom.el`** — Emacs customise system (auto-generated, avoid hand-editing)
- **`codester/`** — The core config, organised by concern:
  - `codester-bootstrap` — Post-init GC reset (100MB), macOS key mappings (Cmd=Meta), pixel scrolling
  - `codester-system` — Package repos, auth (`~/.authinfo.gpg`), backups, vterm (with Claude Code bullet-replacement hack), dired, direnv, project.el + magit integration
  - `codester-ui` — Theme (Catppuccin Mocha), font (Menlo 14pt), doom-modeline (clickable VC segment), ligatures, winner-mode
  - `codester-completion` — Vertico + Orderless + Consult + Embark + Corfu + Cape + YASnippet
  - `codester-coding` — Language support (Clojure/CIDER, Go, Java, Scala via eglot), magit/forge, smartparens, treesit-auto, dape (DAP debugging), claude-code-ide
  - `codester-text` — Prose editing: visual-fill-column (90 chars), jinx spell-check (en_GB), flymake-proselint, markdown/GFM modes
  - `codester-tools` — Tramp, elfeed (RSS reader with org-based config in `elfeed.org`)
- **`elisp/`** — Custom elisp libraries (currently `ox-confluence.el`)
- **`snippets/`** — YASnippet snippet files

Module load order in `init.el` matters — `codester-bootstrap` must run first (sets up key mappings), `codester-system` initialises packages before anything else uses `use-package`.

## Key Design Decisions

- **Eglot over lsp-mode** — Uses built-in eglot for LSP, configured for Clojure, Java, Go, Scala
- **Completion stack** — Vertico (minibuffer) + Corfu (in-buffer) + Orderless (matching) + Consult (search commands) — no Helm/Ivy
- **Smartparens in strict mode** — Enabled globally for prog/text/lisp modes with paredit-style bindings
- **CIDER + eglot coexistence** — Custom `my/cider-capf` function uses `cape-capf-super` to merge eglot, cider, and file completions into a single CAPF
- **Eglot signature help disabled** — `(:documentationFormat ["plaintext"])` preferred over popups
- **Flicker-free startup** — `early-init.el` sets frame colours and font before GUI draws; catppuccin theme loads later in `codester-ui`
- **Java debug** — `eglot-java` configured with `java-debug` plugin bundle for DAP via `dape`

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
- `` C-` `` / `C-return` — vterm-toggle / vterm-toggle-insert-cd
- `M-$` — jinx-correct
- `C-c C-c` — compile
- `C-c C-a` / `C-c C-r` — eglot-code-actions / eglot-rename
- `C-c l t` — run test (Java via eglot-java, Python via python-pytest)
- `C-c C-'` — claude-code-ide menu
- `M-g M-n` / `M-g M-p` — next/prev flymake error


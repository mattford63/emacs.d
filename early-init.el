;;; early-init.el --- Early init -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Performance - high threshold during init, reset after
(setq gc-cons-threshold most-positive-fixnum
      read-process-output-max (* 1024 1024))

;; Prevent UI flicker
(setq inhibit-startup-screen t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push `(font . ,(if (eq system-type 'darwin) "Menlo 14" "DejaVu Sans Mono 12"))
      default-frame-alist)
(push '(ns-proxy-icon . nil) default-frame-alist)
(push '(left-fringe . 12) default-frame-alist)
(push '(background-color . "#1e1e2e") default-frame-alist)
(push '(foreground-color . "#cdd6f4") default-frame-alist)

;;; early-init.el ends here

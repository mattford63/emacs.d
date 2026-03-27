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

;;; early-init.el ends here

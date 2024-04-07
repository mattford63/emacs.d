;;; codester-tools.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Bamboo HR
(use-package ts)
(use-package a)
(use-package tablist)
(use-package bhr
  :vc (:fetcher github :repo elken/bhr.el)
  :config
  (setq bhr-org-name "juxtpro"))

;; Tramp
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; News Reader
(use-package elfeed)
(use-package elfeed-org
  :config
  (elfeed-org))

;; Pintentry
(use-package pinentry
  :config
  (pinentry-start))

;; Mu4e
;; https://macowners.club/posts/email-emacs-mu4e-macos/#storing-trusted-root-certificates
(require 'mu4e)
(require 'mu4e-views)
(require 'mu4e-column-faces)
(mu4e-column-faces-mode)
(require 'mu4e-marker-icons)
(mu4e-marker-icons-mode)
(setq mu4e-views-default-view-method "html")
(mu4e-views-mu4e-use-view-msg-method "html")
(setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
(setq mu4e-views-auto-view-selected-message t)
(define-key mu4e-headers-mode-map (kbd "v") #'mu4e-views-mu4e-select-view-msg-method)
(define-key mu4e-headers-mode-map (kbd "M-n") #'mu4e-views-cursor-msg-view-window-down)
(define-key mu4e-headers-mode-map (kbd "M-p") #'mu4e-views-cursor-msg-view-window-up)
(define-key mu4e-headers-mode-map (kbd "f") #'mu4e-views-toggle-auto-view-selected-message)
(define-key mu4e-headers-mode-map (kbd "i") #'mu4e-views-mu4e-view-as-nonblocked-html)

(setq mu4e-maildir "~/.maildir")
(setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
(setq mu4e-update-interval 300)
(setq mu4e-attachment-dir "~/Downloads")
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-user-mail-address-list '("mattford63@icloud.com"
                                    "mattford63@gmail.com"
				    "matt@dancingfrog.co.uk"
				    "mtf@juxt.pro"))
(setq mu4e-maildir-shortcuts
      '(("/icloud/INBOX" . ?i)
        ("/icloud/Sent Messages" . ?I)
        ("/gmail/INBOX" . ?g)
        ("/gmail/[Gmail]/Sent Mail" . ?G)
        ("/juxt/INBOX" . ?j)
        ("/juxt/[Gmail]/Sent Mail" . ?J)))

(add-to-list 'mu4e-bookmarks
             `(:name "Inbox - iCloud"
               :query "maildir:/icloud/INBOX"
               :key ?i) t)
(add-to-list 'mu4e-bookmarks
             `(:name "Inbox - Gmail"
               :query "maildir:/gmail/INBOX"
               :key ?g) t)
(add-to-list 'mu4e-bookmarks
             `(:name "Inbox - JUXT"
               :query "maildir:/juxt/INBOX"
               :key ?j) t)

(provide 'codester-tools)

;;; codester-tools.el ends here

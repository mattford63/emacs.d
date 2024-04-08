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

;; Pinentry
(use-package pinentry
  :config
  (pinentry-start))

;; Mu4e
;; https://macowners.club/posts/email-emacs-mu4e-macos/#storing-trusted-root-certificates
(require 'mu4e)
(keymap-global-set "C-c m" #'mu4e)
(setq mu4e-split-view 'vertical)
(setq mu4e-headers-visible-columns 120)
(setq mu4e-headers-fields '((:human-date . 12)
			    (:flags . 6)
			    (:from . 22)
			    (:subject)))
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
	("/icloud/Archive" . ?u)
        ("/juxt/INBOX" . ?j)
        ("/juxt/[Gmail]/Sent Mail" . ?J)
	("/juxt/[Gmail]/All Mail" . ?h)))

(add-to-list 'mu4e-bookmarks
             `(:name "Inbox - iCloud"
               :query "maildir:/icloud/INBOX"
               :key ?i) t)
(add-to-list 'mu4e-bookmarks
             `(:name "Inbox - JUXT"
               :query "maildir:/juxt/INBOX"
               :key ?j) t)

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "icloud"
          :enter-func
          (lambda () (mu4e-message "Enter iCloud context"))
          :leave-func
          (lambda () (mu4e-message "Leave iCloud context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg :to "matt@dancingfrog.co.uk")))
          :vars '((user-mail-address . "matt@dancingfrog.co.uk" )
                  (user-full-name . "Matt Ford")
                  (mu4e-drafts-folder . "/icloud/Drafts")
                  (mu4e-refile-folder . "/icloud/Archive")
                  (mu4e-sent-folder . "/icloud/Sent Messages")
                  (mu4e-trash-folder . "/icloud/Deleted Messages")))

        ,(make-mu4e-context
          :name "juxt"
          :enter-func
          (lambda () (mu4e-message "Enter JUXT context"))
          :leave-func
          (lambda () (mu4e-message "Leave JUXT context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "mtf@juxt.pro")))
          :vars '((user-mail-address . "mtf@juxt.pro")
                  (user-full-name . "Matt Ford")
                  (mu4e-drafts-folder . "/juxt/[Gmail]/Drafts")
                  (mu4e-refile-folder . "/juxt/[Gmail]/All Mail")
                  (mu4e-sent-folder . "/juxt/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/juxt/[Gmail]/Bin")))))

(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'ask)

(use-package mu4e-column-faces
  :config
  (mu4e-column-faces-mode))

(use-package mu4e-marker-icons
  :config
  (mu4e-marker-icons-mode))

(use-package mu4e-views
  :config
  (setq mu4e-views-default-view-method "html-nonblock")
  (mu4e-views-mu4e-use-view-msg-method "html-nonblock")
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
  (setq mu4e-views-auto-view-selected-message t)
  (define-key mu4e-headers-mode-map (kbd "v") #'mu4e-views-mu4e-select-view-msg-method)
  (define-key mu4e-headers-mode-map (kbd "M-n") #'mu4e-views-cursor-msg-view-window-down)
  (define-key mu4e-headers-mode-map (kbd "M-p") #'mu4e-views-cursor-msg-view-window-up)
  (define-key mu4e-headers-mode-map (kbd "f") #'mu4e-views-toggle-auto-view-selected-message)
  ;;(define-key mu4e-headers-mode-map (kbd "i") #'mu4e-views-mu4e-view-as-nonblocked-html)
  )


(provide 'codester-tools)

;;; codester-tools.el ends here

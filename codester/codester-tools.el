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
  (setq bhr-org-name "juxtpro")
  :bind
  ("C-c b" . 'bhr-submit-multiple))

;; Tramp
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; News Reader
(use-package elfeed
  :bind
  ("C-c e" . 'elfeed))

(use-package elfeed-org
  :config
  (elfeed-org))

;; Pinentry
(use-package pinentry
  :config
  (pinentry-start))

;; Mu4e
(require 'mu4e)
(keymap-global-set "C-c m" #'mu4e)

(defface mu4e-header-highlight-face
  `((t :inherit hl-line :weight normal :underline t
       ,@(and (>= emacs-major-version 27) '(:extend t))))
  "Face for the header at point."
  :group 'mu4e-faces)

(setq mu4e-split-view 'horizontal)
(setq mu4e-headers-visible-columns 120)
(setq mu4e-headers-fields '((:date . 12)
			    (:flags . 6)
			    (:from . 22)
			    (:subject)))
(setq mu4e-maildir "~/.maildir")
(setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
(setq mu4e-update-interval 300)
(setq mu4e-attachment-dir "~/downloads")
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
             `(:name "Griffo"
	       :query "sera"
               :key ?s) t)

(require 'smtpmail)
(setq message-kill-buffer-on-exit t)
(setq message-sendmail-envelope-from 'header)

;; use .authinfo.gpg for host:port:user:pass settings
(setq
 message-send-mail-function 'smtpmail-send-it
 smtpmail-stream-type 'starttls
 smtpmail-default-smtp-server "smtp.mail.me.com"
 smtpmail-smtp-service 587
 smtpmail-servers-requiring-authorization ".*")

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
		  (mu4e-sent-messages-behavior . (lambda () 'sent))
                  (mu4e-sent-folder . "/icloud/Sent Messages")
                  (mu4e-trash-folder . "/icloud/Deleted Messages")
		  (smtpmail-smtp-server . "smtp.mail.me.com")))
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
		  (mu4e-sent-messages-behavior . (lambda () 'delete))
		  (mu4e-sent-folder . "/juxt/[Gmail]/Sent Mail")
		  (mu4e-trash-folder . "/juxt/[Gmail]/Bin")
		  (smtpmail-smtp-server . "smtp.gmail.com")))))

(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'ask)

(defun message-modes ()
  "Load message modes."
  (progn
    (jinx-mode)
    (flymake-mode)
    (flymake-proselint-setup)))

(add-hook 'message-mode-hook 'message-modes)

(use-package mu4e-column-faces
  :config
  (mu4e-column-faces-mode))

(use-package mu4e-marker-icons
  :config
  (mu4e-marker-icons-mode))

;; (use-package mu4e-views
;;   :config
;;   (setq mu4e-views-default-view-method "html-nonblock")
;;   (mu4e-views-mu4e-use-view-msg-method "html-nonblock")
;;   (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
;;   (setq mu4e-views-auto-view-selected-message t)
;;   (define-key mu4e-headers-mode-map (kbd "v") #'mu4e-views-mu4e-select-view-msg-method)
;;   (define-key mu4e-headers-mode-map (kbd "M-n") #'mu4e-views-cursor-msg-view-window-down)
;;   (define-key mu4e-headers-mode-map (kbd "M-p") #'mu4e-views-cursor-msg-view-window-up)
;;   (define-key mu4e-headers-mode-map (kbd "f") #'mu4e-views-toggle-auto-view-selected-message)
;;   (define-key mu4e-headers-mode-map (kbd "C-c C-=") #'xwidget-webkit-zoom-in)
;;   (define-key mu4e-headers-mode-map (kbd "C-c C--") #'xwidget-webkit-zoom-out)
;;   (define-key xwidget-webkit-mode-map (kbd "C-c C-=") #'xwidget-webkit-zoom-in)
;;   (define-key xwidget-webkit-mode-map (kbd "C-c C--") #'xwidget-webkit-zoom-out)

;;   ;;(define-key mu4e-headers-mode-map (kbd "i") #'mu4e-views-mu4e-view-as-nonblocked-html)
;;   )

(provide 'codester-tools)

;;; codester-tools.el ends here

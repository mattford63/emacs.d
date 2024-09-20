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

(use-package elfeed-webkit
  :init
  (setq elfeed-webkit-auto-enable-tags '(webkit))
  :config
  (elfeed-webkit-auto-toggle-by-tag)
  :bind (:map elfeed-show-mode-map
              ("C-%" . elfeed-webkit-toggle)
	      ("C-c C-=" . xwidget-webkit-zoom-in)
	      ("C-c C--" . xwidget-webkit-zoom-out))
  :after elfeed)

;; Pinentry
(use-package pinentry
  :config
  (pinentry-start))

;; ;; Mu4e
;; ;; mu init -m ~/.maildir --my-address matt@dancingfrog.co.uk --my-address mattford63@icloud.com --my-address mattford63@gmail.com --my-address mtf@juxt.pro
;; (use-package mu4e
;;   :ensure nil
;;   :config
;;   (keymap-global-set "C-c m" #'mu4e)

;;   (setq mu4e-split-view 'horizontal)
;;   (setq mu4e-headers-visible-columns 120)
;;   (setq mu4e-headers-fields '((:date . 12)
;; 			      (:flags . 6)
;; 			      (:from . 22)
;; 			      (:subject)))
;;   (setq mu4e-maildir "~/.maildir")
;;   (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
;;   (setq mu4e-update-interval 300)
;;   (setq mu4e-attachment-dir "~/downloads")
;;   (setq mu4e-change-filenames-when-moving t)
;;   (setq mu4e-user-mail-address-list '("mattford63@icloud.com"
;;                                       "mattford63@gmail.com"
;; 				      "matt@dancingfrog.co.uk"
;; 				      "mtf@juxt.pro"))
;;   (setq mu4e-maildir-shortcuts
;; 	'(("/icloud/INBOX" . ?i)
;;           ("/icloud/Sent Messages" . ?I)
;; 	  ("/icloud/Archive" . ?u)
;;           ("/juxt/INBOX" . ?j)
;;           ("/juxt/[Gmail]/Sent Mail" . ?J)
;; 	  ("/juxt/[Gmail]/All Mail" . ?h)))

;;   (add-to-list 'mu4e-bookmarks
;;                `(:name "Griffo"
;; 		       :query "sera"
;; 		       :key ?s) t)

;;   (require 'smtpmail)
;;   (setq message-kill-buffer-on-exit t)
;;   (setq message-sendmail-envelope-from 'header)

;;   ;; use .authinfo.gpg for host:port:user:pass settings
;;   (setq
;;    message-send-mail-function 'smtpmail-send-it
;;    smtpmail-stream-type 'starttls
;;    smtpmail-default-smtp-server "smtp.mail.me.com"
;;    smtpmail-smtp-service 587
;;    smtpmail-servers-requiring-authorization ".*")

;;   (setq mu4e-contexts
;; 	`(,(make-mu4e-context
;;             :name "icloud"
;;             :enter-func
;;             (lambda () (mu4e-message "Enter iCloud context"))
;;             :leave-func
;;             (lambda () (mu4e-message "Leave iCloud context"))
;;             :match-func
;;             (lambda (msg)
;;               (when msg
;; 		(mu4e-message-contact-field-matches msg :to "matt@dancingfrog.co.uk")))
;;             :vars '((user-mail-address . "matt@dancingfrog.co.uk" )
;;                     (user-full-name . "Matt Ford")
;;                     (mu4e-drafts-folder . "/icloud/Drafts")
;;                     (mu4e-refile-folder . "/icloud/Archive")
;; 		    (mu4e-sent-messages-behavior . sent)
;;                     (mu4e-sent-folder . "/icloud/Sent Messages")
;;                     (mu4e-trash-folder . "/icloud/Deleted Messages")
;; 		    (smtpmail-smtp-server . "smtp.mail.me.com")))
;; 	  ,(make-mu4e-context
;;             :name "juxt"
;;             :enter-func
;;             (lambda () (mu4e-message "Enter JUXT context"))
;;             :leave-func
;;             (lambda () (mu4e-message "Leave JUXT context"))
;;             :match-func
;;             (lambda (msg)
;;               (when msg
;; 		(mu4e-message-contact-field-matches msg
;;                                                     :to "mtf@juxt.pro")))
;;             :vars '((user-mail-address . "mtf@juxt.pro")
;;                     (user-full-name . "Matt Ford")
;;                     (mu4e-drafts-folder . "/juxt/[Gmail]/Drafts")
;;                     (mu4e-refile-folder . "/juxt/[Gmail]/All Mail")
;; 		    (mu4e-sent-messages-behavior . delete)
;; 		    (mu4e-sent-folder . "/juxt/[Gmail]/Sent Mail")
;; 		    (mu4e-trash-folder . "/juxt/[Gmail]/Bin")
;; 		    (smtpmail-smtp-server . "smtp.gmail.com")))))

;;   (setq mu4e-context-policy 'pick-first)
;;   (setq mu4e-compose-context-policy 'ask)

;;   (defun message-modes ()
;;     "Load message modes."
;;     (progn
;;       (jinx-mode)
;;       (flymake-mode)
;;       (flymake-proselint-setup)))

;;   (add-hook 'message-mode-hook 'message-modes)
;;   (with-eval-after-load "mm-decode"
;;     (add-to-list 'mm-discouraged-alternatives "text/html")
;;     (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;; (use-package mu4e-column-faces
;;   :config
;;   (mu4e-column-faces-mode)
;;   (custom-set-faces
;;    `(mu4e-header-highlight-face
;;      ((t :inherit hl-line :weight normal :underline t
;; 	 ,@(and (>= emacs-major-version 27) '(:extend t)))))))

;; (use-package mu4e-marker-icons
;;   :ensure
;;   :config
;;   (mu4e-marker-icons-mode)
;;   :after mu4e)

;; (use-package mu4e-views
;;   :config
;;   (setq mu4e-views-default-view-method "html-nonblock")
;;   (mu4e-views-mu4e-use-view-msg-method "html-nonblock")
;;   (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
;;   (setq mu4e-views-auto-view-selected-message t)
;;   (defun toggle-mu4e-view ()
;;     "Toggle gnus view or views view"
;;     (interactive)
;;     (if mu4e-views--advice-installed
;; 	(mu4e-views-unload-function)
;;       (mu4e-views-advice-mu4e)))
;;   :bind ((:map mu4e-headers-mode-map
;; 	       ("v" . mu4e-views-mu4e-select-view-msg-method)
;; 	       ("M-n" . mu4e-views-cursor-msg-view-window-down)
;; 	       ("M-p" . mu4e-views-cursor-msg-view-window-up)
;; 	       ("f" . mu4e-views-toggle-auto-view-selected-message)
;; 	       ("C-%" . toggle-mu4e-view)
;; 	       ("C-c C-=" . xwidget-webkit-zoom-in)
;; 	       ("C-c C--" . xwidget-webkit-zoom-out))
;; 	 (:map mu4e-view-mode-map
;; 	       ("C-%" . toggle-mu4e-view))
;; 	 (:map mu4e-views-view-actions-mode-map
;; 	       ("C-%" . toggle-mu4e-view)))
;;   ;;(define-key mu4e-headers-mode-map (kbd "i") #'mu4e-views-mu4e-view-as-nonblocked-html)
;;   :after mu4e
;;   )

(provide 'codester-tools)

;;; codester-tools.el ends here

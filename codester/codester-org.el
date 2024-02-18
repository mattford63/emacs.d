(use-package org
  :init
  (defun jcs-retrieve-url ()
    "Retrieve the URL of the current Safari page as a string."
    (do-applescript "tell application \"Safari\" to return URL of document 1"))
  
  (defun jcs-get-link (hostp)
    "Retrieve URL from current Safari page and prompt for description.
   With the universal argument, return a link to the host only.
   Insert an Org link at point."
    (interactive "P")
    (let* ((link (read-from-minibuffer "Link Description: "))
           (result (jcs-retrieve-url))
           (urlobj (url-generic-parse-url result))
           (host (concat (url-type urlobj) "://" (url-host urlobj))))
      (insert (format "[[%s][%s]]" (if hostp host (org-trim result)) link))))
  
  (defun org-hide-properties ()
    "Hide all org-mode headline property drawers in buffer. Could be
     slow if it has a lot of overlays."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
	(let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov_this 'display "")
          (overlay-put ov_this 'hidden-prop-drawer t))))
    (put 'org-toggle-properties-hide-state 'state 'hidden))
  
  (defun org-show-properties ()
    "Show all org-mode property drawers hidden by org-hide-properties."
    (interactive)
    (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
    (put 'org-toggle-properties-hide-state 'state 'shown))
  
  (defun org-toggle-properties ()
    "Toggle visibility of property drawers."
    (interactive)
    (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
	(org-show-properties)
      (org-hide-properties)))
  
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c M-l" . jcs-get-link)
   ;;("C-c p" . org-toggle-properties)
   )
  :config
  (setq org-agenda-files '("~/src/org/")
	org-refile-targets '((org-agenda-files :maxlevel . 3))
	org-image-actual-width nil
	org-confirm-babel-evaluate nil
	org-src-preserve-indentation t
	org-cycle-hide-drawer-startup t
	org-image-actual-width (list 450))
  :hook
  (org-mode . (lambda () (progn (text-modes) (org-hide-properties) (org-display-inline-images)))))

(use-package org-roam
  :after org magit
  :config
  (setq org-roam-directory (file-truename "~/src/org/roam"))
  (org-roam-db-autosync-mode)
  (require 'org-roam-dailies)
  :init
  (add-to-list 'magit-section-initial-visibility-alist (cons 'org-roam-node-section 'hide))
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
  (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         :map org-mode-map
         ("C-c n i" . org-roam-node-insert)
         ("C-c n o" . org-id-get-create)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n l" . org-roam-buffer-toggle)
	 :map org-roam-dailies-map
	 ("C-Y" . org-roam-dailies-capture-yesterday)
	 ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap ("C-c n d" . org-roam-dailies-map))

(use-package mermaid-mode)
(use-package ob-mermaid)

(use-package phscroll
  :vc (:fetcher github :repo misohena/phscroll)
  :after org
  :config
  (with-eval-after-load "org"
    (require 'org-phscroll)))

(use-package websocket
  :after org-roam)

(use-package org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)
    :bind ((:map org-mode-map
		 (("C-c n g" . org-roam-ui-mode)))))

(use-package org-download
  :config
  (setq org-download-screenshot-method "screencapture -i %s"))

(use-package org-roam
  :vc (:fetcher github :repo org-roam/org-roam))

(use-package orgit)

(require 'ox-confluence)

(provide 'codester-org)

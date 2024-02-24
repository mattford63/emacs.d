;;; codester-bootstrap.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(message "Hello World!")

(provide 'codester-bootstrap)

;;; codester-bootstrap.el ends here

;; Performance
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;; Modifier keys
;; (setq mac-option-key-is-meta nil)
;; (setq mac-command-key-is-meta t)
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier nil)

(provide 'codester-bootstrap)

;;; codester-bootstrap.el ends here

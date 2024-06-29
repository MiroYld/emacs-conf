;; Packages configuration
(require 'calfw)
(require 'calfw-org)
(require 'org)
(require 'mu4e)

;; Configuration mu4e
(use-package mu4e
  :ensure nil
  :config
  (setq mu4e-change-filenames-when-moving t
	mu4e-update-interval (* 1 60)
	mu4e-get-mail-command "mbsync -a"
	mu4e-maildir "~/Mail"
	mu4e-compose-format-flowed t
	message-send-mail-function 'smtpmail-send-it)

  (setq mu4e-contexts
	(list
	 (make-mu4e-context
	  :name "Work"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "contact@cuteops.fr")
		  (user-full-name    . "Firat Yildirim")
		  (smtpmail-smtp-server  . "smtp.gmail.com")
		  (smtpmail-smtp-service . 465)
		  (smtpmail-stream-type  . ssl)
		  (mu4e-drafts-folder  . "/Gmail/[Gmail]/Drafts")
		  (mu4e-sent-folder  . "/Gmail/[Gmail]/Sent Mail")
		  (mu4e-refile-folder  . "/Gmail/[Gmail]/All Mail")
		  (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")))))

  (setq mu4e-maildir-shortcuts
	'(("/Gmail/Inbox"             . ?i)
	  ("/Gmail/[Gmail]/Sent Mail" . ?s)
	  ("/Gmail/[Gmail]/Trash"     . ?t)
	  ("/Gmail/[Gmail]/Drafts"    . ?d)
	  ("/Gmail/[Gmail]/All Mail"  . ?a))))

(global-set-key (kbd "C-c m") 'mu4e)

(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

(defun tws-mu4e-save-all-fix (orig-fun &rest args)
  "Temporarily turn on helm-mode for selecting files."
  (helm-mode)
  (apply orig-fun args)
  (helm-mode -1))

(advice-add 'mu4e-view-save-attachments :around #'tws-mu4e-save-all-fix)

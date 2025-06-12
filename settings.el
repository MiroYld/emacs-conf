;; global settings
(setq auto-save-delete-trailing-whitespace t
      auto-save-silent t
      c-set-style "ellemtel"
      company-idle-delay 0.1
      company-minimum-prefix-length 1
      create-lockfiles nil
      custom-file "~/.emacs.d/my-custom.el"
      display-line-numbers-type t
      docker-command "/usr/bin/docker"
      history-length 100
      inhibit-splash-screen t
      inhibit-startup-message t
      lsp-idle-delay 0.1
      lsp-prefer-capf t
      magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
      make-backup-files nil
      nyan-animate-nyancat t
      tramp-default-method "ssh"
      undo-tree-enable-undo-in-region nil
      vc-follow-symlinks t
      vertico-buffer-mode 1
      window-divider-default-right-width 1
      savehist-file "~/.emacs.d/savehist"
      savehist-save-minibuffer-history t
      auto-save-default nil
      savehist-autosave-interval 60
      savehist-additional-variables '(kill-ring
				      search-ring
				      regexp-search-ring))
(custom-set-variables
 '(vlf-application 'dont-ask))

;; disable auto save mode when current filetype is an gpg file.
(setq auto-save-disable-predicates
      '((lambda ()
	  (string-suffix-p
	   "gpg"
	   (file-name-extension (buffer-name)) t))))

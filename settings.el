;; Réglages globaux
(setq auto-save-default nil
      c-set-style "ellemtel"
      company-idle-delay 0.1
      company-minimum-prefix-length 1
      create-lockfiles nil
      display-line-numbers-type t
      history-length 100
      inhibit-splash-screen t
      inhibit-startup-message t
      lsp-idle-delay 0.1
      lsp-prefer-capf t
      magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
      make-backup-files nil
      undo-tree-enable-undo-in-region nil
      nyan-animate-nyancat t
      vc-follow-symlinks t
      vertico-buffer-mode 1
      window-divider-default-right-width 1
      savehist-file "~/.emacs.d/savehist"
      savehist-save-minibuffer-history t
      savehist-autosave-interval 60
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring))

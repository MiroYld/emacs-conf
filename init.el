;; Démarrer Emacs en plein écran
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Masquer la barre de menu, la barre d'outils et la scroll bar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq auto-save-default nil
      create-lockfiles nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1
      inhibit-splash-screen t
      inhibit-startup-message t
      vc-follow-symlinks t
      make-backup-files nil
      lsp-prefer-capf t
      ccls-executable "/usr/bin/ccls"
      company-idle-delay 0
      custom-file "~/.emacs.d/my-custom.el")

(defalias 'yes-or-no-p 'y-or-n-p)
(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(with-eval-after-load 'ivy 
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        ivy-virtual-abbreviate 'full
        counsel-find-file-ignore-regexp "\\.go\\'"
        enable-recursive-minibuffers t
        recentf-max-saved-items nil))

;; Charger le thème Doom
(load-theme 'doom-dracula t)

(ivy-mode 1)
(counsel-mode)
(evil-mode)
(savehist-mode)
(nyan-mode 1)
(yas-global-mode 1)
(solaire-global-mode +1)

;; conf lsp
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

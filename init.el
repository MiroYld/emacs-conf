;; Démarrer Emacs en plein écran
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Masquer la barre de menu, la barre d'outils et la scroll bar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq auto-save-default nil
      create-lockfiles nil
      inhibit-splash-screen t
      inhibit-startup-message t
      make-backup-files nil
      custom-file "~/.emacs.d/my-custom.el")

(defalias 'yes-or-no-p 'y-or-n-p)

;; Charger le thème Doom
(load-theme 'doom-one t)

(ivy-mode)
(evil-mode)
(savehist-mode)
(nyan-mode 1)

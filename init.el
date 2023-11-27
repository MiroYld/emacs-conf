;; Démarrer Emacs en plein écran
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Masquer la barre de menu, la barre d'outils et la scroll bar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Désactiver la sauvegarde automatique des fichiers et la création de fichiers de verrouillage
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Démarrer avec un buffer vide
(setq inhibit-splash-screen t
      inhibit-startup-message t)

;; Charger le thème Doom
(load-theme 'doom-one t)

;; Configuration du gestionnaire de paquets MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Installer et charger les paquets nécessaires
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))
(require 'magit)

(unless (package-installed-p 'nyan-mode)
  (package-refresh-contents)
  (package-install 'nyan-mode))
(require 'nyan-mode)
(nyan-mode 1)

(unless (package-installed-p 'flycheck-yamllint)
  (package-refresh-contents)
  (package-install 'flycheck-yamllint))
(require 'flycheck-yamllint)

(unless (package-installed-p 'docker-compose-mode)
  (package-refresh-contents)
  (package-install 'docker-compose-mode))
(require 'docker-compose-mode)

(require 'lsp-mode)
(add-hook 'prog-mode-hook #'lsp)

(require 'ccls)

;; Utiliser des raccourcis clavier classiques pour copier, couper, coller et annuler
(global-set-key (kbd "C-c") 'kill-ring-save) ;; Ctrl + C pour copier
(global-set-key (kbd "C-v") 'yank)           ;; Ctrl + V pour coller
(global-set-key (kbd "C-z") 'undo)           ;; Ctrl + Z pour annuler
(global-set-key (kbd "C-a") 'mark-whole-buffer) ;; Ctrl + A pour tout sélectionner

;; Activer la sauvegarde automatique à chaque modification
(auto-save-mode t)

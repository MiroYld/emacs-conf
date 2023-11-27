;; Démarrer Emacs en plein écran
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Masquer la barre de menu
(menu-bar-mode -1)

;;Désactiver la scroll bar
(scroll-bar-mode -1)

;; Masquer la barre d'outils
(tool-bar-mode -1)

;; Désactiver la sauvegarde automatique des fichiers
(setq make-backup-files nil) ; Ne pas créer de fichiers ~
(setq auto-save-default nil) ; Ne pas créer de fichiers # 

;; Désactiver la création de fichiers de verrouillage
(setq create-lockfiles nil) ; Ne pas créer de fichiers .#

;; Démarrer avec un buffer vide
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Charger le thème Doom
(load-theme 'doom-one t)

;; Configuration du gestionnaire de paquets MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Vérifier et installer Magit si ce n'est pas déjà fait
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))
(require 'magit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nyan-mode t)
 '(package-selected-packages
   '(lsp-mode docker-compose-mode flycheck-yamllint flycheck-yaml nyan-mode vterm doom-themes magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Installer et charger nyan-mode
(unless (package-installed-p 'nyan-mode)
  (package-refresh-contents)
  (package-install 'nyan-mode))
(require 'nyan-mode)
(nyan-mode 1)

;; Installer et charger flycheck pour YAML
(unless (package-installed-p 'flycheck-yamllint)
  (package-refresh-contents)
  (package-install 'flycheck-yamllint))
(require 'flycheck-yamllint)

;; Installer et charger docker-compose-mode
(unless (package-installed-p 'docker-compose-mode)
  (package-refresh-contents)
  (package-install 'docker-compose-mode))
(require 'docker-compose-mode)

;; Charger Lsp-mode
(require 'lsp-mode)
(add-hook 'prog-mode-hook #'lsp)

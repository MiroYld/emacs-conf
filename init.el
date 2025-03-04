(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/ui.el")
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/hooks.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/xmake.el")
(load "~/.emacs.d/gitlab-ci.el")
(load "~/.emacs.d/auto-save.el")

;; EAF -> https://github.com/emacs-eaf/emacs-application-framework#install
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
(require 'eaf-markdown-previewer)
(require 'eaf-airshare)
(require 'eaf-markdown-previewer)
(require 'eaf-image-viewer)
(require 'eaf-system-monitor)
(require 'eaf-video-player)
(require 'eaf-pdf-viewer)
(require 'eaf-file-sender)
(require 'eaf-map)
(require 'eaf-org-previewer)

;; Auto save
(require 'auto-save)
(auto-save-enable)

(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)

;; disable auto save mode when current filetype is an gpg file.
(setq auto-save-disable-predicates
      '((lambda ()
      (string-suffix-p
      "gpg"
      (file-name-extension (buffer-name)) t))))

;; Others conf
(counsel-mode)
(desktop-environment-mode)
(doom-modeline-mode 1)
(evil-mode)
(global-display-line-numbers-mode)
(ivy-mode 1)
(ivy-rich-mode 1)
(savehist-mode)
(smartparens-global-mode t)
(solaire-global-mode +1)
(toggle-frame-fullscreen)
(vertico-mode 1)
(vertico-posframe-mode 1)
(yas-global-mode 1)
(pdf-tools-install)
(pdf-loader-install)
(display-battery-mode 1)
(nyan-mode 1)
(global-git-gutter-mode t)

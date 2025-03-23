(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")

(load "~/.emacs.d/hooks.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/ui.el")
(load "~/.emacs.d/xmake.el")

(require 'eaf)
(require 'eaf-airshare)
(require 'eaf-file-sender)
(require 'eaf-image-viewer)
(require 'eaf-map)
(require 'eaf-markdown-previewer)
(require 'eaf-markdown-previewer)
(require 'eaf-org-previewer)
(require 'eaf-pdf-viewer)
(require 'eaf-system-monitor)
(require 'eaf-video-player)

;; Others conf
(counsel-mode)
(desktop-environment-mode)
(display-battery-mode 1)
(display-time-mode 1)
(doom-modeline-mode 1)
(evil-mode)
(global-display-line-numbers-mode)
(global-git-gutter-mode t)
(ivy-mode 1)
(ivy-rich-mode 1)
(nyan-mode 1)
(pdf-loader-install)
(pdf-tools-install)
(savehist-mode)
(smartparens-global-mode t)
(solaire-global-mode +1)
(toggle-frame-fullscreen)
(vertico-mode 1)
(vertico-posframe-mode 1)
(yas-global-mode 1)

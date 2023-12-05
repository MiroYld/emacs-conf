;; Masquer la barre de menu, la barre d'outils et la scroll bar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq auto-save-default nil
      create-lockfiles nil
      company-idle-delay 0.1
      company-minimum-prefix-length 1
      lsp-idle-delay 0.2
      c-set-style "ellemtel"
      inhibit-splash-screen t
      magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
      inhibit-startup-message t
      nyan-animate-nyancat t
      history-length 20
      vc-follow-symlinks t
      make-backup-files nil
      lsp-prefer-capf t
      ccls-executable "/usr/bin/ccls"
      company-idle-delay 0
      custom-file "~/.emacs.d/my-custom.el"
      ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) "
      )

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

(load-theme 'doom-acario-light t)

(ivy-mode 1)
(counsel-mode)
(evil-mode)
(savehist-mode)
(nyan-mode 1)
(yas-global-mode 1)
(solaire-global-mode)
(global-display-line-numbers-mode)
(smartparens-global-mode t)
(toggle-frame-fullscreen)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

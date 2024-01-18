; Masquer la barre de menu, la barre d'outils et la scroll bar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(exwm-enable)

(defvar efs/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun efs/kill-panel ()
  (interactive)
  (when efs/polybar-process
    (ignore-errors
      (kill-process efs/polybar-process)))
  (setq efs/polybar-process nil))

(defun efs/start-panel ()
  (interactive)
  (efs/kill-panel)
  (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))
(efs/start-panel)

(setq auto-save-default nil
      create-lockfiles nil
      company-idle-delay 0.1
      company-minimum-prefix-length 1
      doom-modeline-icon nil
      dracula-alternate-mode-line-and-minibuffer t
      lsp-idle-delay 0.2
      c-set-style "ellemtel"
      display-line-numbers-type t
      inhibit-splash-screen t
      magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
      inhibit-startup-message t
      history-length 20
      vc-follow-symlinks t
      vertico-buffer-mode 1
      make-backup-files nil
      lsp-prefer-capf t
      ccls-executable "/usr/bin/ccls")

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

(require 'exwm)
(require 'exwm-config)
(setq window-divider-default-right-width 1)
(window-divider-mode)

(defun start-nautilus ()
  (interactive)
  (start-process "" nil "nautilus"))
(exwm-input-set-key (kbd "M-<f1>") #'start-nautilus)

(defun start-google ()
  (interactive)
  (start-process "" nil "google-chrome"))
(exwm-input-set-key (kbd "M-<f2>") #'start-google)

(defun start-discord ()
  (interactive)
  (start-process "" nil "discord"))
(exwm-input-set-key (kbd "M-<f3>") #'start-discord)

(ivy-mode 1)
(doom-modeline-mode 1)
(display-battery-mode 1)
(vertico-mode 1)
(vertico-posframe-mode 1)
(counsel-mode)
(evil-mode)
(savehist-mode)
(yas-global-mode 1)
(global-display-line-numbers-mode)
(smartparens-global-mode t)
(toggle-frame-fullscreen)

(with-eval-after-load 'evil
  (setq evil-want-C-i-jump nil
	evil-symbol-word-search t
	evil-insert-state-modes nil
	evil-motion-state-modes nil
	evil-move-cursor-back nil
	evil-kill-on-visual-paste nil
	evil-move-cursor-back t)

  (mapc (lambda (mode)
	  (evil-set-initial-state mode 'normal))
	'(fundamental-mode prog-mode text-mode conf-mode diff-mode))

  (fset 'evil-visual-update-x-selection 'ignore)

  (setq-default evil-symbol-word-search t)
  (defalias 'forward-evil-word 'forward-evil-symbol)

(add-hook 'yaml-mode-hook #'ansible-doc-mode)
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps))
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(add-hook 'exwm-init-hook (lambda () (start-process-shell-command "compton" nil "compton --backend glx --vsync opengl-swc")))
(add-hook 'exwm-init-hook (lambda () (start-process-shell-command "pasystray" nil "pasystray")))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(defvar efs/frame-transparency '(80 . 80))
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                multi-vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)

(exwm-randr-enable)
(exwm-enable)
(window-divider-mode)
(server-start)										        	   	      		 
(call-process "/bin/bash" "/home/miro/personal/emacs-conf/polybar/start_polybar.sh")


(defun efs/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil  "feh --bg-scale /home/miro/Pictures/bg.jpg"))

(start-process-shell-command "xrandr" nil "")
(efs/set-wallpaper)

(global-set-key (kbd "C-<f12>") 'suspend-computer)
(defun suspend-computer ()
  (interactive)
  (shell-command "i3lock-fancy && systemctl suspend"))

(global-set-key (kbd "C-c t") 'multi-vterm)

(setq auto-save-default nil
      create-lockfiles nil
      company-idle-delay 0.1
      company-minimum-prefix-length 1
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
      ccls-executable "/usr/bin/ccls"
      window-divider-default-right-width 1)

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

(load-theme 'doom-dracula t)

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
(counsel-mode)
(desktop-environment-mode)
(doom-modeline-mode 1)
(evil-mode)
(global-display-line-numbers-mode)
(ivy-mode 1)
(savehist-mode)
(smartparens-global-mode t)
(solaire-global-mode +1)
(toggle-frame-fullscreen)
(vertico-mode 1)
(vertico-posframe-mode 1)
(yas-global-mode 1)
(pdf-tools-install)
(pdf-loader-install) 

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

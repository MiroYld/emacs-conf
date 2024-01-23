;; Start the exwm window manager and launch certain processes on initialization
(add-hook 'exwm-init-hook
          (lambda ()
            (start-process-shell-command "compton" nil "compton --backend glx --vsync opengl-swc")
            (start-process-shell-command "pasystray" nil "pasystray")))

;; Disable menu, scroll, and tool bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Set transparency for Emacs frames
(defvar efs/frame-transparency '(80 . 80))
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))

;; Disable line numbers for certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                multi-vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Load exwm and related configurations
(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)

;; Enable exwm randr and exwm
(exwm-randr-enable)
(exwm-enable)

;; Enable window divider mode
(window-divider-mode)

;; Start Emacs server
(server-start)

;; Call a shell process to start Polybar
;; Replace by correct path
(call-process "/bin/bash" "/home/miro/personal/emacs-conf/start_polybar.sh")

;; Function to set wallpaper using feh
(defun efs/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil  "feh --bg-scale /home/miro/personal/emacs-conf/img/background.jpg"))

;; Run xrandr in a shell command process
(start-process-shell-command "xrandr" nil "")

;; Set wallpaper using the defined function
(efs/set-wallpaper)

;; Global keybinding to suspend the computer with i3lock-fancy and systemctl suspend
(global-set-key (kbd "C-<f12>") 'suspend-computer)
(defun suspend-computer ()
  (interactive)
  (shell-command "i3lock-fancy && systemctl suspend"))

;; Global keybinding to open a multi-vterm
(global-set-key (kbd "C-c t") 'multi-vterm)

(setq auto-save-default nil                        ; Disable auto-save
      create-lockfiles nil                         ; Disable lockfiles
      company-idle-delay 0.1                       ; Company mode idle delay
      company-minimum-prefix-length 1              ; Company mode minimum prefix length
      dracula-alternate-mode-line-and-minibuffer t ; Use alternate mode line and minibuffer for Dracula theme
      lsp-idle-delay 0.2                           ; LSP mode idle delay
      c-set-style "ellemtel"                       ; Set C-style to "ellemtel"
      display-line-numbers-type t                  ; Display line numbers
      inhibit-splash-screen t                      ; Inhibit splash screen at startup
      inhibit-startup-message t                    ; Inhibit startup message
      history-length 20                            ; Set command history length
      vc-follow-symlinks t                         ; Follow symlinks in version control
      vertico-buffer-mode 1                        ; Enable vertico buffer mode
      make-backup-files nil                        ; Disable backup files
      lsp-prefer-capf t                            ; Prefer capf for LSP completion
      ccls-executable "/usr/bin/ccls"              ; Set the ccls executable path
      window-divider-default-right-width 1         ; Set window divider right width
      )

(defalias 'yes-or-no-p 'y-or-n-p)      ; Use 'y' and 'n' instead of 'yes' and 'no'

;; Enable paredit mode for Emacs Lisp
(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

;; Ivy configuration
(with-eval-after-load 'ivy
  (setq ivy-use-virtual-buffers t                         ; Use virtual buffers in Ivy
	ivy-re-builders-alist '((swiper . ivy--regex-plus)
				(t . ivy--regex-fuzzy))   ; Regex builders for Ivy
	ivy-virtual-abbreviate 'full                      ; Abbreviate virtual buffers
	counsel-find-file-ignore-regexp "\\.go\\'"        ; Ignore .go files in counsel-find-file
	enable-recursive-minibuffers t                    ; Enable recursive minibuffers
	recentf-max-saved-items nil                       ; Limit the number of recent files saved in recentf
	))

;; Load dracula theme
(load-theme 'doom-dracula t)

;; Function to start Nautilus file manager
(defun start-nautilus ()
  (interactive)
  (start-process "" nil "nautilus"))
(exwm-input-set-key (kbd "M-<f1>") #'start-nautilus)

;; Function to start Google Chrome
(defun start-google ()
  (interactive)
  (start-process "" nil "google-chrome"))
(exwm-input-set-key (kbd "M-<f2>") #'start-google)

;; Function to start Discord
(defun start-discord ()
  (interactive)
  (start-process "" nil "discord"))
(exwm-input-set-key (kbd "M-<f3>") #'start-discord)

(counsel-mode)                     ; Enable counsel mode
(desktop-environment-mode)         ; Enable desktop environment mode
(doom-modeline-mode 1)             ; Enable doom modeline
(evil-mode)                        ; Enable evil mode
(global-display-line-numbers-mode) ; Enable global display line numbers mode
(ivy-mode 1)                       ; Enable ivy mode
(savehist-mode)                    ; Enable savehist mode
(smartparens-global-mode t)        ; Enable global smartparens mode
(solaire-global-mode +1)           ; Enable solaire mode
(toggle-frame-fullscreen)          ; Toggle frame fullscreen
(vertico-mode 1)                   ; Enable vertico mode
(vertico-posframe-mode 1)          ; Enable vertico-posframe mode
(yas-global-mode 1)                ; Enable yasnippet global mode
(pdf-tools-install)                ; Install and enable pdf-tools
(pdf-loader-install)               ; Install and enable pdf-loader

;; Evil configuration
(with-eval-after-load 'evil
  (setq evil-want-C-i-jump nil             ; Disable C-i jump
        evil-symbol-word-search t          ; Use symbol-based word search
        evil-insert-state-modes nil        ; Disable insert state
        evil-motion-state-modes nil        ; Disable motion state
        evil-move-cursor-back nil          ; Disable move cursor back in insert mode
        evil-kill-on-visual-paste nil      ; Disable killing on visual paste
        evil-move-cursor-back t            ; Move cursor back in normal state
	)

  ;; Set initial state to 'normal' for certain modes
  (mapc (lambda (mode)
          (evil-set-initial-state mode 'normal))
        '(fundamental-mode prog-mode text-mode conf-mode diff-mode))

  (fset 'evil-visual-update-x-selection 'ignore) ; Ignore visual update to X selection

  (setq-default evil-symbol-word-search t) ; Set symbol-based word search globally
  (defalias 'forward-evil-word 'forward-evil-symbol) ; Alias forward-evil-word to forward-evil-symbol

  ;; Enable ansible-doc-mode for yaml-mode
  (add-hook 'yaml-mode-hook #'ansible-doc-mode)

  ;; Normalize evil keymaps in edebug-mode
  (add-hook 'edebug-mode-hook 'evil-normalize-keymaps)
  )

;; LSP configuration for C and C++ modes
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

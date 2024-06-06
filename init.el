;; Adding xmake plugin
(load "~/.emacs.d/xmake.el")

;; Disable menu, scroll, and tool bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Set font size
(set-face-attribute 'default nil :font "Ubuntu Mono-12" :weight 'normal)

(setq auto-save-default nil		; Disable auto-save
      c-set-style "ellemtel"		; Set C-style to "ellemtel"
      ccls-executable "/usr/bin/ccls"	; Set the ccls executable path
      company-idle-delay 0.1		; Company mode idle delay
      company-minimum-prefix-length 1   ; Company mode minimum prefix length
      create-lockfiles nil		; Disable lockfiles
      display-line-numbers-type t       ; Display line numbers
      dracula-alternate-mode-line-and-minibuffer t ; Use alternate mode line and minibuffer for Dracula theme
      history-length 20			; Set command history length
      inhibit-splash-screen t		; Inhibit splash screen at startup
      inhibit-startup-message t		; Inhibit startup message
      lsp-idle-delay 0.2		; LSP mode idle delay
      lsp-prefer-capf t			; Prefer capf for LSP completion
      magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1 ; start magit on fullscreen
      make-backup-files nil		; Disable backup files
      undo-tree-enable-undo-in-region nil ; undo command
      nyan-animate-nyancat t            ; Enable nyan animation
      vc-follow-symlinks t		; Follow symlinks in version control
      vertico-buffer-mode 1		; Enable vertico buffer mode
      window-divider-default-right-width 1 ; Set window divider right width
      )
(defalias 'yes-or-no-p 'y-or-n-p) ; Use 'y' and 'n' instead of 'yes' and 'no'

(defun my-enable-solaire-mode ()
  "Enable solaire-mode while change buffer"
  (unless (minibufferp)
    (solaire-mode)))
(add-hook 'buffer-list-update-hook #'my-enable-solaire-mode)

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
(load-theme 'doom-acario-light t)

(require 'calfw)
(require 'calfw-org)
(require 'org)
(require 'company)
(require 'company-lua)

;; Fonction pour ouvrir le calendrier Org
(defun my-open-calendar ()
  (interactive)
  (cfw:open-org-calendar))

;; Ajouter un raccourci pour ouvrir le calendrier
(global-set-key (kbd "C-c c") 'my-open-calendar)

;; Définir le chemin vers le fichier Org
(setq org-agenda-files '("~/tasks.org"))

;; Fonction pour ajouter une entrée Org-mode pour la date sélectionnée
(defun my-add-org-entry ()
  "Ajouter une entrée Org-mode pour la date sélectionnée."
  (interactive)
  (let* ((date (cfw:cursor-to-date))
         (org-file (car org-agenda-files))
         (entry (read-string "Entrée: ")))
    (find-file org-file)
    (goto-char (point-max))
    (insert (format "\n* TODO %s\n  SCHEDULED: <%s>" entry (format-time-string "%Y-%m-%d %a" (encode-time 0 0 0 (calendar-extract-day date) (calendar-extract-month date) (calendar-extract-year date)))))
    (save-buffer)
    (kill-buffer)))

;; Ajouter une touche de raccourci pour ajouter une entrée
(define-key cfw:calendar-mode-map (kbd "a") 'my-add-org-entry)

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
(display-battery-mode 1)           ; Display battery
(global-company-mode 1)            ; Display battery

;; Evil configuration
(with-eval-after-load 'evil
  (setq evil-want-C-i-jump nil		; Disable C-i jump
	evil-symbol-word-search t	; Use symbol-based word search
	evil-insert-state-modes nil	; Disable insert state
	evil-motion-state-modes nil	; Disable motion state
	evil-move-cursor-back nil ; Disable move cursor back in insert mode
	evil-kill-on-visual-paste nil ; Disable killing on visual paste
	evil-move-cursor-back t	    ; Move cursor back in normal state
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
  (add-hook 'edebug-mode-hook 'evil-normalize-keymaps))

;; LSP configuration for C and C++ modes
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

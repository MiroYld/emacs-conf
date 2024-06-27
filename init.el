;; Adding xmake plugin
(load "~/.emacs.d/xmake.el")

;; Disable menu, scroll, and tool bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Set font size
(set-face-attribute 'default nil :font "Ubuntu Mono-12" :weight 'normal)

(require 'calfw)
(require 'calfw-org)
(require 'org)

(setq auto-save-default nil             ; Disable auto-save
      c-set-style "ellemtel"            ; Set C-style to "ellemtel"
      company-idle-delay 0.1            ; Company mode idle delay
      company-minimum-prefix-length 1   ; Company mode minimum prefix length
      create-lockfiles nil              ; Disable lockfiles
      display-line-numbers-type t       ; Display line numbers
      history-length 20                 ; Set command history length
      inhibit-splash-screen t           ; Inhibit splash screen at startup
      inhibit-startup-message t         ; Inhibit startup message
      lsp-idle-delay 0.1                ; LSP mode idle delay
      lsp-prefer-capf t                 ; Prefer capf for LSP completion
      magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1 ; start magit on fullscreen
      make-backup-files nil             ; Disable backup files
      undo-tree-enable-undo-in-region nil ; undo command
      nyan-animate-nyancat t            ; Enable nyan animation
      vc-follow-symlinks t              ; Follow symlinks in version control
      vertico-buffer-mode 1             ; Enable vertico buffer mode
      window-divider-default-right-width 1 ; Set window divider right width
      )

;; Configure the Modus Themes' appearance
(setq modus-themes-mode-line '(accented borderless)
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-completions'((t . (extrabold underline)))
      modus-themes-org-blocks 'tinted-background
      modus-themes-scale-headings t
      modus-themes-region '(bg-only)
      modus-themes-headings
      '((1 . (rainbow overline background 1.4))
	(2 . (rainbow background 1.3))
	(3 . (rainbow bold 1.2))
	(t . (semilight 1.1))))

;; Load the dark theme by default
(load-theme 'modus-vivendi t)

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


;; Fonction pour ouvrir le calendrier Org
(defun my-open-calendar ()
  (interactive)
  (cfw:open-org-calendar))

;; Ajouter un raccourci pour ouvrir le calendrier
(global-set-key (kbd "C-c c") 'my-open-calendar)

;; Définir le chemin vers le fichier Org
(setq org-agenda-files '("~/.emacs.d/tasks.org"))

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

;; Evil configuration
(with-eval-after-load 'evil
  (setq evil-want-C-i-jump nil          ; Disable C-i jump
	evil-symbol-word-search t       ; Use symbol-based word search
	evil-insert-state-modes nil     ; Disable insert state
	evil-motion-state-modes nil     ; Disable motion state
	evil-move-cursor-back nil ; Disable move cursor back in insert mode
	evil-kill-on-visual-paste nil ; Disable killing on visual paste
	evil-move-cursor-back t     ; Move cursor back in normal state
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


(use-package mu4e
  :ensure nil
  :config

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 1 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  ;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; Configure the function to use for sending mail
  (setq message-send-mail-function 'smtpmail-send-it)

  (setq mu4e-contexts
	(list
	 ;; Work account
	 (make-mu4e-context
	  :name "Work"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "your-email")
		  (user-full-name    . "your-name")
		  (smtpmail-smtp-server  . "smtp.gmail.com")
		  (smtpmail-smtp-service . 465)
		  (smtpmail-stream-type  . ssl)
		  (mu4e-drafts-folder  . "/Gmail/[Gmail]/Drafts")
		  (mu4e-sent-folder  . "/Gmail/[Gmail]/Sent Mail")
		  (mu4e-refile-folder  . "/Gmail/[Gmail]/All Mail")
		  (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")))))

  (setq mu4e-maildir-shortcuts
	'(("/Gmail/Inbox"             . ?i)
	  ("/Gmail/[Gmail]/Sent Mail" . ?s)
	  ("/Gmail/[Gmail]/Trash"     . ?t)
	  ("/Gmail/[Gmail]/Drafts"    . ?d)
	  ("/Gmail/[Gmail]/All Mail"  . ?a))))


(global-set-key (kbd "C-c m") 'mu4e)

(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

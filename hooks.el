;; Hooks
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'sh-mode-hook #'lsp)

;;Ivy configuration
(with-eval-after-load 'ivy-rich
  (setq ivy-use-virtual-buffers t                         ; Use virtual buffers in Ivy
	ivy-re-builders-alist '((swiper . ivy--regex-plus)
				(t . ivy--regex-fuzzy))   ; Regex builders for Ivy
	ivy-virtual-abbreviate 'full                      ; Abbreviate virtual buffers
	counsel-find-file-ignore-regexp "\\.go\\'"        ; Ignore .go files in counsel-find-file
	enable-recursive-minibuffers t                    ; Enable recursive minibuffers
	recentf-max-saved-items nil                       ; Limit the number of recent files saved in recentf
	))

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


(defun my/colorize-compilation-buffer ()
  "Appliquer la coloration ANSI dans le buffer de compilation."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer)

;; --- 2. Raccourci pour arrêter le processus de compilation ---
(global-set-key (kbd "C-c x") 'kill-compilation)

;; --- 3. Commande personnalisée pour compiler avec affichage correct ---
(defun my/compile-with-color (command)
  "Lancer une commande avec couleurs ANSI dans un buffer compilation."
  (interactive "sCommande à compiler: ")
  (let ((compilation-environment '("TERM=xterm-256color")))
    (compilation-start command 'compilation-mode)))

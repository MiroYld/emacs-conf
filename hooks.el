;; Hooks
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

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

(defun ar/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
	   (image-path "~/Downloads/emacs-modern.png")
	   (image (create-image image-path))
	   (size (image-size image))
	   (height (cdr size))
	   (width (car size))
	   (top-margin (floor (/ (- (window-height) height) 2)))
	   (left-margin (floor (/ (- (window-width) width) 2)))
	   (prompt-title "☕ Keep calm, drink coffee ☕"))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (insert (make-string (floor (/ (- (window-width) (string-width prompt-title)) 2)) ?\ ))
      (insert prompt-title))
    (setq cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)))

(setq inhibit-startup-screen t)

(when (< (length command-line-args) 2)
  (add-hook 'emacs-startup-hook (lambda ()
				  (when (display-graphic-p)
				    (ar/show-welcome-buffer)))))

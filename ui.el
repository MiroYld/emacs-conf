;; Disable menu, scroll, and tool bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-face-attribute 'default nil :font "Ubuntu Mono-12" :weight 'bold)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun my-enable-solaire-mode ()
  "Enable solaire-mode while change buffer"
  (unless (minibufferp)
    (solaire-mode)))
(add-hook 'buffer-list-update-hook #'my-enable-solaire-mode)

(setq display-time-load-average nil
      display-time-default-load-average nil
      display-time-format "%Hh%M")

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (load-theme 'doom-dracula t)))

(setq doom-modeline-icon t)

(defun my-cfw-open-org-calendar ()
  (interactive)
  (setq org-agenda-files (list "~/org/todo.org"))
  (cfw:open-org-calendar))

(global-set-key (kbd "C-c c") 'my-cfw-open-org-calendar)


; background term for terminal: #1E202B

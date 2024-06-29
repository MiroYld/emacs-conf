;; Disable menu, scroll, and tool bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-face-attribute 'default nil :font "Ubuntu Mono-12" :weight 'normal)

;; Modous configuration
(setq modus-themes-mode-line '(accented borderless)
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-completions '((t . (extrabold underline)))
      modus-themes-org-blocks 'tinted-background
      modus-themes-scale-headings t
      modus-themes-region '(bg-only)
      modus-themes-headings
      '((1 . (rainbow overline background 1.4))
	(2 . (rainbow background 1.3))
	(3 . (rainbow bold 1.2))
	(t . (semilight 1.1))))
(load-theme 'modus-vivendi t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun my-enable-solaire-mode ()
  "Enable solaire-mode while change buffer"
  (unless (minibufferp)
    (solaire-mode)))
(add-hook 'buffer-list-update-hook #'my-enable-solaire-mode)

;; Keybindings
(global-set-key (kbd "C-c m") 'mu4e)
(global-set-key (kbd "C-c r") 'revert-buffer)

(defun open-file-at-point ()
  "Open the file at the point using `ffap`."
  (interactive)
  (let ((file (thing-at-point 'filename)))
    (if file
	(find-file file)
      (message "No file at point"))))

;; Bind the function to C-c f
(global-set-key (kbd "C-c f") 'open-file-at-point)

(require 'lsp-mode)

(defun implement-function ()
  "implement declared function"
  (interactive)
  (let* ((header-file (buffer-file-name))
	 (class-name (save-excursion
		       (goto-char (point-min))
		       (re-search-forward "^class \\([A-Za-z0-9_]+\\)" nil t)
		       (match-string 1)))
	 (function-declaration (thing-at-point 'line t))
	 (type-and-name (progn
			  (string-match "\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*(" function-declaration)
			  (list (match-string 1 function-declaration) ; type
				(match-string 2 function-declaration)))) ; function name
	 (type (car type-and-name))
	 (function-name (cadr type-and-name))
	 (args-and-ret (progn
			 (string-match "\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*(\\(.*\\))" function-declaration)
			 (match-string 2 function-declaration)))
	 (cpp-file (concat (file-name-sans-extension header-file) ".cpp"))
	 (formatted-function-declaration
	  (if (string= args-and-ret "") ; cas de fonction sans arguments
	      (format "%s %s::%s() {\n  // TODO: Implement %s\n}\n"
		      type class-name function-name function-name)
	    (format "%s %s::%s(%s) {\n  // TODO: Implement %s\n}\n"
		    type class-name function-name args-and-ret function-name))))
    (if (and function-name class-name (file-exists-p cpp-file))
	(with-current-buffer (find-file-noselect cpp-file)
	  (goto-char (point-max))
	  (if (search-backward (concat type " " class-name "::" function-name) nil t)
	      (progn
		(goto-char (match-end 0))
		(search-forward "{")
		(forward-line)
		(if (search-backward "// TODO: Implement " nil t)
		    (progn
		      (delete-region (line-beginning-position) (line-end-position))
		      (insert formatted-function-declaration)
		      (goto-char (point-max))
		      (message "Function %s updated" function-name))
		  (message "Function %s already defined" function-name)))
	    (insert "\n" formatted-function-declaration)
	    (goto-char (point-max))
	    (message "Function %s added." function-name))
	  (save-buffer)))))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c C-r") 'lsp-find-references)
  (define-key lsp-mode-map (kbd "C-c C-a") 'implement-function)
  (define-key lsp-mode-map (kbd "C-c C-d") 'lsp-find-definition)
  (define-key lsp-mode-map (kbd "C-c C-t") 'lsp-find-type-definition)
  (define-key lsp-mode-map (kbd "C-c C-f") 'lsp-format-buffer))

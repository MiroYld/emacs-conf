(require 'calfw)
(require 'calfw-org)
(require 'org)

(defun my-open-calendar ()
  "Ouvre le calendrier Org-mode."
  (interactive)
  (cfw:open-org-calendar))

(global-set-key (kbd "C-c c") 'my-open-calendar)

(setq org-agenda-files '("~/.emacs.d/todo.org"))
(setq calendar-month-name-array
      ["Janvier" "Février" "Mars" "Avril" "Mai" "Juin"
       "Juillet" "Août" "Septembre" "Octobre" "Novembre" "Décembre"])
(setq calendar-day-name-array
      ["Dimanche" "Lundi" "Mardi" "Mercredi" "Jeudi" "Vendredi" "Samedi"])
(setq calendar-week-start-day 0) ; 0:Sunday, 1:Monday

(defun my-add-org-entry ()
  "Ajouter une entrée Org-mode pour la date sélectionnée."
  (interactive)
  (let* ((date (cfw:cursor-to-date))  ;; Utiliser `cfw:cursor-to-date` pour obtenir la date sélectionnée
         (org-file (car org-agenda-files)) ;; Fichier Org dans lequel ajouter l'entrée
         (entry (read-string "Entrée: "))) ;; Demander l'entrée à ajouter
    (if date
        (progn
          (find-file org-file)
          (goto-char (point-max)) ;; Aller à la fin du fichier
          ;; Insérer l'entrée avec la date sélectionnée
          (insert (format "\n* TODO %s\n  SCHEDULED: <%s>"
                          entry
                          (format-time-string "%Y-%m-%d %a"
                                              (encode-time 0 0 0
                                                            (calendar-extract-day date)
                                                            (calendar-extract-month date)
                                                            (calendar-extract-year date)))))
          (save-buffer) ;; Enregistrer le fichier
          (kill-buffer)) ;; Fermer le buffer
      (message "Aucune date sélectionnée.")))) ;; Message si aucune date sélectionnée

(define-key cfw:calendar-mode-map (kbd "a") 'my-add-org-entry)

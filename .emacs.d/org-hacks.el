(setq org-todo-keywords
      '((sequence "TODO" "ONGOING" "PAUSED" "|" "DONE")))

(setq org-agenda-files
      (list "~/ORG/notes/"
            "~/ORG/todo/"
            "~/ORG/schedule"))

;; (setq org-display-custom-times t)
;; (setq org-time-stamp-custom-formats
;;       (quote ("<%y%m%d>" . "<%A, %B %d, %Y -- %I:%M %p>")))

(defun org-show-current-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
        (show-children)))

;; Custom key-bindings
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-b") 'org-mark-ring-goto)))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

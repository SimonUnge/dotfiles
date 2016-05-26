(setq org-todo-keywords
      '((sequence "TODO" "DEFINED" "PAUSED" "|" "ONGOING" "COMPLETE"
      "ACCEPTED" "DONE")))

(setq org-agenda-files
      (list "~/git/PO/"))

(setq org-display-custom-times t)
(setq org-time-stamp-custom-formats
      (quote ("<%y%m%d>" . "<%A, %B %d, %Y -- %I:%M %p>")))

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

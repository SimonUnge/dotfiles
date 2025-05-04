(setq org-todo-keywords
      '((sequence "TODO" "ONGOING" "PAUSED" "|" "DONE")))

(setq org-agenda-files
      (list "~/org/"))

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

(global-set-key (kbd "C-c c") 'org-capture)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-capture-use-agenda-date nil)
(setq org-capture-templates-contexts nil)
(add-hook 'org-capture-mode-hook 'delete-other-windows)


(setq org-capture-templates
      '(
        ("p" "RabbitMQ Streams Project Entry" entry
         (file+datetree "~/org/rmq-tiered-storage.org")
         "* %U - Daily Update
** Technical Decisions
   - *Decision made:* %?
   - *Alternatives considered:*
   - *Rationale:*
   - *Impact:*

** Challenges Encountered
   - *Challenge:*
   - *Solution attempted:*
   - *Outcome:*
   - *Lessons learned:*

** Performance Metrics
   - *Current metrics:*
   - *Improvements observed:*
   - *Areas needing attention:*

** Collaboration Notes
   - *Meetings:*
   - *Cross-team interactions:*
   - *Feedback received:* ")
        ("g" "General To-Do"
         entry (file+headline "~/org/todos.org" "General Tasks")
         "* TODO [#B] %?\n:Created: %T\n "
         :empty-lines 0)
        ("m" "Meeting"
         entry (file+datetree "~/org/meetings.org")
         "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
         :tree-type week
         :clock-in t
         :clock-resume t
         :empty-lines 0)
        ))

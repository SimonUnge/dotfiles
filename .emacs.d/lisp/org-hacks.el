;;; org-hacks.el --- Custom Org mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Extended Org mode configuration with custom templates, functions and keybindings
;;
;;; Code:

(require 'org)

;;; Basic configuration
(setq org-todo-keywords
      '((sequence "TODO" "ONGOING" "PAUSED" "|" "DONE")))

;; Define org directory and files
(defvar org-directory "~/org/"
  "Base directory for Org files.")

(setq org-agenda-files
      (list org-directory))

(setq org-log-done t)

;; Custom timestamp formats (uncommented and improved)
(setq org-display-custom-times t)
(setq org-time-stamp-custom-formats
      '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))

;;; Custom functions
(defun org-show-current-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (condition-case nil
        (progn
          (outline-back-to-heading)
          (unless (and (bolp) (org-on-heading-p))
            (org-up-heading-safe)
            (hide-subtree))
          (org-overview)
          (org-reveal t)
          (org-show-entry)
          (show-children))
      (error (message "Cannot navigate further")))))

;;; Keybindings
;; Local keybindings
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-b") 'org-mark-ring-goto)))

;; Global keybindings
(global-set-key (kbd "C-c c") 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;;; Capture templates
(setq org-capture-use-agenda-date nil)
(setq org-capture-templates-contexts nil)
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; Project-specific templates
(defvar org-project-capture-templates
  '(("p" "RabbitMQ Streams Project Entry" entry
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
   - *Feedback received:* "))
  "Project-specific capture templates.")

;; General templates
(defvar org-general-capture-templates
  '(("g" "General To-Do"
     entry (file+headline "~/org/todos.org" "General Tasks")
     "* TODO [#B] %?\n:Created: %T\n "
     :empty-lines 0)
    ("m" "Meeting"
     entry (file+datetree "~/org/meetings.org")
     "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
     :tree-type week
     :clock-in t
     :clock-resume t
     :empty-lines 0))
  "General purpose capture templates.")

;; Combine all templates
(setq org-capture-templates
      (append org-general-capture-templates
              org-project-capture-templates))

;;; Additional useful settings
;; Better looking org mode
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)

;; Better agenda view
(setq org-agenda-span 'day)
(setq org-agenda-start-on-weekday nil)

;; Refile settings
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(provide 'org-hacks)
;;; org-hacks.el ends here

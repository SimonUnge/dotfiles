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
(global-set-key (kbd "C-c w") 'org-journal-goto-today)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;;; Work Journal
(defun org-journal-goto-today ()
  "Go to today's entry in work journal, creating week if needed."
  (interactive)
  (let* ((week (format-time-string "%V"))
         (day (format-time-string "%A"))
         (file "~/org/work-journal.org"))
    (find-file file)
    (goto-char (point-min))
    (if (re-search-forward (format "^\\*\\* Week %s$" week) nil t)
        (progn
          (re-search-forward (format "^\\*\\*\\* %s$" day) nil t)
          (org-end-of-subtree))
      ;; Week doesn't exist, create it
      (goto-char (point-max))
      (insert (format "\n** Week %s\n*** Monday\n*** Tuesday\n*** Wednesday\n*** Thursday\n*** Friday\n" week))
      (re-search-backward (format "^\\*\\*\\* %s$" day) nil t)
      (org-end-of-subtree))))

;;; Capture templates
(setq org-capture-use-agenda-date nil)
(setq org-capture-templates-contexts nil)
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; Project-specific templates
(defvar org-project-capture-templates
  '(("p" "Project note" entry
     (file+headline (lambda () (read-file-name "Project file: " "~/org/"))
                    "Notes")
     "* %U %?\n:Created: %T\n"
     :empty-lines 0))
  "Project-specific capture templates.")

;; General templates
(defvar org-general-capture-templates
  '(("g" "General To-Do"
     entry (file+headline "~/org/todos.org" "General Tasks")
     "* TODO [#B] %?\n:Created: %T\n "
     :empty-lines 0)
    ("i" "Inbox (quick capture)"
     entry (file+headline "~/org/inbox.org" "Inbox")
     "* %?\n:Created: %T\n"
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

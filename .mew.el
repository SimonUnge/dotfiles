(setq mew-name "Simon Unge") ;; (user-full-name)

(setq mew-use-biff t)
(setq mew-use-biff-bell t)
(setq mew-biff-interval 1)  ; check every minute

(setq mew-config-alist
      '(
        (default
         (user              "sunge")
         (mail-domain       "cisco.com")

         (smtp-server       "outbound.cisco.com")
         (smtp-port         25)
         (smtp-ssl          nil)
         (smtp-mail-from    "sunge@cisco.com")

         (proto             "%");
         (imap-user         "sunge")
         (imap-server       "mail.cisco.com")
         (imap-ssl          t)
         (imap-size         0)
         )
        ))

(setq mew-cs-database-for-encoding
      `(((ascii)                    nil         "7bit"             "7bit" nil)
        ;; West European (Latin 1)
        ((ascii latin-iso8859-1)    iso-8859-1  "quoted-printable" "Q" nil)
        ;; New West European (Latin 9 or Latin 0)
        ((ascii latin-iso8859-15)   iso-8859-15 "quoted-printable" "Q" nil)
        (nil utf-8 "base64" "B" t)))

(setq mew-ssl-verify-level 0)

(setq mew-summary-form '(type (5 date) " " (14 from) " " t (0 subj)))

;; Use and hihglight unread mark
(setq mew-use-unread-mark t)
(setq mew-theme-file "~/.mew-theme.el")

;; spam
                                        ; use 'ls' in Summary mode
(setq mew-spam-prog "ssh")
(setq mew-spam-prog-args '("mail.tail-f.com" "/home/su/bin/learn-spam"))
                                        ; use 'lh' in Summary mode
(setq mew-ham-prog "ssh")
(setq mew-ham-prog-args '("mail.tail-f.com" "/home/su/bin/learn-ham"))

;; Use nice thread functions (tt in summary mode)
(setq mew-use-fancy-thread t)
(setq mew-use-thread-separator t)
(setq mew-use-thread-heuristics t)

(setq mew-demo nil)
(setq mew-demo-picture nil)

;; Should this really be needed??
(setq mew-cs-binary 'binary)

;; To: field list picked at mew-summary-reply if Reply-To: exists.
(setq mew-replyto-to-list '("Reply-To:"))

;; Prompt user for CC: field if non-nil
(setq mew-ask-cc nil)

;; If non-nil, ask for a range for scanning. If false, always use default.
(setq mew-ask-range nil)

;; If non-nil, ask whether or not you really want to send the message
;; which you composed without explicit C-cC-m.
(setq mew-ask-send nil)

;; If non-nil, ask whether or not you really want to pack.
(setq mew-ask-pack nil)

;; If non-nil, mark processing when scanning a folder or exiting Emacs.
(setq mew-ask-mark-process t)

;; If non-nil, the previous dir is used as the default dir for save etc.
(setq mew-summary-preserve-dir t)

;; If non-nil, the previous dir is used as the default dir for copy, etc.
(setq mew-draft-preserve-dir t)

(add-hook 'mew-draft-mode-hook
          (function (lambda () (auto-fill-mode 1))))

(add-hook 'mew-quit-hook
          (function (lambda () (delete-other-windows))))

(setq mew-fcc "%outbox")
(setq mew-cite-fields '("From:"))
(setq mew-cite-format "%s wrote:\n")
(setq mew-cite-prefix "> ")
(setq mew-cite-prefix-function nil)

(defvar has-w3m (require 'mew-w3m nil t))
(if has-w3m
    (progn
      (define-key mew-summary-mode-map "T" 'mew-w3m-view-inline-image)
      (setq mew-ext-url-alist
            '(("^application/" "Fetch by emacs-w3m" mew-w3m-ext-url-fetch nil)
              (t "Browse by emacs-w3m" mew-w3m-ext-url-show nil)))))

(defun flush-queue ()
  (interactive)
  (mew-smtp-flush-queue mew-case))

(add-hook 'mew-smtp-sentinel-hook 'flush-queue)

;; External programs
;;(setq mew-prog-postscript '("gnome-open" ("-geom" "+0+0") t))

(setq mew-prog-pdf '("open" nil t))

;; (setq mew-prog-ooffice "libreoffice")
;; (setq mew-prog-msword-ext "libreoffice")
;; (setq mew-prog-msexcel-ext "libreoffice")
;; (setq mew-prog-mspowerpoint-ext "libreoffice")


;;(setq mew-prog-msword '("ooffice" () t))
;;(setq mew-prog-msexcel '("ooffice" () t))
;;(setq mew-prog-mspowerpoint '("ooffice" () t))

(setq mew-use-master-passwd t)
(setq mew-use-text/html t)
(setq mew-prog-ssl "stunnel4")
(setq mew-prog-text/html-ext "google-chrome")

;; (defun my-calendar (cache begin end &optional params)
;;   ;; called in Message buffer
;;   (save-excursion
;;     (let ((start (point)))
;;       ;; We need to keep composite properties of charset.
;;       ;; This must be "insert-buffer-substring".
;;       (insert-buffer-substring cache begin end)
;;       (icalendar-import-buffer "%diary-tmp" t nil))))

;; (defvar my-calendar-prog 'my-calendar)

;; (add-hook 'mew-init-hook
;;           (function
;;            (lambda ()
;;              (setcar
;;               (cdr (cdr (cdr (assoc "text/calendar" mew-mime-content-type))))
;;               'my-calendar-prog))))

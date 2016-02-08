(cd "~/")
(menu-bar-mode -1)
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

(load-file "~/.emacs.d/simon-tail-f.el")

(require 'sort)
(require 'package)
(package-initialize)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/")
 t)

;; (defvar my-packages '(starter-kit
;;                       starter-kit-lisp
;;                       starter-kit-bindings
;;                       starter-kit-eshell
;;                       clojure-mode
;;                       clojure-test-mode
;;                       nrepl))

;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))

(load-file "~/.emacs.d/yang-snippet.el")

(load-file "~/.emacs.d/org-hacks.el")

;; (setq org-todo-keywords
;;       '((sequence "TODO" "ONGOING" "|" "DONE" "PAUSED" "CANCELED")))

;; (setq org-agenda-files
;;       (list "~/git/PO/"))

;; (setq org-display-custom-times t)
;; (setq org-time-stamp-custom-formats
;;       (quote ("<%y%m%d>" . "<%A, %B %d, %Y -- %I:%M %p>")))

(defalias 'list-buffers 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("rest-src"
                (filename . "/home/su/work/dev/trunk/lib/rest/src/"))
               ("rest-eunit"
                (filename . "/home/su/work/dev/trunk/lib/rest/test/eunit/"))
               ("rest-lux"
                (filename . "/home/su/work/dev/trunk/lib/rest/test/lux/"))
               ("erlang" (mode . erlang-mode))
               ("yang" (mode . yang-mode))
               ("lux" (mode . lux-mode))
               ("dired" (mode . dired-mode))
               ("cli-spec" (name . ".*\.cli"))
               ("support-mail" (name . "support\.tail-f.*"))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "<f8>") 'execute-extended-command)


(add-hook 'c-mode-hook
          (lambda
            ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'erlang-mode-hook
          (lambda
            ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'yang-mode-hook
          (lambda
            ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(add-hook 'org-mode-hook
          (lambda
            ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(org-agenda-files (quote ("~/git/PO/backlog.org" "/home/su/git/PO/retrospectives.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Open Edit with Emacs in new full-size buffer
(setq same-window-regexps '("Update\.html"))

(defun go-to-rt (x)
  (interactive "sRT#:")
  (dired (format "~/work/rt/%s" x)))

(when (and (daemonp) (locate-library "edit-server"))
  (require 'edit-server)
  (setq edit-server-new-frame nil)
  (edit-server-start))

(show-paren-mode t)
(column-number-mode t)
(ido-mode t)

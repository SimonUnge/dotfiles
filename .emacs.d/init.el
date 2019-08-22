(cd "~/")
(menu-bar-mode -1)
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq create-lockfiles nil)

(load-file "~/.emacs.d/simon-tail-f.el")

(require 'sort)
(require 'package)
(package-initialize)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/")
 t)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

(load-file "~/.emacs.d/yang-snippet.el")

(load-file "~/.emacs.d/org-hacks.el")

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
            (add-to-list 'write-file-functions
                         'delete-trailing-whitespace)))
(add-hook 'python-mode-hook
          (lambda
            ()
            (add-to-list 'write-file-functions
                         'delete-trailing-whitespace)))

(add-hook 'python-mode-hook (lambda ()
                              (auto-fill-mode 0)))
;; (add-hook 'erlang-mode-hook (lambda ()
;;                               (interactive) (column-marker-1 80)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(org-agenda-files nil t)
 '(package-selected-packages
   (quote
    (gruvbox-theme iedit tabbar helm project-explorer rainbow-delimiters projectile rainbow-identifiers rainbow-mode plantuml-mode docbook-snippets docbook buffer-expose groovy-mode elixir-mode elixir-yasnippets py-autopep8 idomenu pylint erlang yaml-mode mew))))
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
;; (column-number-mode t)
(ido-mode t)
(electric-indent-mode -1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
                `((".*" ,temporary-file-directory t)))

(setq jiralib-url "https://jira-eng-gpk3.cisco.com/jira")

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

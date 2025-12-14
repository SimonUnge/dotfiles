;;; init.el --- Simon's Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;; This is a modular Emacs configuration that uses use-package for package management
;; and organizes settings into logical groups.
;;
;;; Code:

;; Set default directory
(cd "~/")

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Basic settings that should be loaded immediately
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)

;; Load core modules
(require 'init-packages)  ;; Must be loaded first to set up use-package
(require 'init-env)
(require 'init-ui)
(require 'init-editing)
(require 'init-erlang)
(require 'init-scala)
(require 'init-org)
(require 'init-amazon-q)

;; Custom variables - kept in init.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(org-agenda-files nil)
 '(package-selected-packages
   '(erlang exec-path-from-shell markdown-mode markdown-preview-eww
            org-tree-slide sbt-mode scala-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Provide this file as a feature
(provide 'init)
;;; init.el ends here

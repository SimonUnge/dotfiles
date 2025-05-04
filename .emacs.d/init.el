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
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq create-lockfiles nil)
(setq-default indent-tabs-mode nil)

;; Load core modules
(require 'init-packages)  ;; Must be loaded first to set up use-package
(require 'init-ui)
(require 'init-editing)
(require 'init-erlang)
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
   '(use-package format-all org-tree-slide which-key helm-lsp lsp-origami lsp-ui yasnippet lsp-mode markdown-mode ag magit gruvbox-theme iedit tabbar helm project-explorer rainbow-delimiters projectile rainbow-identifiers rainbow-mode plantuml-mode docbook-snippets docbook buffer-expose groovy-mode elixir-mode elixir-yasnippets py-autopep8 idomenu pylint erlang yaml-mode mew)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Provide this file as a feature
(provide 'init)
;;; init.el ends here

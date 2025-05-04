;;; init-ui.el --- UI configuration -*- lexical-binding: t -*-
;;; Commentary:
;; UI settings including themes, mode line, and visual elements
;;
;;; Code:

;; Basic UI settings
(menu-bar-mode -1)                  ;; Disable menu bar
(show-paren-mode t)                 ;; Highlight matching parentheses
(ido-mode t)                        ;; Enable interactive do mode
(electric-indent-mode -1)           ;; Disable electric indent

;; Whitespace visualization
(use-package whitespace
  :ensure nil  ;; Built-in package
  :config
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (global-whitespace-mode t))

;; Key bindings
(global-set-key (kbd "<f8>") 'execute-extended-command)

;; Project explorer settings
(setq pe/omit-gitignore 1)

;; Org-tree-slide key bindings
(with-eval-after-load "org-tree-slide"
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree))

;; Provide this file as a feature
(provide 'init-ui)
;;; init-ui.el ends here

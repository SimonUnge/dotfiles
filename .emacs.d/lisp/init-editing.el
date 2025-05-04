;;; init-editing.el --- Editing configuration -*- lexical-binding: t -*-
;;; Commentary:
;; General editing settings and preferences
;;
;;; Code:

;; Indentation settings
(setq-default indent-tabs-mode nil)  ;; Never use TAB character in code
(auto-fill-mode -1)                  ;; Disable auto-fill mode globally

;; File handling
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq create-lockfiles nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Special handling for "Edit with Emacs" browser extension
(setq same-window-regexps '("Update\.html"))

;; Consolidated hooks for removing trailing whitespace
(dolist (hook '(c-mode-hook erlang-mode-hook yang-mode-hook 
                org-mode-hook python-mode-hook))
  (add-hook hook (lambda ()
                   (add-to-list 'write-file-functions 
                                'delete-trailing-whitespace))))

;; Python-specific settings
(add-hook 'python-mode-hook (lambda ()
                              (auto-fill-mode 0)))

;; Provide this file as a feature
(provide 'init-editing)
;;; init-editing.el ends here

;;; init-env.el --- Environment configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Sets up environment variables and system path integration
;;
;;; Code:

;; Ensure environment variables are properly set in Emacs GUI
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil) ;; Reduce startup time
  (setq exec-path-from-shell-variables '("PATH" "MANPATH")) ;; Variables to import
  (exec-path-from-shell-initialize))

;; Additional environment settings could go here
;; For example:
;; - Setting default directory
;; - Configuring locale settings
;; - Setting up proxy if needed

;; Provide this file as a feature
(provide 'init-env)
;;; init-env.el ends here

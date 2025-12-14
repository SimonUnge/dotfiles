;;; init-erlang.el --- Erlang configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Erlang-specific settings and key bindings
;;
;;; Code:

;; Erlang mode setup
(use-package erlang
  :defer t
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode))
  :config
  ;; Load erl-find-source if available
  (condition-case nil
      (load-file "~/git/erl-find-source/erl-find-source.el")
    (error (message "Failed to load erl-find-source.el")))

  ;; Erlang mode hook
  (defun my-erlang-mode-hook ()
    "Configuration for Erlang Mode. Add this to `erlang-mode-hook'."
    (setq indent-tabs-mode nil)
    (local-set-key "\e." 'erlfs-find-source-under-point)
    (local-set-key "\e," 'erlfs-find-source-unwind)
    (local-set-key "\e?" 'erlfs-find-callers))

  (add-hook 'erlang-mode-hook 'my-erlang-mode-hook))

;; Provide this file as a feature
(provide 'init-erlang)
;;; init-erlang.el ends here

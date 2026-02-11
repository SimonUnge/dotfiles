;;; init-amazon-q-lsp.el --- Amazon Q LSP integration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Amazon Q with Eglot
;;
;;; Code:

(use-package lsp-multi
  :ensure nil)

(use-package amz-q-macs
  :ensure nil
  :demand t
  :bind (("C-c q TAB" . amz-q-macs-complete-here)
         ("C-c q c" . amz-q-toggle-chat)
         ("C-c q e" . amz-q-macs-chat-explain-selection)
         ("C-c q f" . amz-q-macs-chat-fix-selection)
         ("C-c q d" . amz-q-macs-chat-document-selection))
  :custom
  (amz-q-lsp-client 'eglot)
  (amz-q-lsp-auth-method 'identity)
  (amz-q-lsp-supported-modes
   '((scala-mode :language-id "scala")
     (python-mode :language-id "python")
     (emacs-lisp-mode :language-id "elisp")))
  :config
  (amz-q-lsp-setup)
  :hook
  (prog-mode . amz-q-macs-mode))

(provide 'init-amazon-q-lsp)
;;; init-amazon-q-lsp.el ends here

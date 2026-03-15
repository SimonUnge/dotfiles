;;; init-search.el --- Search and project configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Project detection and ripgrep search configuration
;;
;;; Code:

(require 'project)

(use-package rg
  :config
  (rg-enable-default-bindings))

(provide 'init-search)
;;; init-search.el ends here

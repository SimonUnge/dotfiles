;;; init-org.el --- Org mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Org mode settings and customizations
;;
;;; Code:

;; Load org-hacks if available
(require 'org-hacks)

;; Org-tree-slide configuration
(use-package org-tree-slide
  :defer t)

;; Provide this file as a feature
(provide 'init-org)
;;; init-org.el ends here

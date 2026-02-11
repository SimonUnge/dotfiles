;;; init-git.el --- Git configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Git integration with Magit
;;
;;; Code:

(use-package magit
  :bind ("C-x g" . magit-status))

(provide 'init-git)
;;; init-git.el ends here

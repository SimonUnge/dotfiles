;;; init-packages.el --- Package management setup -*- lexical-binding: t -*-
;;; Commentary:
;; Sets up package.el and use-package for better package management
;;
;;; Code:

;; Initialize package sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package
(require 'use-package)
(setq use-package-always-ensure t)  ;; Always ensure packages are installed

;; Provide this file as a feature
(provide 'init-packages)
;;; init-packages.el ends here

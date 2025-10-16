;;; init-amazon-q.el --- Amazon Q integration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup for Amazon Q integration with Emacs
;;
;;; Code:

;; Load q-chat module
(require 'q-chat)

;; Customize popup behavior to use normal window splits
(setq amz-q-chat-popup-function 'pop-to-buffer)

;; Explicitly enable amazon-q-mode
(global-amazon-q-mode 1)

;; Provide this file as a feature
(provide 'init-amazon-q)
;;; init-amazon-q.el ends here

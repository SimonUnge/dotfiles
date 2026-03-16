;;; init-scala.el --- Scala development configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for Scala development with Metals LSP server
;;
;;; Code:

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode: https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package eglot
  :hook ((scala-mode . eglot-ensure)
         (scala-mode . (lambda ()
                         (add-hook 'before-save-hook 'eglot-format-buffer nil t))))
  :config
  (setq eglot-sync-connect nil)  ; Don't block Emacs waiting for LSP
  (setq eglot-connect-timeout 60) ; Longer timeout for large projects
  (setq eglot-events-buffer-config '(:size 0)) ; Disable events buffer for performance
  (add-to-list 'eglot-server-programs
               '(scala-mode . ("metals" "-J-Xmx6G" "-J-XX:+UseG1GC" "-J-XX:+UseStringDeduplication"))))

(provide 'init-scala)
;;; init-scala.el ends here

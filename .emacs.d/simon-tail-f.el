;; Minimal setup to have uniform layout and indentation for source files

;; In the same directory as this file resides we expect
;; two subdirectories: yang and erlang. These will be added to the load path
(setq tailf-modes "~/git/trunk/devel_support/lib/emacs/")
;;(add-to-list 'load-path (concat tailf-modes "erlang"))
(add-to-list 'load-path (concat tailf-modes "relaxng"))
(add-to-list 'load-path (concat tailf-modes "yang"))
(add-to-list 'load-path (concat tailf-modes "lux"))

(setq my-root (file-name-directory load-file-name))
(add-to-list 'load-path (concat my-root "clispec"))
(add-to-list 'load-path (concat my-root "tailfconf"))


;; Never ever use TAB character in code
(setq-default indent-tabs-mode nil)
;;---------------

;; C, C++, Java, Javascript mode: indent 4
(add-hook 'c-mode-hook
          '(lambda () (if (commandp 'c-set-style) (c-set-style "user"))))
;;---------------

;; nXML mode: indent 2
(require 'nxml-mode)
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.nc$" . nxml-mode))

(require 'rng-valid)
(add-to-list 'rng-schema-locating-files (concat my-root "relaxng/schemas.xml"))
;;---------------

;; yang mode: indent 2
;;
(autoload 'yang-mode "yang-mode" "Major mode for editing YANG modules." t)
(add-to-list 'auto-mode-alist '("\\.yang$" . yang-mode))
;;---------------

;;erlang mode: indent 4
(autoload 'erlang-mode "erlang" "Major mode for editing Erlang code." t)
(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl$" . erlang-mode))
;;---------------

(autoload 'lux-mode "lux-mode" "Major mode for editing Lux scripts." t)
(add-to-list 'auto-mode-alist '("\\.lux$" . lux-mode))
(add-to-list 'auto-mode-alist '("\\.luxinc$" . lux-mode))

(autoload 'clispec-mode "clispec-mode" "Major mode for editing clispecs." t)
(add-to-list 'auto-mode-alist '("\\.cli$" . clispec-mode))

(autoload 'tailfconf-mode "tailfconf-mode" "Major mode for editing confd/ncs.conf." t)
(add-to-list 'auto-mode-alist '("\\.conf$" . tailfconf-mode))

(when (executable-find "aspell")

  ;; Since nxml-mode, org-mode etc are derived from text-mode, they all run
  ;; this hook.
  (add-hook 'text-mode-hook #'(lambda()
                                (flyspell-mode)
                                (auto-fill-mode)
                                (ispell-change-dictionary "english")))

  (add-to-list 'auto-mode-alist '("\\.txt\\|README\\'" . text-mode))

  ;; Turn on flyspell for comments and strings in prog-modes.
  ;; This should cover all standard prog modes
  (let ((prog-hook (lambda()
                     (flyspell-prog-mode)
                     (auto-fill-mode)
                     (ispell-change-dictionary "english"))))
    (add-hook 'prog-mode-hook prog-hook)

    ;; FIXME. Perhaps make these modes derive from prog-mode so we don't need this??
    (mapc (lambda (mode-hook) (add-hook mode-hook prog-hook))
          '(yang-mode-hook
            ;lux-mode   skip this - no hook
            erlang-mode-hook))))

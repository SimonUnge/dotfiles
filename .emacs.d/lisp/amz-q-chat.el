;;; amz-q-chat.el --- Integration with the q chat CLI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Swapneil Singh
;;
;; Author: Swapneil Singh <swapneis@amazon.com>
;; Maintainer: Swapneil Singh <swapneis@amazon.com>
;; Created: March 31, 2025
;; Modified: March 31, 2025
;; Version: 0.0.1
;; Homepage: https://github.com/swapneis/amz-q-chat
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The behavior of 'amz-q-chat-run' & 'amz-q-chat-toggle' can be customized to
;; create multiple q-chat processes by using the 'amz-q-chat-process-name-generators'
;; hook - see its documentation for more details, or the provided generators
;; prefixed by `amz-q-chat-standard-process-name-generator`.
;;
;;; Code:

(require 'cl-lib)
(require 'term)

(defgroup amz-q-chat nil
  "Customization variables for Amazon Q Chat integration."
  :group 'emacs)

(defcustom amz-q-chat-cli-default-session-args '("kiro-cli" "chat")
  "Command and arguments to run to start the Q chat session in a terminal."
  :group 'amz-q-chat
  :type '(repeat string))

;; Q chats happen in /sessions/, which represent Q chat processes
;; running outside of Emacs.  Each Q chat process has its own context,
;; including access to underlying system resources.
;;
;; Depending on your circumstances, you might prefer different behaviors:
;;
;; - You want a single chat session, not tied to any specfic activity.
;; - You want multiple chat sessions, one per open workspace.
;;
;; We use the "term" library, which expects each interactive terminal
;; session to have a meaningful name.  It creates a buffer based on that
;; name, making the name part of UX navigation; it also tags the
;; underlying Emacs process with the name.

(defvar amz-q-chat-cli-default-session-name "Q chat"
  "Base name for q chat sessions.

We use this as-is for the default case where you want a single chat
session.")

(cl-defstruct (amz-q-chat-session
               (:constructor amz-q-chat-session/make)
               (:copier amz-q-chat-session/copy))
  (name :type string)
  (command-words :type (list string)))

(defcustom amz-q-chat-session-generators nil
  "Hook-with-args used to create q-chat sessions.

Any generator function in this list must accept 2 args:

1. The calling buffer
2. A string, containing a suggested session name
2. A list of strings, containing a suggested session command-line words.

It should return an amz-q-chat-session value with the following:

1. The real session name.
2. The command-line words.

If a generator declines to handle a case, then it should return nil."
  :group 'amz-q-chat
  :type '(repeat function))

(defun amz-q-chat-session-generator-default (buf default-name default-args)
  "Return a generic q-chat session definition for the given BUF.

DEFAULT-NAME is the suggested base name for the session.  DEFAULT-ARGS
are a \"normal\" command-line chat invocation."
  (amz-q-chat-session/make :name default-name
                           :command-words default-args))

(add-hook 'amz-q-chat-session-generators 'amz-q-chat-session-generator-default 0)

;; Here's an example of a session name generator.  It allocates a
;; session name that varies based on the current project name.
(defun amz-q-chat-session-generator-project (buf default-name default-args)
  "Return a project-scoped q-chat process name for the given BUF.

DEFAULT-NAME is the suggested base name for the session.  DEFAULT-ARGS
are a \"normal\" command-line chat invocation."
  (with-current-buffer buf
    (when (and (fboundp 'project-current)
               (fboundp 'project-name)
               (project-current))
      (amz-q-chat-session/make :name (format "[%s] %s" (project-name (project-current)) default-name)
                               :command-words default-args))))

(defun amz-q-chat-session-from-buffer (buf)
  "Return the expected Q chat session for an given buffer."
  (let ((session (run-hook-with-args-until-success 'amz-q-chat-session-generators
                                                   buf
                                                   amz-q-chat-cli-default-session-name
                                                   amz-q-chat-cli-default-session-args)))
    (unless session
      (error "no session picked for buffer %s" buf))

    session))

(defun amz-q-chat-session-from-context ()
  "Return the expected Q chat session for an interactive call."
  (amz-q-chat-session-from-buffer (current-buffer)))

(defun amz-q-chat-confirm-session-name (session)
  ;; Right now this is less "confirm" than "notify", but we can extend
  ;; the behavior if we need to.
  (message "Using Q chat session name \"%s\"" (amz-q-chat-session-name session))
  session)

(defcustom amz-q-chat-popup-side 'bottom
  "Determines which side the popup buffer for amz-q-chat is created
in `amz-q-chat-popup-function'.
By default, this value is passed to `display-buffer-in-side-window'."
  :group 'amz-q-chat
  :type 'symbol)

(defun amz-q-chat-popup-buffer (buf-or-name)
  "Default behavior for popping up the `amz-q-chat' buffer."
  (let ((existing-popup-window (window-with-parameter 'window-side amz-q-chat-popup-side nil t))
        (buffer (get-buffer buf-or-name)))
    (when (and
           ;; There is a window already in the specified slot
           existing-popup-window
           ;; The window is not the same as the one we want
           (not (equal buffer (window-buffer existing-popup-window))))
      (delete-window existing-popup-window))
    ;; Display `buf-or-name' on the specified side
    (let ((popup-window (display-buffer-in-side-window buffer
                                                       `((side . ,amz-q-chat-popup-side)
                                                         (dedicated . t)))))
      (if popup-window
          (select-window popup-window t)
        (warn "Could not display `amz-q-chat' buffer!")))))

(defcustom amz-q-chat-popup-function 'amz-q-chat-popup-buffer
  "Function that is called to open and switch to the `amz-q-chat' buffer in `amz-q-chat-toggle'.
Takes a single input, `buf-or-name', which specifies the buffer to show.

If set to a non-default value, this function may not respect `amz-q-chat-popup-side'."
  :group 'amz-q-chat
  :type 'function)

(defun amz-q-chat-session-buffer (session)
  "Return the buffer of the q-chat associated with a given SESSION."
  (let ((q-proc (get-process (amz-q-chat-session-name session))))
    (when (and q-proc (process-live-p q-proc))
      (process-buffer q-proc))))

(defun amz-q-chat-buffer-p (buf)
  "Return true if the given buffer BUF is a `amz-q-chat' buffer."
  (and-let* ((proc (and (bufferp buf)
                        (get-buffer-process buf))))
    (process-get proc 'amz-q-chat-session)))

(defun amz-q-chat-input-sender (proc string)
  (term-send-string proc string)
  ;; Q Chat's Readline-equivalent uses the CR octet as the "input is
  ;; ready" indicator.  A plain LF without the CR tells the reader to
  ;; keep accumulating more text.  For an interactive terminal in raw
  ;; mode, this means Enter and Control+j, respectively.
  ;;
  ;; In line mode, `term' accumulates text if you hit `C-j', and sends
  ;; the text if you hit `RET'. So we just need to tweak what we send
  ;; for the chat session.
  (term-send-string proc "\r")
  (term-send-string proc "\n"))

(defun amz-q-chat-emulate-terminal (proc str)
  ;; The `term-emulate-terminal' mechanism uses narrowing
  ;; (`save-restriction') to isolate incoming-from-process text from any
  ;; text that the user has been putting in.  It *looks* like this
  ;; text-hiding prevents us from responding normally to all of the
  ;; terminal codes that Chat spews out.  In practice, this means stray
  ;; spaces+newlines accumulate at the end of the buffer after the
  ;; prompt.  Clean those up manually.  I've noticed that the same
  ;; prompt ("What is 3+5?") sticks different whitespace into the
  ;; buffer on successive runs, which leads me to blame Chat.

  (term-emulate-terminal proc str)

  (let ((m (process-mark proc))
        (p (point-max)))
    (when (< m p)
      (save-excursion
        (goto-char m)
        (when (looking-at-p "[\n ]+\\'")
          (delete-region m p))))))

(define-derived-mode amz-q-chat-mode term-mode "Q Chat"
  "Major mode for interacting with Q chat.

This major mode runs on top of `term-mode'.  See the `term-mode'
documentation for its features.

The Q chat integration runs under term's line mode by default, rather
than character mode.  This allows us to get most of the normal Emacs
interactive experience, while still conforming to the chat's
command-line editor expectations.

This chat interface supports multi-line input.  Edit the buffer using
your preferred method, and insert linefeed characters to separate
lines.  (Anything that calls `newline' under the hood should work, or
use \\`C-q C-j' to insert the linefeed directly.)  When done, use \\<term-mode-map>\\[term-send-input]
to send your entire text block to the conversation."
  :group 'amz-q-chat
  :interactive nil

  ;; Line mode has a better user experience.
  ;;
  ;; The underlying chat process expects character mode from terminals,
  ;; so it can respond to Readline-style interactive editing, but Emacs
  ;; handles that editing.  And with line mode, we don't have to worry
  ;; about losing our standard key bindings (unlike character mode).
  ;;
  ;; We have one sticking point: The chat's readline library can see the
  ;; difference betweeen a "VT100" terminal's raw ENTER (CRLF) and
  ;; Control+J (LF), and leverages awareness that to distinguish between
  ;; end-of-input and add-a-new-line.  But term's line mode only sends
  ;; LF by default.  We can compensate for that in
  ;; `amz-q-chat-input-sender' to make things work.
  (term-line-mode)
  (setq term-input-sender 'amz-q-chat-input-sender)
  (setq term-prompt-regexp "^> *")
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start term-prompt-regexp)
  (setq-local font-lock-defaults '(nil t)))

(defun amz-q-chat-allocate-term (chat-session)
  (let ((q-buffer (apply 'make-term
                         (amz-q-chat-session-name chat-session)
                         (car (amz-q-chat-session-command-words chat-session))
                         nil
                         (cdr (amz-q-chat-session-command-words chat-session)))))
    (with-current-buffer q-buffer
      (let ((proc (get-buffer-process (current-buffer))))
        (set-process-filter proc #'amz-q-chat-emulate-terminal)
        ;; This sets up a "magic" property that distinguishes chat
        ;; sessions from other processes.  We don't use the session object
        ;; today, but it might prove useful in the future.
        (process-put proc 'amz-q-chat-session chat-session)))
    q-buffer))

(defun amz-q-chat-stop (chat-session)
  "Locate CHAT-SESSION and terminate its process."
  (interactive (list (amz-q-chat-confirm-session-name
                      (amz-q-chat-session-from-context))))
  (let* ((q-buffer (amz-q-chat-session-buffer chat-session))
         (q-buffer-active (term-check-proc q-buffer)))
    (delete-process (get-buffer-process q-buffer))))

(defun amz-q-chat-start (chat-session)
  "Start a chat process for CHAT-SESSION, in `amz-q-chat-mode'.

Return the chat buffer.  If a process for the session already exists,
then we return its buffer immediately."
  (interactive (list (amz-q-chat-confirm-session-name
                      (amz-q-chat-session-from-context))))
  (let* ((q-buffer (amz-q-chat-session-buffer chat-session))
         (q-buffer-active (term-check-proc q-buffer)))
    ;; Create a new process if we don't have one already
    (unless q-buffer-active
      (setq q-buffer (amz-q-chat-allocate-term chat-session))
      ;; Switch to `q-chat-mode'
      (with-current-buffer q-buffer
        (amz-q-chat-mode)))
    q-buffer))

(defun amz-q-chat-restart (chat-session)
  "Stop any running CHAT-SESSION process and start a new one.

Return the new chat buffer."
  (interactive (list (amz-q-chat-confirm-session-name
                      (amz-q-chat-session-from-context))))
  (amz-q-chat-stop chat-session)
  (amz-q-chat-start chat-session))

;;;###autoload
(defun amz-q-chat-run (chat-session &optional restart)
  "Run a Q CHAT-SESSION, and pop its buffer.

When used interactively, the command will pick a session based on the
current buffer.  (See `amazon-q-chat-session-generators'.)  Use a prefix
key to force Emacs to RESTART any existing process for the session."
  (interactive (list (amz-q-chat-confirm-session-name
                      (amz-q-chat-session-from-context))
                     current-prefix-arg))
  (let ((q-buffer (if restart
                      (amz-q-chat-restart chat-session)
                    (amz-q-chat-start chat-session))))
    (funcall amz-q-chat-popup-function q-buffer)))

;;;###autoload
(defun amz-q-chat-toggle (chat-session &optional restart)
  "Run a Q CHAT-SESSION, and toggle its buffer.

This chat buffer interface attempts to synchronize the underlying chat
process with the ongoing Emacs directory context.  (Contrast this to the
alternate approach of running multiple child sessions with different
contexts.)

When used interactively, the command will pick a session based on the
current buffer.  (See `amazon-q-chat-session-generators'.)  Use a prefix
key to force Emacs to RESTART any existing process for the session."
  (interactive (list (amz-q-chat-confirm-session-name
                      (amz-q-chat-session-from-context))
                     current-prefix-arg))
  (let ((q-buffer (amz-q-chat-session-buffer chat-session)))
    (if (equal (current-buffer) q-buffer)
        (delete-window)
      (let ((target-dir (expand-file-name default-directory))
            (old-buffer (current-buffer)))
        (save-window-excursion
          (setq q-buffer (if restart
                             (amz-q-chat-restart chat-session)
                           (amz-q-chat-start chat-session)))
          (switch-to-buffer q-buffer t)
          ;; Change directory of Amazon Q buffer
          (unless (or
                   ;; Not a local file
                   (ignore-errors
                     (let ((inhibit-message t))
                       (tramp-file-name-host (tramp-dissect-file-name target-dir))))
                   ;; Already in the directory of the original buffer
                   (equal (expand-file-name default-directory) target-dir))
            (end-of-buffer)
            (beginning-of-line)
            ;; Change directories and then restore the current prompt contents
            (let ((line-contents (buffer-substring (point) (line-end-position)))
                  (initial-pos (point)))
              (delete-region (point) (line-end-position))
              (insert (format "!cd %s && pwd" target-dir))
              (term-send-input)
              (insert line-contents))
            (setq-local default-directory (expand-file-name target-dir))))
        (funcall amz-q-chat-popup-function q-buffer)))))

;; Work around bugs in the underlying "term" library.  They exist in v30.1.
;;
;; We don't know when upstream will have fixes for these, so we'll set
;; an arbitrarily large fix version.  We can correct it later.
(when (string-version-lessp emacs-version "999.999.999")
  ;; I use <https://www.xfree86.org/current/ctlseqs.html> as references for these.

  (define-advice term-erase-in-line (:around (orig kind) kind-2)
    ;; The term-erase-in-line function ("Erase in Line") doesn't check
    ;; for case 2 ("Erase All").  Implement it here as "Erase to Right"
    ;; followed by "Erase to Left".  Upstream bug report #nnnnnn.
    (funcall orig 0)
    (funcall orig 1))

  (define-advice term-handle-ansi-escape (:around (orig proc params char) correct-hpa)
    (cond
     ((eq char ?G)
      ;; "Cursor Character Absolute" uses one-based indexing, but the
      ;; `term-handle-ansi-escape' function uses zero-based indexing
      ;; incorrectly.  (It corrects for similar deltas in the Cursor
      ;; Position and Set Scrolling Region handlers, but not in this
      ;; one.)  Upstream bug report #nnnnnn.
      (setf (car params) (1- (car params)))))
    (funcall orig proc params char)))

(provide 'amz-q-chat)

;;; amz-q-chat.el ends here

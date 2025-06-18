;;; q-chat.el --- Amazon Q integration for Emacs -*- lexical-binding: t -*-
;; Author: Simon Unge
;; Version: 2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience

;;; Commentary:
;; This package provides integration with Amazon Q chat from Emacs.
;; Uses amz-q-chat as the underlying engine for better terminal handling
;; and session management.
;;
;; Amazon Q is an AI assistant from AWS that can help with coding tasks,
;; answer questions, and provide guidance on various programming topics.
;; This package allows you to interact with Amazon Q directly from your
;; Emacs environment without switching contexts.
;;
;; Features:
;; - Start interactive Amazon Q sessions within Emacs
;; - Send entire buffers, regions, or specific functions to Amazon Q
;; - Ask questions about your code
;; - Get code reviews for git diffs
;; - Save conversations for future reference
;;
;; Usage:
;; Enable the minor mode with M-x amazon-q-mode, then use these keybindings:
;; - C-c q i - Start an interactive Amazon Q session
;; - C-c q q - Ask a question about selected code or current buffer
;; - C-c q p - Send the current file path to Amazon Q
;; - C-c q r - Send the selected region to Amazon Q
;; - C-c q s - Send the entire buffer to Amazon Q
;; - C-c q d - Send git diff for review
;; - C-c q h - Save the conversation
;; - C-c q f - Ask about the function at point
;;
;; Note: This package requires the Amazon Q CLI to be installed and configured
;; on your system. Run 'q chat' in a terminal to verify it works before using
;; this package.

;;; Code:

(require 'amz-q-chat)

(defun amazon-q-interactive ()
  "Start an interactive Amazon Q session using amz-q-chat engine."
  (interactive)
  (let ((session (amz-q-chat-session-from-context)))
    (amz-q-chat-run session)))

(defun amazon-q--get-buffer ()
  "Get the Q chat buffer, creating session if needed."
  (let ((session (amz-q-chat-session-from-context)))
    (or (amz-q-chat-session-buffer session)
        (amz-q-chat-start session))))

(defun amazon-q--send-content (content &optional prefix)
  "Send CONTENT to Q chat buffer with optional PREFIX message."
  (let ((q-buffer (amazon-q--get-buffer)))
    (with-current-buffer q-buffer
      (goto-char (point-max))
      (when prefix
        (insert prefix)
        (term-send-input))
      (insert content)
      (term-send-input)
      (funcall amz-q-chat-popup-function q-buffer))))

(defun amazon-q-send-buffer ()
  "Send the current buffer content to Amazon Q chat session."
  (interactive)
  (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
        (buffer-name (buffer-name)))
    (amazon-q--send-content
     buffer-content
     (format "Here's my code from buffer %s:" buffer-name))))

(defun amazon-q-send-file-path ()
  "Send the file path of the current buffer to Amazon Q chat session."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if file-path
        (amazon-q--send-content
         (format "Read this file and wait for further instructions: %s" file-path))
      (message "Buffer is not visiting a file"))))

(defun amazon-q-send-region ()
  "Send the selected region to Amazon Q chat session."
  (interactive)
  (if (use-region-p)
      (let ((region-content (buffer-substring-no-properties (region-beginning) (region-end))))
        (amazon-q--send-content
         region-content
         "Here's the code I want to discuss:"))
    (message "No region selected")))

(defun amazon-q-ask-question (question)
  "Ask Amazon Q a specific question about the current buffer or region."
  (interactive "sQuestion for Amazon Q: ")
  (let ((content (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (amazon-q--send-content
     (format "%s\n\n%s" content question)
     "Please review this code and answer my question:")))

(defun amazon-q-review-diff ()
  "Send git diff to Amazon Q for code review suggestions."
  (interactive)
  (let ((diff-output (shell-command-to-string "git diff")))
    (if (string-empty-p (string-trim diff-output))
        (message "No changes to review")
      (amazon-q--send-content
       diff-output
       "Please review this git diff and suggest improvements:"))))

(defun amazon-q-save-conversation ()
  "Save the current Amazon Q conversation to a file."
  (interactive)
  (let ((file (read-file-name "Save conversation to: "))
        (q-buffer (amazon-q--get-buffer)))
    (when q-buffer
      (with-current-buffer q-buffer
        (write-region (point-min) (point-max) file)
        (message "Conversation saved to %s" file)))))

(defun amazon-q-ask-about-current-function (question)
  "Ask Amazon Q a specific question about the current function."
  (interactive "sQuestion about this function: ")
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (if bounds
        (let ((function-content (buffer-substring-no-properties
                                 (car bounds) (cdr bounds))))
          (amazon-q--send-content
           (format "%s\n\n%s" function-content question)
           "Please review this function and answer my question:"))
      (message "No function definition found at point"))))

;; Define the minor mode for Amazon Q integration
(define-minor-mode amazon-q-mode
  "Minor mode for Amazon Q integration."
  :lighter " Q"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c q i") 'amazon-q-interactive)
            (define-key map (kbd "C-c q q") 'amazon-q-ask-question)
            (define-key map (kbd "C-c q p") 'amazon-q-send-file-path)
            (define-key map (kbd "C-c q r") 'amazon-q-send-region)
            (define-key map (kbd "C-c q s") 'amazon-q-send-buffer)
            (define-key map (kbd "C-c q d") 'amazon-q-review-diff)
            (define-key map (kbd "C-c q h") 'amazon-q-save-conversation)
            (define-key map (kbd "C-c q f") 'amazon-q-ask-about-current-function)
            map))

(define-globalized-minor-mode global-amazon-q-mode
  amazon-q-mode
  (lambda () (amazon-q-mode 1)))

(provide 'q-chat)
;;; q-chat.el ends here

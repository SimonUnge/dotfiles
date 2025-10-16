;;; q-chat.el --- Amazon Q integration for Emacs -*- lexical-binding: t -*-
;; Author: Simon Unge
;; Version: 2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;;; Commentary:
;; This package provides integration with Amazon Q chat from Emacs.
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
;; - C-c q q - Ask a question about selected code or current buffer (via temp file)
;; - C-c q Q - Ask a question about selected code or current buffer (direct)
;; - C-c q p - Send the current file path to Amazon Q
;; - C-c q r - Send the selected region to Amazon Q (via temp file)
;; - C-c q R - Send the selected region to Amazon Q (direct)
;; - C-c q s - Send the entire buffer to Amazon Q
;; - C-c q d - Send git diff for review (via temp file, async)
;; - C-c q D - Send git diff for review (direct)
;; - C-c q h - Save the conversation
;; - C-c q f - Ask about the function at point (via temp file)
;; - C-c q F - Ask about the function at point (direct)
;; - C-c q t - Toggle Amazon Q chat buffer visibility
;;
;; Note: This package requires the Amazon Q CLI to be installed and configured
;; on your system. Run 'q chat' in a terminal to verify it works before using
;; this package.
;;; Code:

(require 'amz-q-chat)

(defvar amazon-q--temp-files nil
  "List of temporary files created by Amazon Q functions.")

(defun amazon-q-interactive ()
  "Start an interactive Amazon Q session using amz-q-chat.
Does nothing if a session is already running."
  (interactive)
  (let ((session (amz-q-chat-session-from-context)))
    (if (amz-q-chat-session-buffer session)
        (message "Amazon Q session already exists")
      (amz-q-chat-run session))))

(defun amazon-q--send-to-chat (message)
  "Send MESSAGE to the Amazon Q chat session."
  (let* ((session (amz-q-chat-session-from-context))
         (q-buffer (amz-q-chat-session-buffer session)))
    (if q-buffer
        (with-current-buffer q-buffer
          (goto-char (point-max))
          (insert message)
          (term-send-input)
          (pop-to-buffer q-buffer))
      (message "No Amazon Q session found. Please run amazon-q-interactive first."))))

(defun amazon-q-send-buffer ()
  "Send the current buffer content directly to Amazon Q chat session."
  (interactive)
  (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
        (buffer-name (buffer-name)))
    (amazon-q--send-to-chat 
     (format "Here's my code from buffer %s:\n\n%s" buffer-name buffer-content))))

(defun amazon-q-send-file-path ()
  "Send the file path of the current buffer to Amazon Q chat session."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if file-path
        (amazon-q--send-to-chat 
         (format "Read this file and wait for further instructions: %s" file-path))
      (message "Buffer is not visiting a file"))))

(defun amazon-q-send-region ()
  "Send the selected region to a temp file and instruct Amazon Q to read it."
  (interactive)
  (if (use-region-p)
      (let ((region-content (buffer-substring-no-properties (region-beginning) (region-end)))
            (temp-file (make-temp-file "amazon-q-content-" nil ".txt")))
        ;; Track the temp file
        (push temp-file amazon-q--temp-files)
        ;; Write region content to temp file
        (with-temp-file temp-file
          (insert region-content))
        ;; Send instruction to read the file
        (amazon-q--send-to-chat 
         (format "Read this file in silence and await follow up question: %s" temp-file)))
    (message "No region selected")))

(defun amazon-q-send-region-direct ()
  "Send the selected region directly to Amazon Q without using temp files."
  (interactive)
  (if (use-region-p)
      (let ((region-content (buffer-substring-no-properties (region-beginning) (region-end))))
        (amazon-q--send-to-chat 
         (format "Here's the selected code:\n\n%s\n\nPlease analyze this code and wait for my question." region-content)))
    (message "No region selected")))

(defun amazon-q-ask-question (question)
  "Ask Amazon Q a specific question about the current buffer or region using a temp file."
  (interactive "sQuestion for Amazon Q: ")
  (let ((content (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max))))
        (temp-file (make-temp-file "amazon-q-content-" nil ".txt")))
    ;; Track the temp file
    (push temp-file amazon-q--temp-files)
    ;; Write content to temp file
    (with-temp-file temp-file
      (insert content))
    ;; Send combined instruction
    (amazon-q--send-to-chat 
     (format "Read this file: %s and answer this question: %s" temp-file question))))

(defun amazon-q-ask-question-direct (question)
  "Ask Amazon Q a specific question about the current buffer or region directly."
  (interactive "sQuestion for Amazon Q: ")
  (let ((content (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (amazon-q--send-to-chat 
     (format "Here's my code:\n\n%s\n\nQuestion: %s" content question))))

(defun amazon-q-review-diff ()
  "Send git diff to Amazon Q for code review suggestions asynchronously."
  (interactive)
  (let ((temp-file (make-temp-file "amazon-q-diff-" nil ".txt")))
    ;; Track the temp file
    (push temp-file amazon-q--temp-files)
    ;; Show a message that we're starting
    (message "Running git diff asynchronously...")
    ;; Run git diff asynchronously
    (make-process
     :name "amazon-q-git-diff"
     :buffer nil
     :command (list "git" "diff")
     :connection-type 'pipe
     :filter (lambda (proc string)
               (with-temp-file temp-file
                 (insert string)))
     :sentinel (lambda (proc event)
                 (when (string= (string-trim event) "finished")
                   (if (= 0 (file-attribute-size (file-attributes temp-file)))
                       (message "No changes to review")
                     (progn
                       (message "Git diff completed, sending to Amazon Q...")
                       (amazon-q--send-to-chat 
                        (format "Review this git diff and suggest improvements: %s" temp-file)))))))))

(defun amazon-q-review-diff-direct ()
  "Send git diff directly to Amazon Q for code review suggestions."
  (interactive)
  (message "Running git diff...")
  (let ((diff-output (shell-command-to-string "git diff")))
    (if (string-empty-p (string-trim diff-output))
        (message "No changes to review")
      (amazon-q--send-to-chat 
       (format "Review this git diff and suggest improvements:\n\n%s" diff-output)))))

(defun amazon-q-save-conversation ()
  "Save the current Amazon Q conversation to a file."
  (interactive)
  (let ((file (read-file-name "Save conversation to: "))
        (session (amz-q-chat-session-from-context)))
    (let ((q-buffer (amz-q-chat-session-buffer session)))
      (when q-buffer
        (with-current-buffer q-buffer
          (write-region (point-min) (point-max) file))))))

(defun amazon-q-cleanup-temp-files ()
  "Delete temporary files created by Amazon Q functions."
  (interactive)
  (dolist (file amazon-q--temp-files)
    (when (file-exists-p file)
      (delete-file file)))
  (setq amazon-q--temp-files nil)
  (message "Amazon Q temporary files cleaned up"))

(defun amazon-q-ask-about-current-function (question)
  "Ask Amazon Q a specific question about the current function using temp file."
  (interactive "sQuestion about this function: ")
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (if bounds
        (let ((function-content (buffer-substring-no-properties
                                 (car bounds) (cdr bounds)))
              (temp-file (make-temp-file "amazon-q-content-" nil ".txt")))
          ;; Track the temp file
          (push temp-file amazon-q--temp-files)
          ;; Write content to temp file
          (with-temp-file temp-file
            (insert function-content))
          ;; Send combined instruction
          (amazon-q--send-to-chat 
           (format "Read this file: %s and answer this question: %s" temp-file question)))
      (message "No function definition found at point"))))

(defun amazon-q-ask-about-current-function-direct (question)
  "Ask Amazon Q a specific question about the current function directly."
  (interactive "sQuestion about this function: ")
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (if bounds
        (let ((function-content (buffer-substring-no-properties
                                 (car bounds) (cdr bounds))))
          (amazon-q--send-to-chat 
           (format "Here's the function:\n\n%s\n\nQuestion: %s" function-content question)))
      (message "No function definition found at point"))))

(defun amazon-q-toggle ()
  "Toggle the Amazon Q chat buffer visibility."
  (interactive)
  (let ((session (amz-q-chat-session-from-context)))
    (amz-q-chat-toggle session)))

;; Define the minor mode for Amazon Q integration
(define-minor-mode amazon-q-mode
  "Minor mode for Amazon Q integration."
  :lighter " Q"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c q i") 'amazon-q-interactive)
            (define-key map (kbd "C-c q q") 'amazon-q-ask-question)
            (define-key map (kbd "C-c q Q") 'amazon-q-ask-question-direct)
            (define-key map (kbd "C-c q p") 'amazon-q-send-file-path)
            (define-key map (kbd "C-c q r") 'amazon-q-send-region)
            (define-key map (kbd "C-c q R") 'amazon-q-send-region-direct)
            (define-key map (kbd "C-c q s") 'amazon-q-send-buffer)
            (define-key map (kbd "C-c q d") 'amazon-q-review-diff)
            (define-key map (kbd "C-c q D") 'amazon-q-review-diff-direct)
            (define-key map (kbd "C-c q h") 'amazon-q-save-conversation)
            (define-key map (kbd "C-c q f") 'amazon-q-ask-about-current-function)
            (define-key map (kbd "C-c q F") 'amazon-q-ask-about-current-function-direct)
            (define-key map (kbd "C-c q t") 'amazon-q-toggle)
            map))

(define-globalized-minor-mode global-amazon-q-mode
  amazon-q-mode
  (lambda () (amazon-q-mode 1)))

;; Add hook to clean up temp files when Emacs exits
(add-hook 'kill-emacs-hook 'amazon-q-cleanup-temp-files)

(provide 'q-chat)
;;; q-chat.el ends here

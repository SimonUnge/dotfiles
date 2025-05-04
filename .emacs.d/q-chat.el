;;; q-chat.el --- Amazon Q integration for Emacs -*- lexical-binding: t -*-
;; Author: Simon Unge
;; Version: 1.0
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

(defvar amazon-q--temp-files nil
  "List of temporary files created by Amazon Q functions.")

(defun amazon-q-interactive ()
  "Start an interactive Amazon Q session in a shell buffer.
Does nothing if a session is already running."
  (interactive)
  (if (get-buffer "*Amazon Q Interactive*")
      (message "Amazon Q session already exists")
    (let ((shell-buffer (shell "*Amazon Q Interactive*")))
      (with-current-buffer shell-buffer
        (goto-char (point-max))
        (insert "q chat")
        (comint-send-input)))))

(defun amazon-q-send-buffer ()
  "Send the current buffer content to an existing Amazon Q chat session."
  (interactive)
  (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
        (q-buffer "*Amazon Q Interactive*"))
    ;; Check if Amazon Q session exists
    (if (get-buffer q-buffer)
        (with-current-buffer q-buffer
          ;; Go to the end of the buffer
          (goto-char (point-max))
          ;; Insert a message indicating what we're sending
          (insert (format "Here's my code from buffer %s:" (buffer-name (current-buffer))))
          (comint-send-input)
          ;; Insert the buffer content
          (insert buffer-content)
          (comint-send-input)
          ;; Switch to the Q buffer
          (pop-to-buffer q-buffer))
      ;; If no session exists, show warning
      (message "No Amazon Q session found. Please run amazon-q-interactive first."))))

(defun amazon-q-send-file-path ()
  "Send the file path of the current buffer to an existing Amazon Q chat session."
  (interactive)
  (let ((file-path (buffer-file-name))
        (q-buffer "*Amazon Q Interactive*"))
    ;; Check if the buffer is visiting a file
    (if file-path
        (if (get-buffer q-buffer)
            (with-current-buffer q-buffer
              ;; Go to the end of the buffer
              (goto-char (point-max))
              ;; Insert the file path
              (insert (format "Read this file and wait for further instructions: %s" file-path))
              (comint-send-input)
              ;; Switch to the Q buffer
              (pop-to-buffer q-buffer))
          ;; If no session exists, just show warning
          (message "No Amazon Q session found. Please run amazon-q-interactive first."))
      (message "Buffer is not visiting a file"))))

(defun amazon-q-send-region ()
  "Send the selected region to a temp file and instruct Amazon Q to read it."
  (interactive)
  (if (use-region-p)
      (let ((q-buffer "*Amazon Q Interactive*")
            (region-content (buffer-substring-no-properties (region-beginning) (region-end)))
            (temp-file (make-temp-file "amazon-q-content-" nil ".txt")))
        ;; Track the temp file
        (push temp-file amazon-q--temp-files)
        ;; Write region content to temp file
        (with-temp-file temp-file
          (insert region-content))
        ;; Check if Amazon Q session exists
        (if (get-buffer q-buffer)
            (with-current-buffer q-buffer
              ;; Go to the end of the buffer
              (goto-char (point-max))
              ;; Insert the instruction to read the file
              (insert (format "Read this file in silence and await follow up question: %s" temp-file))
              (comint-send-input)
              ;; Switch to the Q buffer
              (pop-to-buffer q-buffer))
          ;; If no session exists, just show warning
          (message "No Amazon Q session found. Please run amazon-q-interactive first.")))
    (message "No region selected")))

(defun amazon-q-ask-question (question)
  "Ask Amazon Q a specific question about the current buffer or region using a temp file."
  (interactive "sQuestion for Amazon Q: ")
  (let ((q-buffer "*Amazon Q Interactive*")
        (content (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max))))
        (temp-file (make-temp-file "amazon-q-content-" nil ".txt")))
    ;; Track the temp file
    (push temp-file amazon-q--temp-files)
    ;; Write content to temp file
    (with-temp-file temp-file
      (insert content))
    ;; Check if Q session exists
    (if (get-buffer q-buffer)
        (with-current-buffer q-buffer
          ;; Go to the end of the buffer
          (goto-char (point-max))
          ;; Send a combined instruction to read the file and answer the question
          (insert (format "Read this file: %s and answer this question: %s" temp-file question))
          (comint-send-input)
          ;; Switch to the Q buffer
          (pop-to-buffer q-buffer))
      ;; If no session exists, just show warning
      (message "No Amazon Q session found. Please run amazon-q-interactive first."))))

(defun amazon-q-review-diff ()
  "Send git diff to Amazon Q for code review suggestions asynchronously."
  (interactive)
  (let ((temp-file (make-temp-file "amazon-q-diff-" nil ".txt"))
        (q-buffer "*Amazon Q Interactive*"))
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
                       (if (get-buffer q-buffer)
                           (with-current-buffer q-buffer
                             (goto-char (point-max))
                             (insert (format "Review this git diff and suggest improvements: %s" temp-file))
                             (comint-send-input)
                             (pop-to-buffer q-buffer))
                         (message "No Amazon Q session found. Please run amazon-q-interactive first.")))))))))

(defun amazon-q-save-conversation ()
  "Save the current Amazon Q conversation to a file."
  (interactive)
  (let ((file (read-file-name "Save conversation to: "))
        (q-buffer "*Amazon Q Interactive*"))
    (when (get-buffer q-buffer)
      (with-current-buffer q-buffer
        (write-region (point-min) (point-max) file)))))

(defun amazon-q-cleanup-temp-files ()
  "Delete temporary files created by Amazon Q functions."
  (interactive)
  (dolist (file amazon-q--temp-files)
    (when (file-exists-p file)
      (delete-file file)))
  (setq amazon-q--temp-files nil)
  (message "Amazon Q temporary files cleaned up"))

(defun amazon-q-ask-about-current-function (question)
  "Ask Amazon Q a specific question about the current function."
  (interactive "sQuestion about this function: ")
  (let ((q-buffer "*Amazon Q Interactive*")
        (bounds (bounds-of-thing-at-point 'defun))
        (temp-file nil))
    (if bounds
        (let ((function-content (buffer-substring-no-properties
                                 (car bounds) (cdr bounds))))
          ;; Create temp file and write function content
          (setq temp-file (make-temp-file "amazon-q-content-" nil ".txt"))
          ;; Track the temp file
          (push temp-file amazon-q--temp-files)
          ;; Write content to temp file
          (with-temp-file temp-file
            (insert function-content))
          ;; Check if Q session exists
          (if (get-buffer q-buffer)
              (with-current-buffer q-buffer
                ;; Go to the end of the buffer
                (goto-char (point-max))
                ;; Send a combined instruction to read the file and answer the question
                (insert (format "Read this file: %s and answer this question: %s"
                                temp-file question))
                (comint-send-input)
                ;; Switch to the Q buffer
                (pop-to-buffer q-buffer))
            ;; If no session exists, show warning
            (message "No Amazon Q session found. Please run amazon-q-interactive first.")))
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

;; Add hook to clean up temp files when Emacs exits
(add-hook 'kill-emacs-hook 'amazon-q-cleanup-temp-files)

(provide 'q-chat)
;;; q-chat.el ends here

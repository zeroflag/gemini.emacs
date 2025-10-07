;;; -*- lexical-binding: t; -*-
;;; gemini.el --- Gemini chatbot
;;;
;;; Author: Attila Magyar
;;; URL: http://github.com/zeroflag/gemini.emacs
;;; Version: 0.12
;;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;;; Gemini chatbot for Emacs
;;;
;;; Code:
;;;
(require 'request)
(require 'json)

(defcustom gemini-api-key-file
  (expand-file-name "~/.gemini.emacs/.api-key.txt")
  "Path to Gemini API key file"
  :type 'file
  :group 'gemini)

(defcustom gemini-model "gemini-flash-latest"
  "The LLM model used by Gemini chatbot"
  :type 'string
  :group 'gemini)

(defcustom gemini-log-file "/tmp/gemini.el.log"
  "Log file path used by Gemini chatbot"
  :type 'file
  :group 'gemini)

(defcustom gemini-default-system-message (concat
                                          "You are a helpful assistant running inside Emacs. "
                                          "Use the following context to answer the given question. "
                                          "Be brief and use lines shorter than 80 characters")
  "System instructions for Gemini chabot"
  :type 'string
  :group 'gemini)

(when (not (file-exists-p gemini-api-key-file))
  (error (format "File %s does not exist" gemini-api-key-file)))

(defvar gemini-api-key
  (string-trim
   (with-temp-buffer
     (insert-file-contents gemini-api-key-file)
     (buffer-string))))

(defvar gemini-debug nil)
(defvar gemini-chat-history nil)
(defvar gemini-api-base-url
  "https://generativelanguage.googleapis.com/v1beta")

(defun gemini-gen-url ()
  (let ((url (concat gemini-api-base-url "/models/%s:generateContent?key=%s")))
    (format url gemini-model gemini-api-key)))

(defun gemini-models-url ()
  (let ((url (concat gemini-api-base-url "/models?key=%s")))
    (format url gemini-api-key)))

(defun gemini-strip-response (s)
  (replace-regexp-in-string "```\\w*" "" (string-trim s)))

(defun gemini-api-request (prompt system-message callback)
  "Send the PROMPT and SYSTEM-MESSAGE to Gemini API.

   Then call CALLBACK with the response text."
  (let ((payload (json-encode
                  `(("contents" . [ (("parts" . [ (("text" . ,prompt)) ]) ) ])
                    ("systemInstruction" .
                     (("parts" . [ (("text" . ,system-message)) ])))))))
    (request
      (gemini-gen-url)
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data payload
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((candidates (alist-get 'candidates data))
                         (first (aref candidates 0))
                         (content (alist-get 'content first))
                         (parts (alist-get 'parts content))
                         (text (alist-get 'text (aref parts 0))))
                    (funcall callback (gemini-strip-response text)))))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Gemini API error: %S" error-thrown))))))

(defun gemini-model-name (item)
  (string-remove-prefix "models/" (alist-get 'name item)))

(defun gemini-show-models ()
  "Fetch available gemini models"
  (interactive)
  (request
    (gemini-models-url)
      :type "GET"
      :headers '(("Content-Type" . "application/json"))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((models (alist-get 'models data))
                         (names  (mapconcat #'gemini-model-name models "\n")))
                  (display-message-or-buffer names))))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Gemini API error: %S" error-thrown)))))

(defun gemini-build-context () (buffer-string))

(defun gemini-append-history (s)
  (push s gemini-chat-history))

(defun gemini-add-history (question answer)
  (gemini-append-history (format " - User: %s" question))
  (gemini-append-history (format " - AI: %s" answer))
  (gemini-append-history ""))

(defun gemini-build-history ()
  (string-join (reverse gemini-chat-history) "\n"))

(defun gemini-build-prompt (context question)
  (format (concat "***CONTEXT***\n\n%s\n\n"
                  "***CHAT HISTORY***\n\n%s\n"
                  "QUESTION: %s")
          context (gemini-build-history) question))

(defun gemini-read-question ()
  (string-trim (thing-at-point 'line t)))

(defun gemini-forget ()
  "Clear gemini chatbot history."
  (interactive)
  (setq gemini-chat-history nil))

(defun gemini-send-question (question &optional system-message)
  "Ask the QUESTION from Gemini using the optional SYSTEM-MESSAGE"
  (interactive)
  (let* ((sys-msg (or system-message gemini-default-system-message))
         (prompt (gemini-build-prompt (gemini-build-context) question)))
    (when gemini-debug
      (write-region prompt nil gemini-log-file))
    (gemini-api-request
     prompt
     sys-msg
     (lambda (response)
       (gemini-add-history question response)
       (move-end-of-line nil)
       (insert "\n")
       (insert response)))))

(defun gemini-send-line ()
  "Send buffer content to Gemini AI."
  (interactive)
  (gemini-send-question (gemini-read-question)))

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; gemini.el ends here

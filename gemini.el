;;; gemini.el --- Gemini chatbot
;;; -*- lexical-binding: t; -*-
;;;
;;; Author: Attila Magyar
;;; URL: http://github.com/zeroflag/gemini.emacs
;;; Version: 0.12
;;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;;; Gemini chatbot for Emacs
;;;
;;; Code:
(require 'request)
(require 'json)

(defvar gemini-script-dir
  (file-name-directory load-file-name))

(defvar gemini-api-key-file
  (expand-file-name ".api-key.txt" gemini-script-dir))

(when (not (file-exists-p gemini-api-key-file))
  (error (format "File %s does not exist" gemini-api-key-file)))

(defvar gemini-api-key
  (string-trim
   (with-temp-buffer
     (insert-file-contents gemini-api-key-file)
     (buffer-string))))

(defvar gemini-debug nil)
(defvar gemini-modell "gemini-2.0-flash")
(defvar gemini-chat-history nil)
(defvar gemini-api-url
  "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s")

(defun gemini-strip-response (s)
  (replace-regexp-in-string "```\\w*" "" (string-trim s)))

(defun gemini-send (prompt system-message callback)
  "Send the PROMPT and SYSTEM-MESSAGE to Gemini API.

   Then call CALLBACK with the response text."
  (let ((url (format gemini-api-url gemini-modell gemini-api-key))
        (payload (json-encode
                  `(("contents" . [ (("parts" . [ (("text" . ,prompt)) ]) ) ])
                    ("systemInstruction" .
                     (("parts" . [ (("text" . ,system-message)) ])))))))
    (request
      url
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

(defun gemini-send-line (system-message)
  (let* ((question (gemini-read-question))
         (prompt   (gemini-build-prompt (gemini-build-context) (thing-at-point 'line t))))
    (when gemini-debug
      (write-region prompt nil "/tmp/gemini.el.log"))
    (gemini-send
     prompt
     system-message
     (lambda (response)
       (gemini-add-history question response)
       (move-end-of-line nil)
       (insert "\n")
       (insert response)))))

(defun gemini-interactive-send ()
  "Send buffer content to Gemini AI."
  (interactive)
  (gemini-send-line
   (concat
    "You are a helpful assistant running inside Emacs. "
    "Use the following context to answer the given question. "
    "Be brief and use lines shorter than 80 characters")))

(provide 'gemini)

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; gemini.el ends here

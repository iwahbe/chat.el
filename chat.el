;;; chat.el --- An Emacs facade for ChatGPT  -*- lexical-binding:t -*-

(require 'json)
(require 'url)
(require 'url-http)

(defgroup chat ()
  "Chatting with OpenAI models, such as GPT3."
  :group 'external)

(defcustom chat-api-env-key "OPENAI_API_KEY"
  "The environmental variable used to look up an API key if not specified."
  :type 'string :group 'chat)

(defcustom chat-api-key nil
  "The API Key used to query api.openapi.com."
  :type 'string :group 'chat)

(defcustom chat-max-tokens 500
  "The maximum number of tokens for ChatGPT to return for a single request.
A nil value translates to no upper bound in the number of tokens."
  :type 'number :group 'chat)

(defcustom chat-system-prompt "You are a helpful AI Assistant embedded into Emacs."
  "The system prompt given to all ChatGPT requests."
  :type 'string :group 'chat)

(defvar chat--input-history nil
  "The variable that stores input history.")

(defun chat-get-api-key ()
  "Get API key for OpenAI.
If no key is found, error."
  (or chat-api-key
      (and chat-api-env-key (getenv chat-api-env-key))
      (user-error
       "Could not get OpenAI API key. Either set `chat-api-key' or ensure that \"%s\" is set"
       chat-api-env-key)))

(defun chat--async-raw (input callback)
  "Query ChatGPT with INPUT.
CALLBACK is called in the receiving buffer with POINT at the
beginning of the new data."
  (let* ((url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,(concat "Bearer " (chat-get-api-key)))))
         (url-request-data (json-encode
                            `(("model" . "gpt-3.5-turbo")
                              ,@(when chat-max-tokens
                                  `(("max_tokens" . ,chat-max-tokens)))
                              ("stream" . t)
                              ("messages" .
                               ((("role" . "system")
                                 ("content" . ,chat-system-prompt))
                                (("role" . "user")
                                 ("content" . ,input)))))))
         (filter (lambda (filter proc data)
                   (let ((length (funcall filter proc data))
                         (b (process-buffer proc)))
                     (when (buffer-live-p b)
                       (with-current-buffer b
                         (goto-char (- (point-max) (length data)))
                         (funcall callback))
                       length)))))
    (advice-add #'url-http-generic-filter :around filter)
    (url-retrieve "https://api.openai.com/v1/chat/completions"
                  (lambda (status)
                    (advice-remove #'url-http-generic-filter filter)
                    (when-let (err (plist-get status :error))
                      (error "Failed to get response: %s" err))))))

(defun chat--async-query (input callback)
  "Apply CALLBACK to each data block returned from calling ChatGPT on INPUT."
  (let ((b (current-buffer)))
    (chat--async-raw
     input
     (lambda ()
       (when (search-forward "data: " nil t)
         (let ((chunk (json-parse-buffer
                       :array-type 'list
                       :object-type 'alist)))
           (with-current-buffer b
             (funcall callback chunk))))))))

(defun chat--async-text (input callback)
  "Make a call to ChatGPT on INPUT and call CALLBACK on the resulting text chunks."
  (chat--async-query
   input
   (lambda (x)
     (funcall callback
              (alist-get 'content
                         (alist-get 'delta
                                    (car (alist-get 'choices x))))))))

(defun chat-query-insert (input)
  "Insert ChatGPTs response to INPUT."
  (interactive "sChatGPT Input: ")
  (let ((p (point-marker)))
    (chat--async-text input
                      (lambda (chunk)
                        (save-excursion
                          (goto-char (marker-position p))
                          (switch-to-buffer (marker-buffer p))
                          (insert-before-markers-and-inherit chunk))))))

(provide 'chat)
;;; chat.el ends here
